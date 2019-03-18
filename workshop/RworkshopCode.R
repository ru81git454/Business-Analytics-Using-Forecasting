library(tidyverse)
library(lubridate)
#library(data.table) wday() conflicts with wday() in lubridate, FYI
library(ggridges)
library(gghighlight)

#ORGANIZE YOUR CODE PROJECTS IN 'PROJECTS'
#Click on little cube in top right and select a new folder for a new project.
#now all your files will be in there and you dont' have to always set a new working directory
#keep all your saved images, markdown files, and R data files for each access

#PIPES: %>% make your code much more readable than having super nested functions
#Ex: filter(my_df, mpg > 50) is the same as 
#my_df %>%
#filter(mpg > 50) 

#KEY VERBS IN TIDYVERSE:
#filter() Dates > as.Date('2007-01-01') 
#group_by()
#arrange() desc(var)
#select() and select_if select_at useful with contains('xyz') or matches('xyz')
#summarize()
#mutate & mutate_at(c('gender', 'age') & mutate_if mutate_all
#count() use with fct_lump and renaming
#slice(1:10) good for top 10s or top n per group

#TidyR: Help keep data in 'tidy' format for easier analysis

#gather() make data long for visualization. columns will be variable types so you can facet by that column
#spread() opposite of gather. useful for when you want to create distance metrics
#unite() good for latitutde and longitude
#separate() useful for separating dates and times
#glimpse() better than summary()


#ggplot2: Data visualization

#facet_wrap(~ var) For each factor level, plot the data
#geom_tile() Used for heatmaps
#stat_ecdf()
#scale_x_date(data_breaks = '1 day', date_labels = '%Y%m') 

#Other useful functions in tidyverse
#fct_lump() Groups categories into top n categories by counts
#ymd()
#floor_date() very useful for selecting different levels of aggregation for Time Series
#str_detect() useful with select to find columns with certain kinds of values
#make.names() for when you import csvs with spaces or unusual chars
#parse_numeric() useful when you have % or $ or 200.000


################################################################
#CREATE SMALLER VERSION OF DATASET JUST FOR 2018 ARRESTS
#################################################################
#df <- fread('la_arrest.csv', stringsAsFactors=FALSE)
#glimpse(df)

#one common problem is bad column names with spaces or special chars. make_names can help R'ize them
#colnames(df) <- make.names(colnames(df))
#glimpse(df)

#if you have big dataset (like 1M+) just run it on like 10 to make sure then rerun
#df <- df %>%
  #mutate(Arrest.Date = mdy(Arrest.Date)) #looks good with 10 rows, so do all obs. just assign back to df

#glimpse(df)
#df %>%
  #arrange(desc(Arrest.Date))

#month of may arrests
#la_arrest_may2018 <- df %>%
#  filter(between(Arrest.Date,as.Date('2018-05-01'), as.Date('2018-05-31')))%>%
#  arrange(Arrest.Date)

#la_arrest_may2018 <- la_arrest_may2018 %>%
#  mutate(Arrest.Date = as.character(Arrest.Date))%>%
#  arrange(Report.ID)

#write.csv(la_arrest_may2018, 'la_may2018.csv', row.names = F)

###########################################################################
#START HERE
library(tidyverse)
###########################################################################
#imagine the LAPD has asked you to forecast arrests in each area. In order to set quotas.
df <- read.csv('la_may2018.csv', stringsAsFactors = FALSE ) #na.strings = c('', 'none')
glimpse(df); summary(df); str(df)
#check to see how many areas. 
length(unique(df$Area.Name)) #21 areas

#if you were intersted in 21 area level models. Let's say this is your goal. For our sake only focus on top 1
df %>%
  count(Area.Name, sort=T) #count func will tell you have many obs associated with the value 
#based on this let's go with Holywood
#

#create just hollywood obs
holly <- df %>%
  filter(Area.Name == 'Hollywood')
glimpse(holly) #can you see the issue we'll have with the times? 


#let's combine time with date, but problem is time format is weird! missing 0s
#ISO FORMAT is YYYY-MM-DD HH:MM:SS
#the hms library can help when you have these columns with just times
library(hms)         
hm(holly$Time) #but this doesn't work!

pm('21:00')
hm('01:35') #the key is that there HAS to be 4 digits in the time. So we need to pad!

#the beauty of dplyr and ggplot and pipes is that you can combine cleaning with visualization easily
#combine into one step. so basic plot of overall arrests in the month.
holly <- holly %>%
  mutate(Time = str_pad(Time, width=4, side='left', pad='0'))%>% #we want 4 digit times, pad with 0s from left
  mutate(Time = hm(paste0(str_sub(Time, 1,2), ':', str_sub(Time, 3,4))))%>% #split left half and right, add : in middle then convert to hm()
  unite('date_time', Arrest.Date, Time, sep = ' ', remove=FALSE)%>% #combine the columns into one datetime
  mutate(date_time = ymd_hms(date_time))%>% #now we have ISO format, use ymd_hms() keep separate for now
  mutate(Arrest.Date = ymd(Arrest.Date))#focus back to original now and conver to year month day
glimpse(holly) #check out classes of Time and date time
  
#first visualization. Counting daily arrests
holly %>%
  count(Arrest.Date)%>% #for each unique arrest date, count how many rows
  #take dates on x axis, then n on y axis. think of aes() as using values from columns in your DF
  ggplot(aes(Arrest.Date, n, group=1))+ #whenever you get error about groups, group=1 will fix 95% of time
  geom_line()+
  scale_x_date(date_breaks = '1 days', date_labels = '%d-%a')+ #%a is abbr weekday, %D is full YMD, %A is fullname, check ?striptime
  theme(axis.text.x = element_text(angle=90)) #useful when you have long labels

Character	Meaning
a	Abbreviated weekday
A	Full weekday
b	Abbreviate month name
B	Full month name
I	Hours (12 hour)
p	AM/PM
z	Timezone, offset in hours and minutes from UTC
#what do we see? remember this dataset is just arrests in hollywood in may 2018

#START TO EXTRACT DOW AND TIME OF DAY. Day of week effect?
#use the wday() function with label = TRUE
#data.table has wday() as well... need to specify namespace
holly %>%
  mutate(dow = lubridate::wday(Arrest.Date, label = TRUE, abbr = TRUE, week_start=1))%>% #create new column with day of week derived from date
  count(dow)%>% #week_start =7 means week starts on Sundays, week_start=1 means mondays
  ggplot(aes(dow, n, group=1))+
  geom_point()+ #we can always layer on more geoms if we think it looks nicer. 
  geom_line()

#What do we find about weekly seasonality? 


#What about daily seasonality? To extract the hour, we simply use hour()
#Remember: before extracting, the variables must be date or datetime objects! won't work on chars
holly %>%
  mutate(time_day = hour(date_time))%>%
  count(time_day)%>%
  ggplot(aes(time_day, n))+
  geom_point()+
  geom_line()

#What do we see? What kind of daily seasonality? But do you think every day of the week has this pattern?

#enter faceting: we plot by distinct levels(category) of a factor variable
#it would be nice to facet by the day of week, right? 
holly %>%
  mutate(dow = lubridate::wday(Arrest.Date, abbr = TRUE, label=TRUE, week_start = 1))%>% #add back in DOW. Def is sunday start  (ISO)
  mutate(time_day = hour(date_time))%>%
  count(dow, time_day)%>% #now we have to count dOW and time for each! double layering
  ggplot(aes(time_day, n, group=1))+
  geom_line()+
  facet_wrap(~dow, ncol=7) #now do this for each day of week. and put in 7 columns

#but maybe it's easier to look all 7 lines on top of each other? 
holly %>%
  mutate(dow = lubridate::wday(Arrest.Date, abbr = TRUE, label=TRUE, week_start = 1))%>% #add back in DOW. Def is sunday start  (ISO)
  mutate(time_day = hour(date_time))%>%
  count(dow, time_day)%>% #now we have to count dOW and time for each! double layering
  ggplot(aes(time_day, n, color=dow))+ #just add our variable dow as a color
  geom_line(size=2, alpha=.5)+
  scale_color_brewer(type='qual') #useful for specially chosen color palettes depending on viz needs
#What did we learn about daily seasonality? 

#But we can imagine the arrest times probably depend on the type of crimes
#but this plotting pattern is now even more powerful because we can group by diff types of crimes
holly %>%
  mutate(dow = lubridate::wday(Arrest.Date, abbr = TRUE, label=TRUE, week_start = 1))%>% #add back in DOW. Def is sunday start  (ISO)
  mutate(time_day = hour(date_time))%>%
  count(charge = fct_lump(Charge.Group.Description,7),dow, time_day)%>% #look at top 7 types of crimes
  filter(!charge %in% c('', 'Other'))%>%#now we have to count dOW and time for each! double layering
  ggplot(aes(time_day, n, color=charge))+
  geom_line(size=2, alpha=.5)+
  gghighlight::gghighlight()+
   #can use gghighlight package to better see
  facet_wrap(charge~dow, ncol=7, scales='free_y') #What can we say about the times and crimes?
#the beauty of this is you just keep modifying your code and rerunning to look at other things
#such as maybe not top 5 crimes, but top 7, 


#let's say you want to look at am and pm crimes. Which happen in am vs pm? 

#finally we will use ggridges to look at distributions by units of time
##is this plot useful? not really. we should have histogram of times for each day of week
holly %>%
  mutate(dow = lubridate::wday(Arrest.Date, abbr = TRUE, label=TRUE, week_start = 1))%>% #add back in DOW. Def is sunday start  (ISO)
  mutate(is_morning = am(date_time))%>% #returns TRUE/FALSE WHETHER IN morning. use for filter!
  na.omit()%>%
  ggplot(aes(Arrest.Date, fill=is_morning))+ #when you use fill with binary, you get rough idea of proportion
    geom_histogram(bins=24, alpha=.5)+
    facet_wrap(~dow) #overall this plot isn't that useful, but shows how you can use am/pm()

#we want to know When do arrests tend to happen for different crimes?

library(ggridges)
holly %>%
  mutate(dow = lubridate::wday(Arrest.Date, abbr = TRUE, label=TRUE, week_start = 1))%>%
  mutate(hr = hour(date_time))%>%
  count(crime = fct_lump(Charge.Group.Description,10), dow, hr)%>%
  filter(!crime %in% c('', 'Other'))%>% #by default fct_lump creates Other and '' is missing value
  ggplot(aes(x = hr, y = dow, height = ..density.., fill=dow)) + #only difference is add height=..density..
  geom_density_ridges(stat = "density", alpha=.5)+
  theme_ridges()+ #is the guide useful here? if not
  #guides(fill=FALSE)+ in many cases guide is extra and not needed. remove
  facet_wrap(~ crime) #or try by area

#any questions at this point before we look at the taxi data? 

####################### taxi cabs
####################### tricky because some dates in same column have diff formats
####################### 
#cabs <- read.csv('cabs.csv')
#use read_csv() for smarter but slower column parsing
cabs <- read_csv('cabs.csv', na = 'NULL') #doesn't auto do stringsAsFactors
glimpse(cabs)
#just looking at glimpse() what is problem? is it in ISO format? no. missing leading 0

#Daily cab bookings
#Your basic aggregation plots over time
cabs %>%
  mutate(created = mdy_hm(created))%>%
  count(num_days = floor_date(created, unit='3 months'))%>% #adjust unit to your liking. try diff values
  ggplot(aes(num_days,n))+ #remember can run code with cursor anywhere in code block and ctrl+enter
  geom_line() #ctrl+shift+P reruns whatever you last ran. good for when you make changes

#durations and periods: 2 ways of looking at intervals of time b/w 2 events
#you can extract the interval in seconds by using int_length()
#now you can arrange it and plot it. 
#it's giving warnings because so many missing datetimes
cabs %>%
  mutate(created = mdy_hm(created))%>%
  mutate(dur = int_length(from_date %--% created))%>%
  mutate(dur_period = as.period(dur))%>% #periods are more 'human time' thinking in days, weeks, months, etc.
  mutate(dur_duration = as.duration(dur))%>% #duration is computer time
  select(dur, dur_period, dur_duration) #check classes

#mutate_if: useful for converting multiple columns
cabs <- cabs %>%
  mutate_if(str_detect(names(.), 'date|created'), funs(mdy_hm(.)))
glimpse(cabs)

#another way using mutate_at: need to use vars() and try a select helper function: starts_with(), matches()
#contains(), pass the function you want to use on the colums inside funs(func to use)
#the . means the object you've just created and passed to it
cabs %>%
  mutate_at(vars(matches('date|created')), funs(mdy_hm(.)))
  

#this shows the problem
table(unique(nchar(cabs$from_date))) # we have dates recorded with different numbers of characters!
#remember ISO should be YY:MM:DD:HH:MM:SS
head(unique(cabs$from_date),20) #you can see missing 0 padding for times like 9:30. should be 09:30


#What if you have multiple date/time formats in a single column? 
cabs$to_date <- parse_date_time(cabs$to_date, orders= c('mdy','mdy_HM', 'mdy_HMS'))
cabs$from_date <- parse_date_time(cabs$from_date, orders= c('mdy','mdy_HM', 'mdy_HMS'))
cabs$created <- parse_date_time(cabs$created, orders= c('mdy','mdy_HM', 'mdy_HMS'))
glimpse(cabs) #automatically added leading 0
#in the future combine with mutate_at()

#Since we have created at times and actual usage time, what about analyzing the time difference?
#create difftime object and set units and plot
#can see people from 393 are booking sometimes longer in adv
cabs %>%
  mutate(t_diff = difftime(from_date,created, units='hours'))%>% #so again we can play with units
  filter(t_diff < 10000)%>%
  ggplot(aes(t_diff))+
  geom_histogram(binwidth = 4)+ #4 hour bins
  coord_cartesian(xlim = c(0,100))+
  #xlim(0,80)+ #this won't include values outside of x lim. perhaps better was is coord_cartesian(xlim=(0,80))
  facet_wrap(~fct_lump(as.factor(from_area_id), 11),scales = 'free_y', ncol=4)
#i suggest using fct_lump() with facet_wrap when you have 10+ categories
#when some categories have much higher counts, you can use scales='free_y' to let scales adapt to counts
#This view lets you better compare shapes of distributions at the expense of counts

#another useful wayt to plot this is ecdf
#Do you know how to interpret it? 
cabs %>%
  mutate(t_diff = difftime(from_date,created, units='hours'))%>%
  filter(t_diff < 10000)%>%
  ggplot(aes(t_diff, color=fct_lump(as.factor(from_area_id),5)))+ #combine with fct_lump to make fewer cats
  stat_ecdf()+
  coord_cartesian(xlim=c(0,100))+
  scale_x_continuous(breaks = seq(0,10000,10))+ #custom breaks on x axis
  theme(axis.text.x = element_text(angle=90))+
  scale_color_discrete(name= 'Starting Locations')
#this shows that 393 customers do seem to be different than the rest. they book more in advance. Also NAs

#Sometimes temporal data can be nicely visualized using heatmaps. Not always line charts!
#time heatmap
#I like this one
cabs %>%
  mutate(m = month(created, label = TRUE, abbr=TRUE))%>% #start by extracting date components
  mutate(y = year(created))%>%
  mutate(w = week(created))%>%
  mutate(d = wday(created, label=TRUE, abbr=TRUE))%>%
  mutate(dnum = day(created))%>%
  count(y,m,dnum)%>% #then count for each year, each month and each day of week
  filter(y > '2011')%>%
  ggplot(aes(m,dnum, fill=n))+ #put month on x axis, and day of week on Y, color by counts
  geom_tile(na.rm=TRUE)+
  scale_fill_gradient(name='# Orders',low='white', high='red', na.value = 'white')+ #diverging color scheme
  theme_minimal()+
  facet_wrap(~y, ncol=1)+
  labs(title='Day of Month Orders by Year and Month')

#what can we see from this? 

#Variation on previous but days of week by month. Which do you prefer?
cabs %>%
  mutate(m = month(created, label = TRUE, abbr=TRUE))%>%
  mutate(y = year(created))%>%
  mutate(w = week(created))%>%
  mutate(d = wday(created, label=TRUE, abbr=TRUE, week_start=1))%>%
  mutate(dnum = day(created))%>%
  count(y,m,d)%>%
  filter(y > '2011')%>% #only keep full years
  ggplot(aes(m,d, fill=n))+
  geom_tile(na.rm=TRUE)+
  scale_fill_gradient(name='# Orders',low='white', high='red', na.value = 'white')+
  theme_minimal()+
  facet_wrap(~y, ncol=1)+
  labs(title='Day of Week Orders by Year and Month')

#Do a more granular one with hour of day by day of week. Not that useful, but shows the power of heatmaps
cabs %>%
  mutate(m = month(created, label = TRUE, abbr=TRUE))%>%
  mutate(y = year(created))%>%
  mutate(w = week(created))%>%
  mutate(d = wday(created, label=TRUE, abbr=TRUE))%>%
  mutate(dnum = day(created))%>%
  mutate(h = hour(created))%>%
  mutate(mi = minute(created))%>%
  count(d,h,mi)%>%
  filter(h != 0)%>%
  ggplot(aes(h,mi, fill=n))+
  geom_tile(na.rm=TRUE)+
  scale_fill_gradient(name='# Orders', low='white', high='red')+
  scale_x_continuous(breaks = seq(0,23,1))+
  theme_minimal()+
  facet_wrap(~d)+
  labs(title='Orders by hour and minute of day by Day of Week') #window between 8-11 most orders


#Conclusion: keep these patterns and practice them. You will finding yourself using them again and again

#count(new_var = fct_lump(var, 5))
#filter(!var %in% c('', 'Other'))

#cabs %>%
  #mutate(created = mdy_hm(created))%>%
  #count(num_days = floor_date(created, unit='2 weeks'))%>% 
  #ggplot(aes(num_days,n))+ 
  #geom_line()

#facet_wrap(~ var)
#facet_wrap(~ fct_lump(var, 5))


#scale_x_date(date_breaks = '1 days', date_labels = '%d-%a')


#geom_line(size=2, alpha=.5)+
#gghighlight::gghighlight()+
#facet_wrap(charge~dow, ncol=7) 


#CHALLENGE!
#The attached csv ('challenge.csv') is a fake dataset I created to test your data cleaning skills.
#Here are some tasks for you:

#1) Convert the four times and dates columns into two columns, one for purchase datetime one for signup datetime

#2) Plot the number of monthly sign ups over time

#3) Plot the number of monthly sign ups over time by gender (color by gender)

#4) Compute and visualize the difference in time between sign up and first purchase by customer type 
#(facet & gghighlight)

#5) Visualize the average first purchase by day of week (mon, tues, wed, etc.) by customer type

#6) Plot the number of daily sign ups only between the dates of 2016-10-01 and 2017-01-01

#7) Visualize the percentage of customers who make their first purchase within 7 days of signing up

#8) Use ggridges to visualize purchase amount by the day of week for the first purchase

