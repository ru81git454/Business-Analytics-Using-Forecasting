library(readxl)
library(reshape2)
library(dplyr)

# read all sheets
y2009_2010 <- read_excel("APS Actual Sales 2009-201811_Sanitizedv1.xlsx",sheet = "2009-2010")
y2011_2012 <- read_excel("APS Actual Sales 2009-201811_Sanitizedv1.xlsx",sheet = "2011-2012")
y2013_2014 <- read_excel("APS Actual Sales 2009-201811_Sanitizedv1.xlsx",sheet = "2013-2014")
y2015_2016 <- read_excel("APS Actual Sales 2009-201811_Sanitizedv1.xlsx",sheet = "2015-2016")
y2017_2018 <- read_excel("APS Actual Sales 2009-201811_Sanitizedv1.xlsx",sheet = "2017-201811")

#change all negative value into zero
y2009_2010$`Plan qty.`[which(y2009_2010$`Plan qty.`< 0)] <- 0
y2011_2012$`Plan qty.`[which(y2011_2012$`Plan qty.`< 0)] <- 0
y2013_2014$`Plan qty.`[which(y2013_2014$`Plan qty.`< 0)] <- 0
y2015_2016$`Plan qty.`[which(y2015_2016$`Plan qty.`< 0)] <- 0
y2017_2018$`Plan qty.`[which(y2017_2018$`Plan qty.`< 0)] <- 0


# creare new aggregate dataframe
filter_y2009_2010 <- aggregate(`Plan qty.`~ Name4 + Customer + `Product hierarchy`+ Month, sum, data = y2009_2010)
filter_y2011_2012 <- aggregate(`Plan qty.`~ Name4 + Customer + `Product hierarchy`+ Month, sum, data = y2011_2012)
filter_y2013_2014 <- aggregate(`Plan qty.`~ Name4 + Customer + `Product hierarchy`+ Month, sum, data = y2013_2014)
filter_y2015_2016 <- aggregate(`Plan qty.`~ Name4 + Customer + `Product hierarchy`+ Month, sum, data = y2015_2016)
filter_y2017_2018 <- aggregate(`Plan qty.`~ Name4 + Customer + `Product hierarchy`+ Month, sum, data = y2017_2018)

# Combine 2009 - 2018 dataframe
sig <- rbind(filter_y2009_2010,filter_y2011_2012,filter_y2013_2014,filter_y2015_2016,filter_y2017_2018)

# Concate customer and product hierarchy
sig$`Link(cust+pro)` <- paste(sig$Customer,sig$`Product hierarchy`)
sig <- sig[-c(2,3)]

# create pivot table of month, package, and demand numbers
sig_pivot <- as.data.frame(tapply(sig$`Plan qty.`,list(sig$`Link(cust+pro)`,sig$Month),sum))
sig_pivot[is.na(sig_pivot)] <- 0

# merge country back with package 
sig_pivot$Country <- sig[match(rownames(sig_pivot),sig$`Link(cust+pro)`),]$Name4

