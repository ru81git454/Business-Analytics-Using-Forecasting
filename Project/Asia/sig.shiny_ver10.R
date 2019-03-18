library(shiny)
library(forecast)
library(dplyr)
library(ggplot2)
library(openxlsx)
library(zoo)
# Define UI for data upload app ----
ui <- fluidPage( tabsetPanel(
  #tabpanel1 ----
  tabPanel(
    " Upload file",
    # Input: Select a file ----
    fileInput("file1", "Choose CSV File",
              multiple = FALSE,
              accept = c(".csv")),
    # Input: header ----
    checkboxInput("header", "File contain Header?", TRUE),
    
    # Input: disp ----
    selectInput("disp", "Display",
                choices = c(Head = "head",
                            All = "all"),
                selected = "head"),
    
    # Input: country----
    selectInput("country", "Country",
                choices = c(Thailand = "Thailand",
                            Indonesia = "Indonesia"),
                selected = "Thailand"),
    # Input: customerproductpaired ----
    selectInput("customerproductpaired", "銷售編號",
                choices = c(Thailand = "Thailand",
                            Indonesia = "Indonesia")),
    # Horizontal line ----
    tags$hr(),
    plotOutput("actualplot"),
    # Output: contents ----
    tableOutput("contents")
    
  ),
  #tabpanel2----
  tabPanel("The data can predict",
           # Output: QuicKModel ----
           tableOutput("pred_actual")
           
  ),
  #tabpanel3 ----
  tabPanel("QuicKModel",
           # Output: downloadData ----
           downloadButton("downloadData2", "Download"),
           # Input: customerproductpaired ----
           selectInput("customerproductpaired1", "銷售編號",
                       choices = c(Thailand = "Thailand",
                                   Indonesia = "Indonesia")),
           plotOutput("MAplot"),
           # Output: QuicKModel ----
           tableOutput("QuicKModel")
           
  ),
  #tabpanel4 ----
  tabPanel("Team5 Forecast",
           # Output: Team5Forecast ----
           tableOutput("Team5Forecast"),
           # Output: downloadData ----
           downloadButton("downloadData1", "Download")
  )
))




# Define server logic to read selected file ----
server <- function(input, output,session) {
  # model----
  #seasonnaive model 1
  snaive_model<-function(t,v){
    Snaive.fore <- snaive(t, h=12)
    validSnaiveF <<- Snaive.fore$mean
    validSnaiveE <<- v - Snaive.fore$mean
    trainSnaiveF <<- Snaive.fore$fitted
    trainSnaiveE <<- Snaive.fore$residuals
  }
  #ets model 2
  ets_model<-function(t,v){
    hwin <- ets(t)
    hwin.fore  <-forecast(hwin,h=12)
    validhwinF <<-hwin.fore$mean
    validhwinE <<-v-hwin.fore$mean
    trainhwinF <<- hwin.fore$fitted
    trainhwinE <<- hwin.fore$residuals
  }
  #arima model 3 
  arima_model<-function(t,v){
    arimaM<-auto.arima(t)
    arima.fore<-forecast(arimaM,h=12)
    validarimaF<<-arima.fore$mean
    validarimaE<<-v-validarimaF
    trainarimaF<<- arima.fore$fitted
    trainarimaE<<- arima.fore$residuals
  }
  ## ma model 5
  ma_model<-function(p,v){
    package.ma=rollmean(p,k=12,align='right')
    packageMAF<<-package.ma
    packageMAE<<-p-packageMAF
    validMAF<<-tail(package.ma,12)
    validMAF<<-ts(validMAF,start = c(2018,1),frequency = 12)
    ForecastMAF<<-ts(validMAF,start = c(2019,1),frequency = 12)
    validMAE<<-v-validMAF
  }
  ## tslm model 5
  lm_model<-function(t,v){
    lmM<-tslm(t~trend+season)
    lm.fore<-forecast(lmM,h=12)
    validlmF<<-lm.fore$mean
    validlmE<<-v-validlmF
    trainlmF <<- lm.fore$fitted
    trainlmE <<- lm.fore$residuals
  }
  #tabpanel1----
  # renderPlot: actualplot ----
  output$actualplot <- renderPlot({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    req(input$customerproductpaired)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    select_data<<-function(df,cs_pair){
      selected_data<<-df[cs_pair,] %>%
        select(-Country) %>%
        t(.)
    }
    #######trycatch########
    tryCatch(
      {
        df1 <<- read.csv(input$file1$datapath,
                         header = input$header,
                         row.names = 1)
        select_data(df1,input$customerproductpaired)
        # plotcompare(38)
        # take the first column of the dataset
        selected_data
        package.ts <-ts(selected_data, start = c(2009,1), freq =12)
        actual_plot<-autoplot(package.ts, series='Data',alpha=0.2) +
          ggtitle(paste("Actual value",input$customerproductpaired)) +
          xlab("Time") + ylab("Sales") +
          guides(colour=guide_legend(title="series")) +
          scale_color_manual(values=c(Data="black"))
        return(actual_plot)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
  })
  # renderTable: contents ----
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    ts_data<<-function(df){
      SIG_data<<-df %>%
        filter(Country=='Thailand')%>%
        select(-Country) %>%
        t(.)
      nams<<- df%>%
        add_rownames('ID')%>%
        filter(Country=='Thailand')%>%
        select(ID) 
      data_w_nams<<-rbind(t(nams),SIG_data)
    }
    #######trycatch########
    tryCatch(
      {
        df1 <<- read.csv(input$file1$datapath,
                         header = input$header,
                         row.names = 1)
        df2 <<- read.csv(input$file1$datapath,
                         header = input$header)
        ts_data(df1)
        # plotcompare(38)
        # take the first column of the dataset
        x <- nams
        
        updateSelectInput(session, "customerproductpaired",
                          choices = x,
                          selected = x
        )
        updateSelectInput(session, "customerproductpaired1",
                          choices = x,
                          selected = x
        )
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    if(input$disp == "head") {
      return(head(df2))
    }
    else {
      return(df2)
    }
    
  },colnames = F)
  #tabpanel2----
  # renderTable: pred_actual ----
  output$pred_actual <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    ts_data_lynn<-function(df){
      SIG_data<<-df %>%
        filter(Country=='Thailand')%>%
        select(-Country) %>%
        t(.) 
      nams<<- df%>%
        add_rownames('ID')%>%
        filter(Country=='Thailand')%>%
        select(ID) 
      data_w_nams<<-rbind(t(nams),SIG_data)
      sig_actual<<-as.data.frame(SIG_data)
      colnames(sig_actual) <<- nams$ID
    }
    #######trycatch########
    tryCatch(
      {
        df1 <<- read.csv(input$file1$datapath,
                         header = input$header,
                         row.names = 1)
        df2 <<- read.csv(input$file1$datapath,
                         header = input$header)
        ts_data_lynn(df1)
        ###########NA###############
        put_NA = function(ts){
          for(i in 1:length(ts)){
            if(ts[i] == 0){
              ts[i]=NA}
            else{
              break}
          }
          return(ts)
        }
        
        sig_actual <- as.data.frame(sapply(sig_actual,put_NA))
        
        
        # Those without 2 years full data.
        drop_data = function(x){
          time_len <- length(x[,1])
          count_na = function(x){
            time_len - sum(is.na(x))
          }
          
          series_len = sapply(x,count_na)
          sig_cannot_pred <<- x[,which(series_len < 24)]
          sig_above_2years <- x[,which(series_len >= 24)]
          
          # Those last 2 years are all zero
          last_2years_zero = function(x){
            sum(x[(time_len-24+1) : time_len],na.rm = T)
          }
          
          series_drop <<- sapply(sig_above_2years,last_2years_zero)
          sig_pred_actual <<- sig_above_2years[,which(series_drop != 0)]
          sig_drop <<- sig_above_2years[,which(series_drop == 0)]
        }
        drop_data(sig_actual)
        a<-c('sig_pred_actual',colnames(sig_pred_actual))
        return(a)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
  },colnames = F)
  #tabpanel3----
  # renderTable: QuicKModel ----
  output$QuicKModel <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    
    #######trycatch########
    tryCatch(
      {
        df1 <<- read.csv(input$file1$datapath,
                         header = input$header,
                         row.names = 1)
        ts_data(df1)
        MAF_sheet<<-matrix(c('customer','RMSE',as.character(1:12)))
        MARMSEs<-0
        for (i in 1:85){
          nam <- paste(nams[i,])
          #define object
          package.ts <<-ts((SIG_data[,i]), start = c(2009,1), freq =12)
          nvalid=12
          ntrain=length(package.ts)-nvalid
          train.ts <<- window(package.ts, start=c(2009,1),end=c(2009,ntrain))
          valid.ts <<- window(package.ts, start=c(2009,ntrain+1),end = c(2009,ntrain+nvalid))
          ##ma model
          ma_model(package.ts,valid.ts)
          ##model select_accuracy
          MAAccuracy<-accuracy(valid.ts,validMAF)[2]
          MAF<-c(nam,MAAccuracy,ForecastMAF)
          MAF_sheet<<-cbind(MAF_sheet,as.data.frame(MAF))
        }
        
        return(t(MAF_sheet))
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
  },colnames = FALSE)
  # renderPlot: MAPLOot ----
  output$MAplot <- renderPlot({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    req(input$customerproductpaired)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    nam<-input$customerproductpaired
    select_data<<-function(df,cs_pair){
      selected_data<<-df[cs_pair,] %>%
        select(-Country) %>%
        t(.)
    }
    #######trycatch########
    tryCatch(
      {
        df1 <<- read.csv(input$file1$datapath,
                         header = input$header,
                         row.names = 1)
        select_data(df1,input$customerproductpaired1)
        package.ts <-ts(selected_data, start = c(2009,1), freq =12)
        # plotcompare(38)
        # take the first column of the dataset
        #define object
        nvalid=12
        ntrain=length(package.ts)-nvalid
        train.ts <<- window(package.ts, start=c(2009,1),end=c(2009,ntrain))
        valid.ts <<- window(package.ts, start=c(2009,ntrain+1),end = c(2009,ntrain+nvalid))
        ##ma model
        ma_model(package.ts,valid.ts)
        actual_plot<-autoplot(package.ts, series='Data',alpha=0.2) +
          forecast::autolayer(ForecastMAF,series='QuickForecast',linetype='dashed')+
          forecast::autolayer(packageMAF,series='QuickForecast')+
          ggtitle(paste("Forecast value",nam)) +
          xlab("Time") + ylab("Sales") +
          guides(colour=guide_legend(title="series")) +
          scale_color_manual(values=c(Data="black",QuickForecast='Green'))
        return(actual_plot)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
  })
  # downloadHandlet: downloadData2 ----
  output$downloadData2 <- downloadHandler(
    filename='Quick_model.xlsx',
    content = function(file) {
      MA_model_datasets<-list('data'=data_w_nams,'Quicksheet'=t(MAF_sheet))
      write.xlsx(MA_model_datasets, file)
    }
  )
  
  #tabpanel4----
  # renderTable: Team5Forecast ----
  output$Team5Forecast <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    
    #######trycatch########
    tryCatch(
      {
        df1 <<- read.csv(input$file1$datapath,
                         header = input$header,
                         row.names = 1)
        ts_data(df1)
        bestF_sheet<<-matrix(c('customer','model name','RMSE',as.character(1:12)))
        
        for (i in 1:length(sig_pred_actual)){
          nam <- paste(nams[i,])
          #define object
          package.ts <-ts(SIG_data[,i], start = c(2009,1), freq =12)
          nvalid=12
          ntrain=length(package.ts)-nvalid
          valid.ts <- subset(package.ts, start = length(package.ts) - 12+1)
          train.ts <- subset(package.ts, end = length(package.ts) - length(valid.ts))
          #seasonnaive model 1
          snaive_model(train.ts,valid.ts)
          #ets model 2
          ets_model(train.ts,valid.ts)
          #arima model 3 
          arima_model(train.ts,valid.ts)
          ##ma model 
          ma_model(train.ts,valid.ts)
          ## tslm model 5
          lm_model(train.ts,valid.ts)
          
          ##model select_accuracy
          modelsF<-list(validSnaiveF,validhwinF,validarimaF,validMAF,validlmF)
          modelsE<-list(validSnaiveE,validhwinE,validarimaE,validMAE,validlmE)
          modelsNames<-c('Snaive','hwin','arima','MA','lm')
          accuracylists<-list(accuracy(valid.ts,validSnaiveF)[2],
                              accuracy(valid.ts,validhwinF)[2],
                              accuracy(valid.ts,validarimaF)[2],
                              accuracy(valid.ts,validMAF)[2],
                              accuracy(valid.ts,validlmF)[2] )
          modelname<-modelsNames[which.min(accuracylists)]
          bestmodelF<-unlist(modelsF[which.min(accuracylists)])
          bestmodelE<-unlist(modelsF[which.min(accuracylists)])
          bestAccuracy<-unlist(accuracylists[which.min(accuracylists)])
          bestF<-c(nam,modelname,bestAccuracy,bestmodelF)
          bestE<-c(nam,modelname,bestAccuracy,bestmodelE)
          bestF_sheet=cbind(bestF_sheet,as.data.frame(bestF))
          bestE_sheet=cbind(bestE_sheet,as.data.frame(bestE))
        }
        
        return(t(bestF_sheet))
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
  },colnames = FALSE)
  
  # downloadHandlet: downloadData1 ----
  output$downloadData1 <- downloadHandler(
    filename='best_model.xlsx',
    content = function(file) {
      best_model_datasets<-list('data'=data_w_nams,'bestF_sheet'=t(bestF_sheet))
      write.xlsx(best_model_datasets, file)
    }
  )
  
}




# Create Shiny app ----
shinyApp(ui, server)
