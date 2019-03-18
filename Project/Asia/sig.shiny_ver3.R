library(shiny)
library(forecast)
library(dplyr)
library(ggplot2)
library(openxlsx)
library(zoo)
# Define UI for data upload app ----
ui <- fluidPage( tabsetPanel(
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
    # Output: contents ----
    tableOutput("contents")
  ),
  tabPanel("Team5 Forecast",
           # Output: downloadData ----
           downloadButton("downloadData1", "Download")
  ),
  
  tabPanel("Team2 Forecast",
           selectInput("class", "Class",
                       choices = c(class1 = "1",
                                   class2 = "2",
                                   class3 = "3",
                                   class4 = "4"),
                       selected = "Thailand"),
           downloadButton("downloadData2", "Download")
  )
))



# Define server logic to read selected file ----
server <- function(input, output,session) {
  # renderTable: contents ----
  
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    ts_data<-function(df){
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
    bestF_sheet<<-matrix(c('customer','model name','RMSE',as.character(1:12)))
    bestE_sheet<<-matrix(c('customer','model name','RMSE',as.character(1:12)))

    #######model########
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
      package.ma=rollmean(p,k=2,align='right')
      packageMAF<<-package.ma
      packageMAE<<-p-packageMAF
      validMAF<<-tail(package.ma,12)
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
    #######trycatch########
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       row.names = 1)
        ts_data(df)
        # plotcompare(38)
        # take the first column of the dataset
        x <- df[1]
        
        updateSelectInput(session, "customerproductpaired",
                          choices = x,
                          selected = x
        )
        for (i in 1:85){
          nam <- paste(nams[i,])
          #define object
          package.ts <-ts((SIG_data[,i]), start = c(2009,1), freq =12)
          nvalid=12
          ntrain=length(package.ts)-nvalid
          train.ts <- window(package.ts, start=c(2009,1),end=c(2009,ntrain))
          valid.ts <- window(package.ts, start=c(2009,ntrain+1),end = c(2009,ntrain+nvalid))
          #seasonnaive model 1
          snaive_model(train.ts,valid.ts)
          #ets model 2
          ets_model(train.ts,valid.ts)
          #arima model 3 
          arima_model(train.ts,valid.ts)
          ##ma model 
          ma_model(package.ts,valid.ts)
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
        
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
  })
}


# Create Shiny app ----
shinyApp(ui, server)

