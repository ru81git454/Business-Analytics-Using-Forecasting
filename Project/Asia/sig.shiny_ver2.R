library(shiny)
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
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header)
        
        # take the first column of the dataset
        x <- df[1]
        
        updateSelectInput(session, "customerproductpaired",
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
      return(head(df))
    }
    else {
      return(df)
    }
    
  })
}


# Create Shiny app ----
shinyApp(ui, server)

