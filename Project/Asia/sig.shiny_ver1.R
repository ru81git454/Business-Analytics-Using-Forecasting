library(shiny)
# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Uploading Files"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c(".csv")),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: header ----
      checkboxInput("header", "File contain Header?", TRUE),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: disp ----
      selectInput("disp", "Display",
                  choices = c(Head = "head",
                              All = "all"),
                  selected = "head"),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: country----
      selectInput("country", "Country",
                  choices = c(Thailand = "Thailand",
                              Indonesia = "Indonesia"),
                  selected = "Thailand"),
      # Horizontal line ----
      tags$hr(),
      
      # Input: customerproductpaired ----
      selectInput("customerproductpaired", "銷售編號",
                  choices = c(Thailand = "Thailand",
                              Indonesia = "Indonesia"))
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Data file ----
      tableOutput("contents")
      
    )
    
  )
)

# Define server logic to read selected file ----
server <- function(input, output,session) {
  
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
