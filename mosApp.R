## UI
library(shiny)


ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      ),
      tags$hr(),
      checkboxInput("header", "Header", TRUE)
    ),
    mainPanel(
      dataTableOutput("contents")
    )
  )
)

server <- function(input, output) {
  
  dataFile <- reactive({
    infile <- input$file1
    
    if (is.null(infile)) 
      return(NULL)
    
    read.csv(infile$datapath, header = input$header)
  })
  
  
   output$contents <- renderDataTable({ 
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
     
     dataFile()
  })
}

shinyApp(ui, server)
