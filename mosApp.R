## UI
library(shiny)
library(dplyr)
library(ggplot2)
library(tidyverse)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      fluidRow(
      fileInput("file1", "Choose CSV File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      ),
      checkboxInput("header", "Header", TRUE),
      tags$hr(),
      selectInput("Trap Type", "Options:",
                  c("Oviposition" = "ovi",
                    "Light" = "lig",
                    "CO2" = "CO2  ")),
      tableOutput("data")
      ),
      fluidRow(
        textOutput("dataSummary")
      )
    ),
    mainPanel(
      plotOutput("MosPopPlot"),
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
   
   output$MosPopPlot <- renderPlot({
     
     if (is.null(dataFile())) 
       return(NULL)
     
       ggplot(data = dataFile(),aes_string(x = dataFile()$dateCollected, 
                                           y = dataFile()$count)) +
       geom_point()
   
    })
   
   output$dataSummary <- renderPrint({
     
     if (is.null(dataFile())) 
       return(NULL)
     
     str(dataFile())
       
     
   })
   
}

shinyApp(ui, server)
