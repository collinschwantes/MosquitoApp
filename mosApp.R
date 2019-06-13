## UI
library(shiny)
library(shiny)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(readr)
library(shinythemes)

ui <- fluidPage(
  theme = shinytheme("darkly"),
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
      selectInput("TrapType", "Trap Type:",
                  c("Oviposition" = "ovi",
                    "Light" = "lig",
                    "CO2" = "CO2  ")),
      selectInput("Count", "Count Data:",
                  c("No data uploaded data" = "NoData")
                  ),
      selectInput("Date", "Date:",
                  c("No data uploaded data" = "NoData")
                  ),
      tableOutput("data")
      ),
      fluidRow(
        #textOutput("dataSummary")
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data Summary",
                 plotOutput("MosPopPlot"),
                 dataTableOutput("contents")),
        tabPanel("Model Outputs")
      )
    )
  )
)

server <- function(input, output, session) {
  
  dataFile <- reactive({
    infile <- input$file1
    
    if (is.null(infile)) 
      return(NULL)
    
    read.csv(infile$datapath, header = input$header)
  })
  
  ## update selection inputs
  observe({
    
    x <- dataFile()
    
    # Can use character(0) to remove all choices
    if (is.null(x))
      x <- character(0)
    
    # Can also set the label and select items
    
    ColNames <- names(x)
    
    updateSelectInput(session, "Date",
                      label = "Date Column",
                      choices = ColNames,
                      selected = tail(x, 1)
    )
    
    updateSelectInput(session, "Count",
                      label = "Count Column",
                      choices = ColNames,
                      selected = tail(x, 1)
    )
    
  })
  
  
   output$contents <- renderDataTable({ 
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
     
     dataFile()
  })
   
   output$MosPopPlot <- renderPlot(bg = "transparent",{
     
     if (is.null(dataFile())) 
       return(NULL)
     
       ggplot(data = dataFile(),aes_string(x = dataFile()$dateCollected, 
                                           y = dataFile()$count)) +
       geom_point(color = "white") +
         theme_minimal() +
         theme(panel.grid  =  element_line(colour = "dark grey"))
   
    })
   
   output$dataSummary <- renderPrint({
     
     if (is.null(dataFile())) 
       return(NULL)
     
     str(dataFile())
     
     ## add GBIF backbone
     ## show historical trends?
       
     
   })
   
  # output$ModelResults 
   
}

shinyApp(ui, server)
