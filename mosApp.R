## UI
library(shiny)
library(shiny)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(readr)
library(shinythemes)
library(sf)
library(leaflet)
library(raster)

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
      selectInput("Lat", "Latitude:",
                  c("No data uploaded data" = "NoData")
      ),
      selectInput("Lon", "Longitude:",
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
        tabPanel("Summary Map",
                 tags$style(type = "text/css", "#DensityMap {height: 60vh !important;}"),
                 leafletOutput("DensityMap"),
                 sliderInput(inputId = "Res",label = "Date Slider",min = 0.001,max = 0.1,value = .05,step = .005)),
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
    
    read_csv(infile$datapath)
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
                      selected = tail(x,1)
    )
    
    updateSelectInput(session, "Count",
                      label = "Count Column",
                      choices = ColNames,
                      selected = tail(x,1)
    )
    
    updateSelectInput(session, "Lat",
                      label = "Latitude Column",
                      choices = ColNames,
                      selected = tail(x,1)
    )
    
    updateSelectInput(session, "Lon",
                      label = "Longitude Column",
                      choices = ColNames,
                      selected = tail(x,1)
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
     
     if (input$Count == "NoData") {
      stop("Select Count Column")
     }
     
     if (input$Date == "NoData") {
       stop("Select Date Column")
     }
     
     print(str(dataFile()))
     
     dataFile() %>% 
       ggplot(aes_string(x = input$Date, y = input$Count)) +
       geom_point(color = "white", alpha = .3) +
       scale_x_date(date_breaks = "2 weeks") +
         theme_minimal() +
         theme(panel.grid  =  element_line(colour = "dark grey"))
       
    })
   
   output$DensityMap <- renderLeaflet({ 
     if (is.null(dataFile())) 
       return(NULL) 
     
     if (input$Lat == "NoData") {
       stop("Select Latitude Column")
     }
     
     if (input$Lon == "NoData") {
       stop("Select Longitude Column")
     }
     
     
     if (input$Count == "NoData") {
       stop("Select Count Column")
     }
     
     #create density grid
     

    #mosRas <- raster(nrows = NumRow, ncols = NumCol)
    
    mospoints <- dataFile() %>% 
      st_as_sf(coords = c(input$Lat,input$Lon),crs = 3857)
    
    #str(mospoints)
   
    #will want to filter mospoints 
    
    mos_sp <- as(mospoints, "Spatial")
    
    str(mos_sp)
    
   # str(mos_sp)
    
    #create raster
    mosRas <- raster()
    
    #give same spatial attributes as sp object
    extent(mosRas) <- extent(mos_sp)
    res(mosRas) <- input$Res
    
    str(mosRas)
    #seems to be breaking here
    
    denRas <- rasterize(x = mos_sp, y = mosRas, field = 'count', fun = mean)
    
    summary(denRas)
    
    #add leaflet map
    pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#bd0202"), values(denRas),
                        na.color = "grey")
    
    leaflet() %>% 
      addProviderTiles(providers$Stamen.Toner) %>%
      addRasterImage(denRas, colors = pal, opacity = 0.8,project = T) %>%
      addLegend(pal = pal, values = values(denRas),
                title = "Mean Count") 
    
    
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
