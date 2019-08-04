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
library(rgdal)
library(readxl)
library(reactlog)



options(shiny.reactlog = TRUE)

ui <- fluidPage(
  theme = shinytheme("darkly"),
  titlePanel(title = "Mosquito Control Resource Optimization"
           ),
  fluidRow(
    column(4,
    tags$h4(
      tags$a(href = "https://github.com/collinschwantes/MosquitoApp",
             "Find the Githup project here")
      )
     ),
    column(5),
    column(3,
           tags$h4(
             tags$a(href = "https://github.com/collinschwantes/MosquitoApp/issues",
                    "Report Issues Here",icon("bug", "fa-1x"))
           )
    )
    ),
  sidebarLayout(
    sidebarPanel(
      fluidRow(
      fileInput("file1", "Choose CSV or excel File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv",
                  "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                  ".xlsx",
                  "application/vnd.ms-excel",
                  ".xls"
                )
      ),
      # radioButtons("filetype", "Choose File Type:", inline = T, 
      #                    choiceNames = 
      #                      list(".csv",".xls", ".xlsx"),
      #                    choiceValues = 
      #                      list("csv","xls","xlsx")
      #                      ),
      # conditionalPanel(
      #   condition = "input.filetype != 'csv'",
      #   selectInput("sheet", "Select Sheet",
      #               list("Upload excel file"))
      # ),
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
                 sliderInput(inputId = "Res",label = "Resolution Slider",min = 0.001,max = 0.1,value = .01,step = .005)),
        tabPanel("Resouce Optimization Model",
                  tags$p("This panel is intentionally blank")
                )
      )
    )
  )
)

server <- function(input, output, session) {
  
  dataFile <- reactive({
    validate(
        need(expr = input$file1 != "", message = "Upload a datafile")
    )    
    
   input$file1
    
  })
  
  ## update selection inputs for sheets
  observe({
    
    x <- dataFile()
    # 
    # print("From Observe")
    # print(x$type)
        
    if (x$type != "text/csv") {
      
      SheetNames <- excel_sheets(x$datapath)
      
      print(SheetNames)
      ## insert UI 
      
      insertUI(selector = "#file1_progress", where = "afterEnd",  
               ui = selectInput("sheet", "Select Sheet", SheetNames,selected = "")
              )
    }
    
  })
  
  #create event reactive for excel
  
  MosXL <- reactive({
    
    x <- dataFile()
    
    if (x$type == "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet") {
      df <- read_xlsx(path = x$datapath,sheet = input$sheet )
    }
    
    #xls  
    if (x$type == "application/vnd.ms-excel") {
      
      df <- read_xls(path = x$datapath,sheet = input$sheet)
    }
    
    return(df)
    
  })
  
  #create reactive for CSV
  
  
  MosData <- reactive({ 
    
    req(dataFile())
    
    x <- dataFile()
    
    print("In Mos Data")
    print(x)
    
    #xlsx
    
    if (x$type == "text/csv") {
     df <- read_csv(file = x$datapath)
    } else {
      df <- MosXL()
    }
    
    return(df)
    
  })
  
  observe({
    
    df <- MosData()
    
    ColNames <- names(df)

    #print(paste("selected:",tail(x,1)))

    updateSelectInput(session, "Date",
                      label = "Date Column",
                      choices = ColNames,
                      selected = ""
    )

    updateSelectInput(session, "Count",
                      label = "Count Column",
                      choices = ColNames,
                      selected = ""
    )

    updateSelectInput(session, "Lat",
                      label = "Latitude Column",
                      choices = ColNames,
                      selected = ""
    )

    updateSelectInput(session, "Lon",
                      label = "Longitude Column",
                      choices = ColNames,
                      selected = ""
    )
   
  })
  
  
   output$contents <- renderDataTable({ 
    # input$file1 will be NULL initially. 
     validate(
       need(expr = input$file1 != "", message = "")
     ) 
     
     MosData()
  })
   
   output$MosPopPlot <- renderPlot(bg = "transparent",{
    
  req(dataFile())
     ## update these to validate and need
     
     validate(
       need(input$Count != 'NoData',"Select Count Column" ),
       need(input$Date != 'NoData', "Select Date Column")
     )
     
     # if (input$Count == "NoData") {
     #  stop("Select Count Column")
     # }
     # 
     # if (input$Date == "NoData") {
     #   stop("Select Date Column")
     # }
     
     print(str(MosData()))
     
     MosData() %>% 
       ggplot(aes_string(x = input$Date, y = input$Count)) +
       geom_point(color = "white", alpha = .3) +
       scale_x_date(date_breaks = "2 weeks") +
         theme_minimal() +
         theme(panel.grid  =  element_line(colour = "dark grey"))
       
    })
   
   output$DensityMap <- renderLeaflet({ 
     if (is.null(dataFile())) 
       return(NULL) 
     
     ## update these to validate and need
     
     validate(
       need(input$Lat != 'NoData',"Select Latitude Column" ),
       need(input$Lon != 'NoData', "Select Longitude Column"),
       need(input$Count != 'NoData', "Select Count Column")
     )
     

     #create density grid
     

    #mosRas <- raster(nrows = NumRow, ncols = NumCol)
    
    mospoints <- MosData() %>% 
      st_as_sf(coords = c(input$Lon,input$Lat),crs = 3857)
    
   
    #will want to filter mospoints 
    
    mos_sp <- as(mospoints, "Spatial")
    

    
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
   
  # output$ModelResults 
   
}

shinyApp(ui, server)


