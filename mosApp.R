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
library(lubridate)


options(shiny.reactlog = TRUE)

ui <- fluidPage(
  theme = shinytheme("darkly"),
  titlePanel(title = "Mosquito Control Resource Optimization"
           ),
  fluidRow(
    column(4,
    tags$h4(
      tags$a(href = "https://github.com/collinschwantes/MosquitoApp", 
             target="_blank",
             "Instructions and Github Repo")
      )
     ),
    column(5),
    column(3,
           tags$h4(
             tags$a(href = "https://github.com/collinschwantes/MosquitoApp/issues", 
                    target = "_blank",
                    "Provide Feedback",icon("bug", "fa-1x"))
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
      selectInput("Species", "Species:",
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
      tabsetPanel(id = "TabIn",
        tabPanel("Getting Started", tags$h3("Overview"), 
                 tags$p("This project aims to make an accessible model for mosquito control resource optimization. The model uses data provided by users to estimate the mosquito populations in the sampling area for the sampling timeperiod, and the optimal time to apply a treatment or multiple treatments."),
                 tags$h3("Instructions"),tags$hr(),
                 tags$p("This application accepts CSV and excel files (.xls, .xlsx) as inputs. None of your data are stored long term on the Rshiny servers. At the moment, only a single spreadsheet can be uploaded. If you have multiple years of data, please consolidate to one spread sheet."),
                 tags$li("Consistent date formats are required"),
                 tags$li(tags$a(href = "https://github.com/collinschwantes/MosquitoApp/blob/master/SyntheticData/ExampleCulex.csv", target = "_blank",
                                "Example Data Sheet")),
                 tags$li(tags$a(href = "https://github.com/collinschwantes/MosquitoApp/issues",target = "_blank", "Provide Feedback",icon("bug", "fa-1x"))),
                 tags$li("Minimum data requirements for model:"),
                 tags$ul(tags$li("Count data"),
                         tags$li("Date Collected")
                         ),
                 tags$li("User Inputs:"),
                 tags$ul(tags$li("Mosquito Life span in days"),
                         tags$li("Percent of poplation knocked down by treatment"),
                         tags$li("Number of treatments applied"),
                         tags$li("Number of days between treat")
                 )
                 
                                ),
        tabPanel("Data Summary", value = "datasum",
                 plotOutput("MosPopPlot"),
                 dataTableOutput("contents")
        )
        ,
        # tabPanel("Summary Map",
        #          tags$style(type = "text/css", "#DensityMap {height: 60vh !important;}"),
        #          leafletOutput("DensityMap"),
        #          sliderInput(inputId = "Res",label = "Resolution Slider",min = 0.001,max = 0.1,value = .01,step = .005)),
        tabPanel("Mosquito Population Model",
                 sliderInput(inputId = "MosLife",label = "Mosquito Lifespan in Days",min = 1,max = 30,value = 3,step = 1),
                 p("will add a plot")
                 ),
        tabPanel("Resouce Optimization Model",
                 tags$h3("Model Parameters"),
                 fluidRow(
                   h4("Treatment Inputs:"),
                 column(width = 4,
                   numericInput(inputId = "Knockdown",
                                label = "Knockdown rate",
                                min = 0,max = 50, value = 35,
                                step = 5)
                   ),
                 column(width = 4,
                 numericInput(inputId = "Impulses",
                              label = "Number of Applications",
                              min = 0,max = 10, value = 3,step = 1)
        
                 ),
                 column(width = 4,
                        numericInput(inputId = "WaitTime",
                                     label = "Days Between Applications",
                                     min = 0,max = 300, value = 7,step = 7)
                        
                 )
                 ),
                 fluidRow(
                   h4("Model Controls:"),
                 column(width = 6,
                        numericInput(inputId = "Kmax", 
                                     label = "Number of Opitimum Searches (Kmax)", 
                                     value = 5,min = 1,max = 10 )
                        ),
                 column(width = 6,
                        numericInput(inputId = "Jmax", label = "Fourier Modes (Accurarcy vs Runtime)",value = 10,min = 1,max = 25)
                        )
                 ),
                 hr(),
               fluidRow(
                 p("will add plot")
               )
                 
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  dataFile <- reactive({
    validate(
        need(expr = input$file1 != "", message = "Upload a CSV, XLS, or XLSX datafile that contains Date and Count information for mosquito traps")
    )    
    
   input$file1
    
  })
  
  ## update main tab panel to include summary plot and table
  
  observeEvent(input$file1, {
    updateTabsetPanel(session,inputId = "TabIn",selected = "datasum")
  })
  
  ## update selection inputs for sheets
  observeEvent(input$file1, {
    
    x <- dataFile()
    # 
    # print("From Observe")
    # print(x$type)
        
    if (x$type != "text/csv") {
      
      SheetNames <- excel_sheets(x$datapath)
      
      #print(SheetNames)
      ## insert UI 
      if (length(SheetNames) > 0) {
      insertUI(selector = "#file1_progress", where = "afterEnd",  
               ui = selectInput("sheet", "Select Sheet", SheetNames,selected = "")
              )
      }
    }
    
  })
  
  #create event reactive for excel
  
  MosXL <- reactive({
    
    x <- dataFile()
    
    if (x$type == "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet") {
      df <- read_xlsx(path = x$datapath,sheet = input$sheet, .name_repair = "universal" )
    }
    
    #xls  
    if (x$type == "application/vnd.ms-excel") {
      
      df <- read_xls(path = x$datapath,sheet = input$sheet, .name_repair = "universal")
    }
    
    return(df)
    
  })
  
  #create reactive for CSV
  
  
  MosData <- reactive({ 
    
    req(dataFile())
    
    x <- dataFile()
    
    #print("In Mos Data")
    #print(x)
    
    #xlsx
    
    if (x$type == "text/csv") {
     df <- read_csv(file = x$datapath)
    } else {
      df <- MosXL()
    }
    
    #str(df)
    
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
    
    updateSelectInput(session, "Species",
                      label = "Species Column",
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
  
  ## standardize data
  
  cleanData <- reactive({
    
    validate(
      need(input$Count != 'NoData',"Select Count Column" ),
      need(input$Date != 'NoData', "Select Date Column")
    )
    
    df <- MosData() 
    
    dfNames <- names(df)
    
    CountCol <- match(table = dfNames, x = input$Count)
    DateCol <- match(table = dfNames, x = input$Date)
    SpeciesCol <- match(table = dfNames, x = input$Species)
    
    df <- df %>% 
      filter(!is.na(.[[DateCol]]))
    
    print("Mos Data frame Structure after filter")
    str(df)
    
    ClassDate <- class(df[[DateCol]])
    print(ClassDate)
    
    if (length(ClassDate) > 1 ) {
      
      DateChar <- as_date(df[[DateCol]])
      
      print(class(DateChar))
      print(head(DateChar))
      print(summary(DateChar))
      # DateParse <- parse_date_time(x = DateChar,orders = c("ymd","mdy","dmy"),truncated = 3)
      # print(head(DateParse))
       df[[DateCol]] <- DateChar
      # str(df)
      
      ClassDate <- class(df[[DateCol]])
      
      print(ClassDate)
    }
    
    if (ClassDate == "character") {
      
      print(head(df[[DateCol]]))
      
      df[[DateCol]] <- parse_date_time(df[[DateCol]],orders = c("mdy","dmy","ymd") )
      print(df[[DateCol]])
        
    }
    
    if (is.numeric(df[[DateCol]])) {
      print("Is Numeric")
      
      df$Count <- as.numeric(df[[CountCol]])
      df$Date = as.Date(df[[DateCol]],origin = "1899-12-30")
      
      # 
      # dfDateCount <- df %>% 
      #   mutate(Count = as.numeric(.[[CountCol]])) %>% 
      #   mutate(Date = as.Date(.[[DateCol]], origin = "1899-12-30"))
      # 
      # print("Clean Data frame Structure")
      # str(dfDateCount)

      
    # } else if (ClassDate == "Date") {
    #   print("Its a date")
    #   
    #   dfDateCount <- df %>% 
    #     mutate(Count = as.numeric(.[[CountCol]])) %>% 
    #     mutate(Date = .[[DateCol]])
      
    } else {
      print("Is Not Numeric")
      
      
      df$Count <- as.numeric(df[[CountCol]])
      df$Date <- as.Date(df[[DateCol]])
      
      # dfDateCount <- df %>% 
      #   mutate(Count = as.numeric(.[[CountCol]])) %>% 
      #   mutate(Date = as_date(.[[DateCol]]))
      # 
      # print("Clean Data frame Structure")
      # print(str(dfDateCount$Date))
      # print(str(dfDateCount[[DateCol]]))

      
    }
    
    
    df$SpeciesCol <- df[[SpeciesCol]]
    
    
    return(df)
    
  })
  
   output$contents <- renderDataTable(options = list(scrollX = TRUE), { 
    # input$file1 will be NULL initially. 
     validate(
       need(expr = input$file1 != "", message = "")
     ) 
     
     MosData()
  })
  
   output$MosPopPlot <- renderPlot(bg = "transparent",{
     
     
     cleanData() %>% 
       ggplot(aes(x = Date, y = Count)) +
       geom_point(color = "white", alpha = .3) +
       geom_smooth() +
       scale_x_date(date_breaks = "1 months") +
         theme_minimal() +
         theme(panel.grid  =  element_line(colour = "dark grey")) +
       theme(axis.text.x = element_text(angle = 45, hjust = 1))
       
    })
   
   
  ## MAP ##
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
    
    mospoints <- cleanData() %>% 
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


