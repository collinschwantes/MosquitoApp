## UI
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
library(pracma)
library(matlib)
library(NlcOptim)
library(shinycssloaders)
library(nloptr)
library(sfsmisc) 
library(shinyBS)


source("OptimalControlFunctions.R")


options(shiny.reactlog = FALSE)

ui <- fluidPage(
  theme = shinytheme("darkly"),
  titlePanel(title = "Mosquito Control Resource Optimization"
           ),
  fluidRow(
    column(4,
    tags$h4(
      tags$a(href = "https://github.com/collinschwantes/MosquitoApp", 
             target = "_blank",
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
      # selectInput("TrapType", "Trap Type:",
      #             c("Oviposition" = "ovi",
      #               "Light" = "lig",
      #               "CO2" = "CO2  ")),
      # selectInput("Count", "Count Data:",
      #             c("No data uploaded data" = "NoData")
      #             ),
      # selectInput("Date", "Date:",
      #             c("No data uploaded data" = "NoData")
      #             ),
      bsCollapse(id = "collapseParameters",  multiple = T, 
                 open = c("Column Selections", "Population Model Parameters"),
        bsCollapsePanel("Column Selections", style = NULL,
                        selectInput("Count", "Count Data:",
                                    c("No data uploaded data" = "NoData")
                        ),
                        selectInput("Date", "Date:",
                                    c("No data uploaded data" = "NoData")
                        )
                        
        ),         
        bsCollapsePanel("Population Model Parameters",  style = NULL,
                        sliderInput(inputId = "MosLife",
                          label = "Mosquito Lifespan in Days",
                          min = 1,max = 30,value = 3,step = 1),
                        sliderInput(inputId = "MosDecay",
                          label = "Mosquito Lifecycles Between Seasons",
                          min = 2,max = 100,value = 3,step = 1),
                        sliderInput(inputId = "Jmax", label = "Emergence Fourier Modes",
                          value = 10,min = 1,max = 200, step = 1)
        ),
        bsCollapsePanel("Resource Optimization Parameters", style = NULL,
                          em("Treatment Inputs:"),
                                 numericInput(inputId = "rho",
                                              label = "Percent Knockdown",
                                              min = 1,max = 30, value = 15,
                                              step = 1),
                                 numericInput(inputId = "Npulse",
                                              label = "Number of Applications",
                                              min = 0,max = 10, value = 3,step = 1),
                                 numericInput(inputId = "days_between",
                                              label = "Minimum Days Between Applications",
                                              min = 0,max = 300, value = 7,step = 7),
                          em("Model Controls:"),
                                 sliderInput(inputId = "Kmax", 
                                             label = "Average Population Fourier modes", 
                                             value = 1,min = 1,max = 50 ),
                          ## add global opt parameter 
                                 selectInput(inputId = "global_opt", 
                                             label = "Optimization Algorithm", 
                                             choices =  c("Local - Fastest" = 0,
                                                          "Global - GN_DIRECT_L_RAND" = 1,
                                                          "Global - GN_ISRES" = 2),
                                             selected = 0),
                                 sliderInput(inputId = "JmaxOpt", 
                                             label = "Emergence Fourier Modes", 
                                             value = 1,min = 1,max = 200, step = 1))
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
                         tags$li("Mosquito Lifecycle Between Seasons"), ## needs def
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
                 fluidRow(

                   column(width = 6, h4("Fitted Population Model", style = "text-align: center;"),
                          withSpinner(plotOutput("MosPopFitted"))
                          ),
                    column(width = 6, h4("Fitted Emergence Rate", style = "text-align: center;"), 
                          withSpinner(plotOutput("MosEmergence"))
                          )
                   
                   
                 
                    )
                 ),
        tabPanel("Resouce Optimization Model",
                 
                 fluidRow(
                   
                   column(width = 8, h4("Fitted Population Model", style = "text-align: center;"),
                          withSpinner(plotOutput("OptModel"))
                   ),
                   column(width = 4, #h4("Summary Statistics", style = "text-align: center;"),
                          em("Percent Reduction"),
                          h4(textOutput("percent_reduction")),
                          em("Accuracy Measure"),
                          h4(textOutput("accuracy_measure")),
                          em("Control Times (day of year)"),
                          h4(textOutput("PulseTimes")),
                          em("Average Population"),
                          h4(textOutput("AvePopUnCont")),
                          em("Average Controlled Population"),
                          h4(textOutput("AvePopCont"))
                          
                          
                          
                   )
                   
                   
                   
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
    
    # updateSelectInput(session, "Species",
    #                   label = "Species Column",
    #                   choices = ColNames,
    #                   selected = ""
    # )
# 
    # updateSelectInput(session, "Lat",
    #                   label = "Latitude Column",
    #                   choices = ColNames,
    #                   selected = ""
    # )
    # 
    # updateSelectInput(session, "Lon",
    #                   label = "Longitude Column",
    #                   choices = ColNames,
    #                   selected = ""
    # )
#    
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
    #SpeciesCol <- match(table = dfNames, x = input$Species)
    
    df <- df %>% 
      filter(!is.na(.[[DateCol]]))
    
    print("Mos Data frame Structure after filter")
    str(df)
    
    ClassDate <- class(df[[DateCol]])
    print(ClassDate)
    
    if (length(ClassDate) > 1 ) {
      
      DateChar <- as_date(df[[DateCol]])
      
      # print(class(DateChar))
      # print(head(DateChar))
      # print(summary(DateChar))
      # DateParse <- parse_date_time(x = DateChar,orders = c("ymd","mdy","dmy"),truncated = 3)
      # print(head(DateParse))
       df[[DateCol]] <- DateChar
      # str(df)
      
      ClassDate <- class(df[[DateCol]])
      
      # print(ClassDate)
    }
    
    if (ClassDate == "character") {
      
      # print(head(df[[DateCol]]))
      
      df[[DateCol]] <- parse_date_time(df[[DateCol]],orders = c("mdy","dmy","ymd") )
      # print(df[[DateCol]])
        
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
    
    
    #df$SpeciesCol <- df[[SpeciesCol]]
    
    #convert date to day of the year
    df$DayOfYear  <- yday(df$Date)
    
    print("day of year added")
    
    return(df)
    
  })
  
   output$contents <- renderDataTable(options = list(scrollX = TRUE), { 
    # input$file1 will be NULL initially. 
     validate(
       need(expr = input$file1 != "", message = "Upload a file")
     ) 
     MosData()
  })
  
   output$MosPopPlot <- renderPlot(bg = "transparent",{

     cleanData() %>% 
       ggplot(aes(x = Date, y = Count)) +
       geom_point(color = "white", alpha = .3) +
       #geom_smooth() +
       scale_x_date(date_breaks = "1 months") +
         theme_minimal() +
         theme(panel.grid  =  element_line(colour = "dark grey")) +
       theme(axis.text.x = element_text(angle = 45, hjust = 1))
       
    })
   
  #  
  # ## MAP ##
  # output$DensityMap <- renderLeaflet({ 
  #    if (is.null(dataFile())) 
  #      return(NULL) 
  #    
  #    ## update these to validate and need
  #    
  #    validate(
  #      need(input$Lat != 'NoData',"Select Latitude Column" ),
  #      need(input$Lon != 'NoData', "Select Longitude Column"),
  #      need(input$Count != 'NoData', "Select Count Column")
  #    )
  #    
  # 
  #    #create density grid
  #    
  # 
  #   #mosRas <- raster(nrows = NumRow, ncols = NumCol)
  #   
  #   mospoints <- cleanData() %>% 
  #     st_as_sf(coords = c(input$Lon,input$Lat),crs = 3857)
  #   
  #  
  #   #will want to filter mospoints 
  #   
  #   mos_sp <- as(mospoints, "Spatial")
  #   
  # 
  #   
  #   #create raster
  #   mosRas <- raster()
  #   
  #   #give same spatial attributes as sp object
  #   extent(mosRas) <- extent(mos_sp)
  #   res(mosRas) <- input$Res
  #   
  #   str(mosRas)
  #   #seems to be breaking here
  #   
  #   denRas <- rasterize(x = mos_sp, y = mosRas, field = 'count', fun = mean)
  #   
  #   summary(denRas)
  #   
  #   #add leaflet map
  #   pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#bd0202"), values(denRas),
  #                       na.color = "grey")
  #   
  #   leaflet() %>% 
  #     addProviderTiles(providers$Stamen.Toner) %>%
  #     addRasterImage(denRas, colors = pal, opacity = 0.8,project = T) %>%
  #     addLegend(pal = pal, values = values(denRas),
  #               title = "Mean Count") 
  # 
  #  })
  #  
  # # output$ModelResults 
  # 
  
  ModelOutputs <- reactive({
    
    #
    tau_mosq <- input$MosLife  #where tau_mosq is the mosquito lifetime input by user in units of days 
    
    #mu = natural mosquito death rate
    mu <- 1 / tau_mosq #program should set mu = 1 / tau_mosq,
    #where tau_mosq is the mosquito lifetime input by user in units of days 
    
    #m = number of mosquito lifetimes for population decay between seasons..
    m <- input$MosDecay #this will be input by the user, and must be an integer greater than 2, upperbound 100, default value 3
    
    # N_lam = max fourier mode order to calculate
    
    N_lam <- input$Jmax  #user input between 1 and 100, default value 25 (I referred to this as jmax over Skype - JD)
    
    # day of the year 
    
    #get mean counts per day 
    
    print("summarizing data")
    
    InputData <- cleanData() %>% 
      dplyr::select(Count, DayOfYear) %>% 
      group_by(DayOfYear) %>% 
      summarize(MeanCount = mean(Count)) 
  
      InputData <- na.omit(InputData)
      
    str(InputData)
    
    ## can't handle more than 100 pts on my machine
    
    # if(length(InputData$MeanCount) > 10) {
    #   
    #   print("Too Many data points")
    #   
    #   BY <- round(length(InputData$MeanCount)/10)
    #   
    #   subsample <- seq(1, length(InputData$MeanCount), by = BY)
    #  
    #   InputData <- InputData[subsample,] 
    #   
    # }
    
    

    #first round user time inputs to integer values
    t_in <-  InputData$DayOfYear ## grab from data input
    
    y_in <- InputData$MeanCount
    
    #defining N0 = number of input data points (not a user input)
    N0 = length(y_in) # grab from dataset input
    
    #defining vector of time differences to be used later (not a user input)
    delta_t_in <- numeric(N0 - 1) 
    
    print("Defining Delta_t_in")
     
    for(i in 1:(N0 - 1)){
      delta_t_in[i] <- t_in[i+1] - t_in[i]
      if (delta_t_in[i] < 0)
        stop("non-increasing measurement times")
    }
    
    
    #N_pts = number of points in extended data set, N_pts = N0 + 3 always (not a user input)
    N_pts = N0 + 3
    
    
    #defining extended trap count and time data vectors
    t_dat = numeric(N_pts)
    
    
    print("Defining y_dat")
    
    if (t_in[1] - m / mu > 0){
      y_dat = c(0,0, y_in,0)
      
      t_dat[1] = 0
      t_dat[2] = (m -1) / mu
      for (i in 3 : (N_pts - 1)){
        t_dat[i] = t_in[i - 2] - t_in[1] + m / mu
      }
      t_dat[N_pts] = t_dat[N_pts - 1] + (m) / mu
      
      t_dat=matrix(t_dat, ncol=1)
      
    } else {
      y_dat = c(0, y_in, 0, 0)
      
      t_dat[1] = 0
      for (i in 2: (N_pts - 2)){
        t_dat[i] = t_in[i - 1]
      }
      t_dat[N_pts - 1] = t_dat[N_pts - 2] + (2*(m - 1)) / mu - t_dat[2]
      t_dat[N_pts] = t_dat[N_pts - 1] + 1 / mu
    }
    
    
    
    #extended vector of time differences
    delta_t_dat = numeric(N_pts)
    for (i in 1 : (N_pts - 1)){
      delta_t_dat[i] = t_dat[i + 1] - t_dat[i]
    }
    
    delta_t_dat[N_pts] = 0
    
    delta_t_dat= matrix(delta_t_dat,ncol=1)
    
    #tau = length of 'season' in days
    tau = t_dat[N_pts]
    
    
    #Defining matrices 'J' and 'M'
    
    #J matrix and inverse
    J = matrix(0, nrow= N_pts, ncol=N_pts)
    
    for (i in 1 : (N_pts - 1)){
      J[i, i] = 1
      J[i + 1, i] = -exp(-mu*delta_t_dat[i])
    }
    
    J[N_pts, N_pts] = 1
    J[1, N_pts] = -exp(-mu * delta_t_dat[N_pts])
    
    
    #M matrix and inverse
    M = matrix(0, nrow= N_pts, ncol=N_pts)
    
    for (i in 2 : (N_pts - 1)){
      M[i, i] <- (1 - exp( -mu * delta_t_dat[i - 1] ) ) /(mu * delta_t_dat[i - 1] ) - exp( -mu * delta_t_dat[i - 1])
      M[i + 1, i] <- 1 - (1 - exp(-mu * delta_t_dat[i]) )/(mu * delta_t_dat[i])
    }
    
    M[N_pts, N_pts] = (1 - exp( (-1)*mu * delta_t_dat[N_pts - 1] ) ) /(mu * delta_t_dat[(N_pts - 1)] ) -exp( -mu * delta_t_dat[(N_pts - 1)])
    M[1, N_pts] = 0
    M[1, 1] = 0
    M[2, 1] = 1- ( 1 - exp(-mu * delta_t_dat[1]) )/(mu * delta_t_dat[(1)]) 
    
    
    #Obtaining emergence rate vector lambda_dat by contrained optimization
    
    Aeq = matrix(numeric(N_pts), nrow=1)
    Aeq[(1)] = 1
    Aeq[(N_pts)] = -1
    beq = 0
    bineq = numeric(N_pts)
    lb = numeric(N_pts)
    ###################  
    
    
    objective_fun = function(x) {t(( (1 / mu) * (mldivide(J, (M %*% x), pinv = TRUE)) - y_dat))%*%( (1 / mu) * (mldivide(J, (M %*% x), pinv = TRUE)) - y_dat)}
    
    
    print(
      lapply(list(LengthJ = (J), lengthM = (M), lengthx = (N_pts)),FUN = length)
    )

    
    lambda_dat = fmincon(numeric(N_pts),
                         objective_fun,
                         gr = NULL,
                         method = "SQP",
                         A = (-(1 / mu) * ( inv(J) %*%  (M))), 
                         b = bineq,
                         Aeq = Aeq, beq = beq, lb = lb, ub = NULL)
    
    print("Lamba_dat created")
    #########
    
    
    #Calculating emergence rate fourier modes lam_fourier
    
    
    lam_fourier = numeric(N_lam + 1)
    
    
    
    for (j in 1 : (N_pts-1)){
      lam_fourier[1] =  lam_fourier[1] + (1 / tau)*( lambda_dat$par[j] + lambda_dat$par[j + 1] ) * delta_t_dat[j] / 2
    } #zero mode
    
    
    for (k in 2 : (N_lam + 1)) #non-zero postive modes (negative modes given by complex conjugates of positve modes) 
    {
      for (j in 1 : (N_pts - 1)){
        lam_fourier[(k)] = lam_fourier[(k)] +( ( lambda_dat$par[(j + 1)] - lambda_dat$par[(j)] )  / delta_t_dat[(j)] ) *( tau / (2 * pi * (k-1) ) )* ( ( exp(- 2 * pi * complex(real = 0, imaginary = 1) * (k-1) * t_dat[(j + 1)] / tau ) -exp(- 2 * pi * complex(real = 0, imaginary = 1) * (k-1) * t_dat[(j)]/ tau) ) / (2 * pi * (k-1)) )+ complex(real = 0, imaginary = 1) * ( lambda_dat$par[(j + 1)] * exp(- 2 * pi * complex(real = 0, imaginary = 1) * (k-1) * t_dat[(j + 1)] / tau ) -lambda_dat$par[(j)] * exp(- 2 * pi * complex(real = 0, imaginary = 1) * (k-1) * t_dat[(j)] /tau ) ) / (2 * pi * (k-1) )
      }
    }
    
    #Obtaining fitted curves
    t_steps = 6 * tau*10 + 1
    
    t_vec = linspace(0, 6*tau, n = t_steps);
    lambda_fourier_function = numeric(t_steps)
    y_fourier_uncontrolled = numeric(t_steps)
    val_fourier = numeric(t_steps)
    
    y_best_fit_uncontrolled = numeric(t_steps) ## adding optimal model to pop graph
    
    
    
    for( i in 1:t_steps){
      lambda_fourier_function[i] = lam_fourier[1]
      for( j in 2 : (N_lam + 1)){
        lambda_fourier_function[i] = lambda_fourier_function[i] + lam_fourier[(j)] * 
          exp( 2 * pi * complex(real = 0, imaginary = 1) * (j-1) * t_vec[(i)]/ tau) +
          Conj(lam_fourier[(j)]) * exp( -2 * pi * complex(real = 0, imaginary = 1) * (j-1) * t_vec[(i)] / tau)
      }
      
    }
    
    for( i in 1:t_steps){
      
      for( j in 2: (N_lam + 1)){
        val_fourier[(i)] = val_fourier[(i)] + lam_fourier[(j)] * 
          ( tau / (2 * pi * complex(real = 0, imaginary = 1) *(j-1) + mu * tau) )*
          ( exp( 2* pi * complex(real = 0, imaginary = 1) * (j-1) * t_vec[(i)] / tau) - 
              exp(-mu * t_vec[(i)] )) + 
          Conj(lam_fourier[(j)]) * ( tau / (-2 * pi * complex(real = 0, imaginary = 1) *(j-1) + mu * tau ) )*
          ( exp( -2* pi * complex(real = 0, imaginary = 1) * (j-1)* t_vec[(i)] / tau) - exp(-mu * t_vec[(i)] ))
      }
    }
    
    
    
    for(k in 2:t_steps){    
      
      
      y_fourier_uncontrolled[k] = y_dat[1] * exp(- mu * t_vec[k]) +
        (lam_fourier[1] / mu ) * (1 - exp(-mu * t_vec[k])) +  val_fourier[k]
      
      for (j in 1: N_pts - 1){                                                          #new 11/10
        if (mod(t_vec[k-1], tau) >= t_dat[j] && mod(t_vec[k-1],tau) < t_dat[j + 1]){    #new 11/10
          
          
          y_best_fit_uncontrolled[k] = y_best_fit_uncontrolled[k-1] +(-mu * y_best_fit_uncontrolled[k-1]  +lambda_dat$par[j+1] * (mod(t_vec[k-1], tau) -t_dat[j])/ delta_t_dat[j] +lambda_dat$par[j] * (t_dat[j+1]  -mod(t_vec[k-1], tau)) / delta_t_dat[j])*(t_vec[k] - t_vec[k-1])                   #new 11/10
        }                                                                             #new 11/10
      }
      
    }
    
    
    y_uncontrolled_plot = numeric(10 * tau + 1)
    lambda_fourier_plot = numeric(10 * tau + 1)
    t_vec_plot = numeric(10 * tau + 1)
    
    y_best_fit_uncontrolled_plot = numeric(10 * tau + 1)  ## for optimal pop graph
    
    for( i in 1 : (10*tau+1)){
      
      
      y_uncontrolled_plot[i] = y_fourier_uncontrolled[(i + 10*tau*5)]
      lambda_fourier_plot[(i)] = lambda_fourier_function[(i+ 10*tau*5)]
      
      y_best_fit_uncontrolled_plot[i] = y_best_fit_uncontrolled[(i + 10*tau*5)]; # for pop graph
      
      #new code shifts times back to orginal values for model output
      if( t_in[1] - m / mu > 0){
        t_vec_plot[(i)] = t_vec[(i)]-t_dat[(3)] +t_in[(1)];
      } else {
        t_vec_plot[(i)] = t_vec[(i)] -t_dat[(2)] + t_in[(1)];
        
      }
    }
    
    
    #new code shifts times back to orginal values for data point plotting
    t_dat_plot = numeric(N_pts)
    for(i in 1:N_pts){
      
      if( t_in[(1)] - m / mu > 0){
        t_dat_plot[i] = t_dat[i]  - t_dat[(3)] + t_in[(1)];
      } else {
        t_dat_plot[i] = t_dat[i]  - t_dat[(2)] + t_in[(1)];
      }
    }
    ## grab objects for plots
    ModelOutputList <- list(
    
    #for fitted population model
    "SummarizedPopData" = data.frame('DayOfYear' = t_dat_plot, 
                                     "MeanMosPop" = y_dat, 
                                     stringsAsFactors = F),
    "FittedPopModel" =  data.frame("FittedTime" = t_vec_plot, 
                                   "FourierPop" = as.numeric(y_uncontrolled_plot), # convertfor plotting - fourier
                                   "BestFitPop" = as.numeric(y_best_fit_uncontrolled_plot), ## adding best fit plot
                                   stringsAsFactors = F),
    #for emergence model
    "FittedEmergencePoints" = data.frame("DayOfYear" = t_dat_plot,
                                        "EmergenceRate" =  as.numeric(lambda_dat$par)),
    
   "ApproximatedEmergenceRate" = data.frame("FittedTime" = t_vec_plot,
                                            "ApproxEmergence" = as.numeric(lambda_fourier_plot))
    )
    
  })
  
  # model output for Fitted Pops
  
  #ModelOutputs_d <- debounce(ModelOutputs,millis = 1000)
  
  output$MosPopFitted <- renderPlot(bg = "transparent",{
    
      MO <- ModelOutputs()
      
      # str(MO)
      
      ggplot() +
        geom_line(data = MO$FittedPopModel, aes(x =FittedTime , y = BestFitPop, color = "Best Fit Population Model"), size = 1.5 ) +
        geom_line(data = MO$FittedPopModel, aes(x = FittedTime, y = FourierPop, color = "Fitted Model Fourier Approximation"), size = 1.5 ) +
        geom_point(data = MO$SummarizedPopData, aes(x = DayOfYear, y = MeanMosPop, color = "Mean Count"), alpha = .75) +
        scale_color_manual(values = c("#1b9e77","#225ea8","#ffffff")) +
        theme_minimal() +
        theme(panel.grid  =  element_line(colour = "dark grey"),
              text = element_text(colour = "white"),
              axis.text = element_text(colour = "white")) +
        theme(legend.position="bottom") +
        xlab("Day of the Year") +
        ylab("Estimated Count")
      
  })
  
  # model output for fitted emergence
  
  
  output$MosEmergence <- renderPlot(bg = "transparent",{
    
    MO <- ModelOutputs()
    
    # str(MO)
    
    ggplot() +
      geom_line(data = MO$FittedEmergencePoints, aes(x = DayOfYear, y = EmergenceRate, color = "Best Fit Emergence Model"),  size = 1.5) +
      geom_line(data = MO$ApproximatedEmergenceRate, aes(x = FittedTime, y = ApproxEmergence, color = "Fitted Emergence Function Fourier Approximation"), size = 1.5 ) +
      geom_point(data = MO$FittedEmergencePoints, aes(x = DayOfYear, y = EmergenceRate, color = "Mean Count"), alpha = .75) +
      scale_color_manual(values = c("#1b9e77","#225ea8","#ffffff")) +
      theme_minimal() +
      theme(panel.grid  =  element_line(colour = "dark grey"),
            text = element_text(colour = "white"),
            axis.text = element_text(colour = "white")) +
      theme(legend.position="bottom") +
      xlab("Day of the Year") +
      ylab("Emergence Rate")
    
  })
   
  ################################
  #### optmization model #########
  ################################  
  
  OptModel <- reactive({
    
  
    #  max number of dynamics fourier modes to use
    #  in calculating fourier sum 
    #  (different than N_lam = max emergence fourier mode
    #  set by user for curve fitting portion of the code.)
    #  kmax should be an integer between 2 and 200, defualt at 20

    kmax <- input$Kmax 
    
    
    # This variable lets the user select between local optimization
    # and two global optimization schemes.  Local optimization 
    # (global_opt = 0) is MUCH MUCH faster than the global optimizations
    # and tends to work well enough, but is not guaranteed to give the
    # actual optimal control protocol.  For global_opt = 1, the code
    # uses the "directL" global optimization.  This is the faster of
    # the two global options, but tends to be less accurate, and it 
    # can not take into account the minimum number of days between 
    # pulses specified by the user.  For global_opt = 2, the code 
    # uses the "ISRES" global optimization which is pretty slow, 
    # but tends to be more accurate, and can take into account 
    # the minimum number of days between pulses specified by the user. 
    
    global_opt <- input$global_opt
    
    # number of pulses (control activities)
    # set by user, integer between 1 and 10
    
    Npulse <- input$Npulse
    
    # percent knockdown (user set between .01 and 
    # .30, e.g. 1% to 30% knockdown)
    
    rho <- (input$rho)/100
    
    # minimum number of days allowed between 
    # pulses set by user (integer bewtween 0 and 30 days) 
    
    days_between <- input$days_between
    
    
    ########### repeated from curve fitting !!!!
    ########### should fix to only run once... 
    
    #
    tau_mosq <- input$MosLife  #where tau_mosq is the mosquito lifetime input by user in units of days 
    
    #mu = natural mosquito death rate
    mu <- 1 / tau_mosq #program should set mu = 1 / tau_mosq,
    #where tau_mosq is the mosquito lifetime input by user in units of days 
    
    #m = number of mosquito lifetimes for population decay between seasons..
    m <- input$MosDecay #this will be input by the user, and must be an integer greater than 2, upperbound 100, default value 3
    
    # N_lam = max fourier mode order to calculate
    
    N_lam <- input$JmaxOpt  #user input between 1 and 100, default value 25 (I referred to this as jmax over Skype - JD)
    
    print("N_lam created")
    print(N_lam)
    
    # day of the year 
    
    #get mean counts per day 
    
    print("summarizing data")
    
    InputData <- cleanData() %>% 
      dplyr::select(Count, DayOfYear) %>% 
      group_by(DayOfYear) %>% 
      summarize(MeanCount = mean(Count)) 
    
    InputData <- na.omit(InputData)
    
    str(InputData)
    
    ## can't handle more than 100 pts on my machine
    
    
    #first round user time inputs to integer values
    t_in <-  InputData$DayOfYear ## grab from data input
    
    y_in <- InputData$MeanCount
    
    #defining N0 = number of input data points (not a user input)
    N0 = length(y_in) # grab from dataset input
    
    #defining vector of time differences to be used later (not a user input)
    delta_t_in <- numeric(N0 - 1) 
    
    print("Defining Delta_t_in")
    
    for(i in 1:(N0 - 1)){
      delta_t_in[i] <- t_in[i+1] - t_in[i]
      if (delta_t_in[i] < 0)
        stop("non-increasing measurement times")
    }
    
    
    #N_pts = number of points in extended data set, N_pts = N0 + 3 always (not a user input)
    N_pts = N0 + 3
    
    
    #defining extended trap count and time data vectors
    t_dat = numeric(N_pts)
    
    
    print("Defining y_dat")
    
    if (t_in[1] - m / mu > 0){
      y_dat = c(0,0, y_in,0)
      
      t_dat[1] = 0
      t_dat[2] = (m -1) / mu
      for (i in 3 : (N_pts - 1)){
        t_dat[i] = t_in[i - 2] - t_in[1] + m / mu
      }
      t_dat[N_pts] = t_dat[N_pts - 1] + (m) / mu
      
      t_dat=matrix(t_dat, ncol=1)
      
    } else {
      y_dat = c(0, y_in, 0, 0)
      
      t_dat[1] = 0
      for (i in 2: (N_pts - 2)){
        t_dat[i] = t_in[i - 1]
      }
      t_dat[N_pts - 1] = t_dat[N_pts - 2] + (2*(m - 1)) / mu - t_dat[2]
      t_dat[N_pts] = t_dat[N_pts - 1] + 1 / mu
    }
    
    
    
    #extended vector of time differences
    delta_t_dat = numeric(N_pts)
    for (i in 1 : (N_pts - 1)){
      delta_t_dat[i] = t_dat[i + 1] - t_dat[i]
    }
    
    delta_t_dat[N_pts] = 0
    
    delta_t_dat= matrix(delta_t_dat,ncol=1)
    
    #tau = length of 'season' in days
    tau = t_dat[N_pts]
    
    
    #Defining matrices 'J' and 'M'
    
    #J matrix and inverse
    J = matrix(0, nrow= N_pts, ncol=N_pts)
    
    for (i in 1 : (N_pts - 1)){
      J[i, i] = 1
      J[i + 1, i] = -exp(-mu*delta_t_dat[i])
    }
    
    J[N_pts, N_pts] = 1
    J[1, N_pts] = -exp(-mu * delta_t_dat[N_pts])
    
    
    #M matrix and inverse
    M = matrix(0, nrow= N_pts, ncol=N_pts)
    
    for (i in 2 : (N_pts - 1)){
      M[i, i] <- (1 - exp( -mu * delta_t_dat[i - 1] ) ) /(mu * delta_t_dat[i - 1] ) - exp( -mu * delta_t_dat[i - 1])
      M[i + 1, i] <- 1 - (1 - exp(-mu * delta_t_dat[i]) )/(mu * delta_t_dat[i])
    }
    
    M[N_pts, N_pts] = (1 - exp( (-1)*mu * delta_t_dat[N_pts - 1] ) ) /(mu * delta_t_dat[(N_pts - 1)] ) -exp( -mu * delta_t_dat[(N_pts - 1)])
    M[1, N_pts] = 0
    M[1, 1] = 0
    M[2, 1] = 1- ( 1 - exp(-mu * delta_t_dat[1]) )/(mu * delta_t_dat[(1)]) 
    
    
    #Obtaining emergence rate vector lambda_dat by contrained optimization
    
    Aeq = matrix(numeric(N_pts), nrow=1)
    Aeq[(1)] = 1
    Aeq[(N_pts)] = -1
    beq = 0
    bineq = numeric(N_pts)
    lb = numeric(N_pts)
    ###################  
    
    
    objective_fun = function(x) {t(( (1 / mu) * (mldivide(J, (M %*% x), pinv = TRUE)) - y_dat))%*%( (1 / mu) * (mldivide(J, (M %*% x), pinv = TRUE)) - y_dat)}
    
    
    print(
      lapply(list(LengthJ = (J), lengthM = (M), lengthx = (N_pts)),FUN = length)
    )
    
    
    lambda_dat = fmincon(numeric(N_pts),
                         objective_fun,
                         gr = NULL,
                         method = "SQP",
                         A = (-(1 / mu) * ( inv(J) %*%  (M))), 
                         b = bineq,
                         Aeq = Aeq, beq = beq, lb = lb, ub = NULL)
    
    print("Lamba_dat created")
    #########
    
    
    #Calculating emergence rate fourier modes lam_fourier
    
    
    lam_fourier = numeric(N_lam + 1)
    
    
    
    for (j in 1 : (N_pts-1)){
      lam_fourier[1] =  lam_fourier[1] + (1 / tau)*( lambda_dat$par[j] + lambda_dat$par[j + 1] ) * delta_t_dat[j] / 2
    } #zero mode
    
    
    for (k in 2 : (N_lam + 1)) #non-zero postive modes (negative modes given by complex conjugates of positve modes) 
    {
      for (j in 1 : (N_pts - 1)){
        lam_fourier[(k)] = lam_fourier[(k)] +( ( lambda_dat$par[(j + 1)] - lambda_dat$par[(j)] )  / delta_t_dat[(j)] ) *( tau / (2 * pi * (k-1) ) )* ( ( exp(- 2 * pi * complex(real = 0, imaginary = 1) * (k-1) * t_dat[(j + 1)] / tau ) -exp(- 2 * pi * complex(real = 0, imaginary = 1) * (k-1) * t_dat[(j)]/ tau) ) / (2 * pi * (k-1)) )+ complex(real = 0, imaginary = 1) * ( lambda_dat$par[(j + 1)] * exp(- 2 * pi * complex(real = 0, imaginary = 1) * (k-1) * t_dat[(j + 1)] / tau ) -lambda_dat$par[(j)] * exp(- 2 * pi * complex(real = 0, imaginary = 1) * (k-1) * t_dat[(j)] /tau ) ) / (2 * pi * (k-1) )
      }
    }
    
    
    
    #code to execute optimization algorithms
    
    #install.packages("nloptr")
    library("nloptr") #used for local cobyla optimum function and global optimum functions
    
    if(Npulse == 1){ # 1 pulse
      fun1 = function(x)sum_1_pulse(x,rho, mu, tau, kmax, lam_fourier)
      guess = tau /2
      
      
      if(global_opt == 0){ #local optimum 
        opt_out = cobyla(guess, fun1, lower = 0, upper = tau)
        
        times = opt_out$par
        ave_pop_fourier = opt_out$value
        
      }else if(global_opt == 1){ #global optimum directL
        
        opt_out = directL(fun1, lower = 0, upper = tau, randomized = TRUE)
        
        times = opt_out$par
        ave_pop_fourier = opt_out$value
        
      }else if(global_opt == 2){ #global optimum isres
        
        
        opt_out = nloptr(x0=guess, eval_f=fun1, lb = 0, ub = tau, eval_g_ineq = NULL, eval_g_eq = NULL,                 opts = list(algorithm="NLOPT_GN_ISRES",maxeval=10000))
        
        times = opt_out$solution
        ave_pop_fourier = opt_out$objective
        
      }
      
      
    } else{ #2 or more pulses
      funN = function(x)sum_N_pulse(x,rho, Npulse, mu, tau, kmax, lam_fourier) 
      
      #matrix and vector for inequality constraints to enforce minimum days between
      A_mat = matrix(0,nrow=Npulse - 1, ncol=Npulse)
      b_vec = (-1) * days_between * matrix(1, Npulse-1, 1)
      
      for (i in 1:(Npulse -1)){
        A_mat[i,i] = 1
        A_mat[i, i+ 1] = -1
      }
      
      
      l_bound = numeric(Npulse) #column vector of zeros for pulse lower bound
      u_bound = tau * matrix(1,Npulse, 1) #column vector of taus for pulse upper bound
      
      guess = numeric(Npulse); #vector for initial guesses for fmincon
      
      for(i in 1:Npulse){
        if(t_in[1] - m / mu > 0 ){
          
          guess[i] = i * (t_dat[N_pts - 1] - t_dat[3]) / (Npulse + 1) + t_dat[3]
        }else{
          
          guess[i] = i * (t_dat[N_pts - 2] - t_dat[2]) / (Npulse + 1) + t_dat[2]
        }
      } 
      
      if(global_opt == 0){#local optimum  
        
        opt_out = fmincon(guess, funN, gr= NULL, method="SQP", A = A_mat, b=b_vec, Aeq = NULL, beq = NULL, lb = l_bound, ub = u_bound)
        
        
        times = opt_out$par
        ave_pop_fourier = opt_out$value
        
      }else if(global_opt == 1){ #global optimum directL
        
        opt_out = directL(funN, lower = l_bound, upper = u_bound, randomized = TRUE)
        
        times = opt_out$par
        ave_pop_fourier = opt_out$value
        
      }else if(global_opt == 2){ #global optimum isres
        
        ineq = function(x){(-1)*A_mat %*%x - b_vec}
        
        opt_out = nloptr(x0=guess, eval_f=funN, lb = l_bound, ub = u_bound, eval_g_ineq = ineq, eval_g_eq = NULL,                 opts = list(algorithm="NLOPT_GN_ISRES",maxeval=10000))
        
        times = opt_out$solution
        ave_pop_fourier = opt_out$objective
        
      }
    }
    
    
    #code to shift times and generate plot data
    
    #shift pulse times back to original times input by user
    if (t_in[1]- (m/mu)>0){ 
      pulse_times_output = times - t_dat[3] + t_in[1]
    }else {
      pulse_times_output = times - t_dat[2] + t_in[1]
    }
    
    
    
    
    #code to plot results:  
    t_steps = 6 * tau*10 + 1 # ten time steps per day, six total seasons (need to integrate over many seasons to reach periodic population curves)
    t_vec = linspace(0, 6*tau, n = t_steps); #list between 0 and 6*tau, t_steps long
    
    
    y_fourier_controlled = numeric(t_steps) #list (row vector) for controlled population values at each time step
    y_fourier_uncontrolled = numeric(t_steps) #list (row vector) for uncontrolled population values at each time step
    
    
    #simple integrator to calculate population values
    for (i in 2:t_steps){
      for (j in 1: N_pts - 1){
        if (mod(t_vec[i-1], tau) >= t_dat[j] && mod(t_vec[i-1],tau) < t_dat[j + 1]){
          y_fourier_controlled[i] = y_fourier_controlled[i-1] +(-mu * y_fourier_controlled[i-1] +lambda_dat$par[j+1] * (mod(t_vec[i-1], tau) -t_dat[j])/ delta_t_dat[j] +lambda_dat$par[j] * (t_dat[j+1] -mod(t_vec[i-1], tau)) / delta_t_dat[j])*(t_vec[i] - t_vec[i-1]);
          
          y_fourier_uncontrolled[i] = y_fourier_uncontrolled[i-1] +(-mu * y_fourier_uncontrolled[i-1] +lambda_dat$par[j+1] * (mod(t_vec[i-1], tau) -t_dat[j])/ delta_t_dat[j] +lambda_dat$par[j] * (t_dat[j+1] -mod(t_vec[i-1], tau)) / delta_t_dat[j])*(t_vec[i] - t_vec[i-1])
        }
        
      }
      
      #impulses for the controlled population
      for (j in 1:Npulse){
        if (mod(t_vec[i], tau) >= times[j] && mod(t_vec[i-1], tau) < times[j]){
          y_fourier_controlled[i] = (1-rho) * y_fourier_controlled[i];
        }
      }
      
    }
    
    pop_cont = numeric(10*tau+1) #list for controlled population vlaues, one season long
    message("pop cont created")
    pop_un_cont = numeric(10*tau+1) #list for uncontrolled population vlaues, one season long 
    
    str(pop_un_cont)
    t_vec_plot = numeric(10*tau+1) #list for time values, one season long
    
    for (i in 1:(10*tau+1)){
      pop_cont[i] = y_fourier_controlled[i + 10*tau*5] #take controlled population values from the integration for the final season
      pop_un_cont[i] = y_fourier_uncontrolled[i+ 10*tau*5] #take uncontrolled population values from the integration for the final season
      
      #shift plot times for population curves back to original times input by user
      if (t_in[1] - m / mu > 0) {
        t_vec_plot[i] = t_vec[i]-t_dat[(3)] +t_in[(1)]
        
      } else{
        t_vec_plot[i] = t_vec[i] -t_dat[(2)] + t_in[(1)]
      }
      
      
    }
    
    
    #shift data times back to orginal times input by user
    if (t_in[(1)] - (m / mu) > 0){ 
      t_dat_plot = t_dat  - t_dat[(3)] + t_in[(1)]
    }else {
      t_dat_plot = t_dat  - t_dat[2] + t_in[1]
    }
    
    
    #code to generate outputs to user
    
    
    ave_pop_un_cont = integrate.xy(t_vec_plot, pop_un_cont)/(tau)
    ave_pop_cont = integrate.xy(t_vec_plot, pop_cont)/(tau)
    
    percent_reduction = (ave_pop_un_cont - ave_pop_cont) / ave_pop_un_cont
    accuracy_measure = (ave_pop_fourier - ave_pop_cont) / ave_pop_cont
    
    
    # create a list for outputs and render them
    
    #outputs to display to user
    
    #print object list
    
    PrintObj <- list(
      pulse_times_output = pulse_times_output, #times of optimal pulses in units of day of year
      ave_pop_un_cont = ave_pop_un_cont, #average population over interval (0, tau) under uncontrolledfourier dynamics
      ave_pop_cont = ave_pop_cont, #average population over interval (0, tau) under controlled fourier dynamics
      percent_reduction = percent_reduction, #fractional reduction in average controlled population relative to uncontrolled 
      accuracy_measure = accuracy_measure #reliability measuere - if the magnitude of this quantity is larger than .05 or .1 or so, then the fourier sum may be of unrelaiable accuracy, and the user should try increasing kmax
      
    )
    
    
    # 
    # #plots to display to user
    # plot(t_dat_plot, y_dat, ylim = c(0,1.5 * max(y_dat)), xlim=c(t_dat_plot[1],t_dat_plot[1]+tau), col="blue",
    #      main="Fitted Population Model",
    #      ylab="Mosquito population count",
    #      xlab="Day of year")
    # lines(t_vec_plot, pop_un_cont, col="red")
    # lines(t_vec_plot, pop_cont, col = "orange")
    # legend("topleft",c("Data","Uncontrolled Fourier Approximation", "Controlled Fourier Approximation"),fill=c("blue","red","orange"))
    # 
    
    PlotObject <- list(
      #point dataframe
      df_points = data.frame(
        t_dat_plot = t_dat_plot,
        y_dat = y_dat), 
      #line dataframe
      df_lines = data.frame(
        t_vec_plot =  t_vec_plot,
        pop_un_cont = pop_un_cont,
        pop_cont = pop_cont
        ),
    #limits
    ylim = c(0,1.5 * max(y_dat)),
    xlim = c(t_dat_plot[1],t_dat_plot[1] + tau),
    ## labels
    title = "Fitted Population Model", 
    ylab="Mosquito population count",
    xlab="Day of year",
    ## legend data
    LegendData = "Data", 
    LegendUnCont = "Uncontrolled Fourier Approximation", 
    LgendCont = "Controlled Fourier Approximation"
    )
    
    # print("Plot Object")
    # str(PlotObject)
    
    OptOutPutObject <- list(PlotObject = PlotObject, PrintObj = PrintObj)
    
   
    
  }) 
  

  #render plots
  
  output$OptModel <- renderPlot(bg = "transparent",{
    
    OptModel <-   OptModel()$PlotObject
     
     ggplot() +
       geom_line(data = OptModel$df_lines, aes(x = t_vec_plot, y = pop_un_cont, color = "Uncontrolled Population"), size = 1.5 ) +
       geom_line(data = OptModel$df_lines, aes(x = t_vec_plot, y = pop_cont, color = "Controlled Population"), size = 1.5 ) +
       geom_point(data = OptModel$df_points, aes(x = t_dat_plot, y = y_dat, color = "Mean Count"), alpha = .75) +
       scale_color_manual(values = c("#1b9e77","#ffffff","#d95f02")) +
       theme_minimal() +
       theme(panel.grid  =  element_line(colour = "dark grey"),
             text = element_text(colour = "white"),
             axis.text = element_text(colour = "white")) +
       theme(legend.position ="bottom") +
       xlab(OptModel$xlab) +
       ylab(OptModel$ylab) 
     
  
    })
  

  
  #render text
  output$PulseTimes <- renderText({
    
    PrintObj <- OptModel()$PrintObj$pulse_times_output
    
    
    paste(round(PrintObj,0),collapse = ", ")

  })
  
  
  output$AvePopUnCont <- renderText({
    
    PrintObj <- OptModel()$PrintObj$ave_pop_un_cont
    
    round(PrintObj,3)
    
  })  
  
  output$AvePopCont <- renderText({
    
    PrintObj <- OptModel()$PrintObj$ave_pop_cont
    
    round(PrintObj,3)
    
  })  
  
  
  output$percent_reduction <- renderText({
    
    PrintObj <- OptModel()$PrintObj$percent_reduction
    
    paste0(round(100*PrintObj,3)," %")
    
  })
  
  
  output$accuracy_measure <- renderText({
    
    PrintObj <- OptModel()$PrintObj$accuracy_measure
    
    PrintObj <- round(PrintObj,4)
    
    if ( PrintObj > .1) {
      paste(PrintObj,"/nWarning: increase Kmax to improve accuracy")
    } else (
      PrintObj
      )
    
  })

    
  
  
  
}

shinyApp(ui, server)


