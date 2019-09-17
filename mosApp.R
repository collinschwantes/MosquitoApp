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
      # selectInput("TrapType", "Trap Type:",
      #             c("Oviposition" = "ovi",
      #               "Light" = "lig",
      #               "CO2" = "CO2  ")),
      selectInput("Count", "Count Data:",
                  c("No data uploaded data" = "NoData")
                  ),
      selectInput("Date", "Date:",
                  c("No data uploaded data" = "NoData")
                  ),
      # selectInput("Species", "Species:",
      #             c("No data uploaded data" = "NoData")
      # ),
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
                 fluidRow(
                  column(width = 6, sliderInput(inputId = "MosLife",label = "Mosquito Lifespan in Days",min = 1,max = 30,value = 3,step = 1)),
                  column(width = 6, sliderInput(inputId = "MosDecay",label = "Mosquito Lifecycles Between Seasons",min = 2,max = 100,value = 3,step = 1))
                 ),
                 hr(),
                 fluidRow(
                   column(width = 6, h4("Fitted Population Model", style = "text-align: center;"), plotOutput("MosPopFitted")),
                   column(width = 6, h4("Fitted Emergence Rate", style = "text-align: center;"), plotOutput("MosEmergence"))
                   
                 )
                 
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
    
    # updateSelectInput(session, "Species",
    #                   label = "Species Column",
    #                   choices = ColNames,
    #                   selected = ""
    # )
# 
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
      y_dat = c(0, y_in, 0)
      
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
      
    }
    
    
    y_uncontrolled_plot = numeric(10 * tau + 1)
    lambda_fourier_plot = numeric(10 * tau + 1)
    t_vec_plot = numeric(10 * tau + 1)
    
    for( i in 1 : (10*tau+1)){
      
      
      y_uncontrolled_plot[i] = y_fourier_uncontrolled[(i + 10*tau*5)]
      lambda_fourier_plot[(i)] = lambda_fourier_function[(i+ 10*tau*5)]
      
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
                                   "FittedPop" = as.numeric(y_uncontrolled_plot), # convertfor plotting
                                   stringsAsFactors = F),
    
    #for emergence model
    "FittedEmergencePoints" = data.frame("DayOfYear" = t_dat_plot,
                                        "EmergenceRate" =  as.numeric(lambda_dat$par)),
    
   "ApproximatedEmergenceRate" = data.frame("FittedTime" = t_vec_plot,
                                            "ApproxEmergence" = as.numeric(lambda_fourier_plot))
    )
    
  })
  
  # model output for Fitted Pops
  
  output$MosPopFitted <- renderPlot(bg = "transparent",{
    
      MO <- ModelOutputs()
      
      # str(MO)
      
      ggplot() +
         geom_line(data = MO$FittedPopModel, aes(x = FittedTime, y = FittedPop, color = "Fitted Population"), size = 1.5 ) +
        geom_point(data = MO$SummarizedPopData, aes(x = DayOfYear, y = MeanMosPop, color = "Mean Count"), alpha = .75) +
        scale_color_manual(values = c("#225ea8","#ffffff")) +
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
      geom_line(data = MO$ApproximatedEmergenceRate, aes(x = FittedTime, y = ApproxEmergence, color = "Emergence Approximation"), size = 1.5 ) +
      geom_point(data = MO$FittedEmergencePoints, aes(x = DayOfYear, y = EmergenceRate, color = "Mean Count"), alpha = .75) +
      scale_color_manual(values = c("#c7e9b4","#ffffff")) +
      theme_minimal() +
      theme(panel.grid  =  element_line(colour = "dark grey"),
            text = element_text(colour = "white"),
            axis.text = element_text(colour = "white")) +
      theme(legend.position="bottom") +
      xlab("Day of the Year") +
      ylab("Emergence Rate")
    
  })
   
}

shinyApp(ui, server)


