library(shiny)
library(DT)
library(plotly)
library(googleVis)
library(sunburstR)

source("./preprocess.R")
source("./randomForest.R")

shinyServer(function(input, output, session) {
  
  # choices for filters
  stateChoices <- sort(unique(as.character(newData$State)))
  mobileChoices <- sort(unique(as.character(newData$DeviceIsMobile)))
  browserChoices <- sort(unique(as.character(newData$Browser)))
  resolutionChoices <- unique(as.character(newData$Resolution))
  deviceBrandChoices <- unique(as.character(newData$DeviceBrand))
  
  # reactive function to filter out data based on user input
  filterData <- reactive({
    input$go
    isolate({
      if(input$go < 1) {
        filteredData <- newData
      } else {
        filteredData <- newData[newData$State %in% input$state &
                                  newData$DeviceIsMobile %in% input$isMobile &
                                  newData$Browser %in% input$browser &
                                  newData$Resolution %in% input$resolution &
                                  newData$DeviceBrand %in% input$deviceBrand, ]
        
        validate(
          need(length(input$state) > 0,
               "Please select more than one state"),
          need(length(input$isMobile) > 0,
               "Please select more than one mobile options"),
          need(length(input$browser) > 0,
               "Please select more than one browsers"),
          need(length(input$resolution) > 0,
               "Please select more than one resolutions"),
          need(length(input$deviceBrand) > 0,
               "Please select more than one device brands"),
          need(nrow(filteredData) > 0,
               paste0("Not sufficient Data available."))
        )
      }
    })
    return(filteredData)
  })
  
  # get reactive state options
  output$stateCheckboxGroup <- renderUI({
    tags$div(class = "multicol",
             checkboxGroupInput("state", "States:",
                                choices  = stateChoices,
                                selected = stateChoices)
    )
  })
  
  # get reactive mobile options
  output$mobileCheckboxGroup <- renderUI({
    checkboxGroupInput("isMobile", "Mobile Options:",
                                choices  = mobileChoices,
                                selected = mobileChoices)
  })
  
  # get reactive device brand options
  output$deviceBrandCheckboxGroup <- renderUI({
    checkboxGroupInput("deviceBrand", "Device Brand:",
                       choices  = deviceBrandChoices,
                       selected = deviceBrandChoices)
  })
  
  # get reactive browser choices
  output$browserCheckboxGroup <- renderUI({
    checkboxGroupInput("browser", "Browser:",
                       choices  = browserChoices,
                       selected = browserChoices)
  })
  
  # get reactive resolution choices
  output$resolutionCheckboxGroup <- renderUI({
    checkboxGroupInput("resolution", "Resolution:",
                       choices  = resolutionChoices,
                       selected = resolutionChoices)
  })
  
  # observer function for states
  obsStates <- observe({
    if(input$stateSelectAll > 0) {
      if(input$stateSelectAll %% 2 == 0) {
        updateCheckboxGroupInput(session = session,
                                 inputId = "state",
                                 choices = stateChoices,
                                 selected = stateChoices)
        
      } else {
        updateCheckboxGroupInput(session = session,
                                 inputId = "state",
                                 choices = stateChoices,
                                 selected = c())
      }
    }
  })
  
  # observer function for device brands
  obsDeviceBrand <- observe({
    if(input$deviceBrandSelectAll > 0) {
      if(input$deviceBrandSelectAll %% 2 == 0) {
        updateCheckboxGroupInput(session = session,
                                 inputId = "deviceBrand",
                                 choices = deviceBrandChoices,
                                 selected = deviceBrandChoices)
        
      } else {
        updateCheckboxGroupInput(session = session,
                                 inputId = "deviceBrand",
                                 choices = deviceBrandChoices,
                                 selected = c())
      }
    }
  })
  
  # observer function for mobile
  obsMobile <- observe({
    if(input$mobileSelectAll > 0) {
      if(input$mobileSelectAll %% 2 == 0) {
        updateCheckboxGroupInput(session = session,
                                 inputId = "isMobile",
                                 choices = mobileChoices,
                                 selected = mobileChoices)
        
      } else {
        updateCheckboxGroupInput(session = session,
                                 inputId = "isMobile",
                                 choices = mobileChoices,
                                 selected = c())
      }
    }
  })
  
  # observer functions for browser choices
  obsBrowser <- observe({
    if(input$browserSelectAll > 0) {
      if(input$browserSelectAll %% 2 == 0) {
        updateCheckboxGroupInput(session = session,
                                 inputId = "browser",
                                 choices = browserChoices,
                                 selected = browserChoices)
        
      } else {
        updateCheckboxGroupInput(session = session,
                                 inputId = "browser",
                                 choices = browserChoices,
                                 selected = c())
      }
    }
  })
  
  # observer functions for resolution choices
  obsResolution <- observe({
    if(input$resolutionSelectAll > 0) {
      if(input$resolutionSelectAll %% 2 == 0) {
        updateCheckboxGroupInput(session = session,
                                 inputId = "resolution",
                                 choices = resolutionChoices,
                                 selected = resolutionChoices)
        
      } else {
        updateCheckboxGroupInput(session = session,
                                 inputId = "resolution",
                                 choices = resolutionChoices,
                                 selected = c())
      }
    }
  })
  
  output$predictionTable <- renderDataTable({
    
    # removing columns which are not to be displayed
    final.test$UserAgentString <- NULL
    final.test$PreQuotePortal <- NULL
    final.test$QuoteStart <- NULL
    final.test$RetrievePremium <- NULL
    final.test$BindStart <- NULL
    final.test$AddDrivers <- NULL
    final.test$AddVehicles <- NULL
    final.test$ChooseCoverage <- NULL
    final.test$GetPremium <- NULL
    final.test$PaymentStart <- NULL
    final.test$PaymentComplete <- NULL
    final.test$BrowserVersion <- NULL
    final.test$EventType <- NULL
    colnames(final.test) <- c("State", 
                              "Browser",
                              "Mobile Device",
                              "Device Brand",
                              "Resolution",
                              "Duration (in minutes)",
                              "Time of Day (24-hr)",
                              "Day of Week",
                              "Prediction")
    
    input$go
    
    isolate({
      if(input$go < 1) {
        filteredData <- final.test
      } else {
        filteredData <- final.test[final.test$State %in% input$state &
                                     final.test$`Mobile Device` %in% 
                                     input$isMobile &
                                     final.test$Browser %in% input$browser &
                                     final.test$Resolution %in% 
                                     input$resolution &
                                     final.test$`Device Brand` %in% 
                                     input$deviceBrand, ]
        
        validate(
          need(length(input$state) > 0,
               "Please select more than one state"),
          need(length(input$isMobile) > 0,
               "Please select more than one mobile options"),
          need(length(input$browser) > 0,
               "Please select more than one browsers"),
          need(length(input$resolution) > 0,
               "Please select more than one resolutions"),
          need(length(input$deviceBrand) > 0,
               "Please select more than one device brands"),
          need(nrow(filteredData) > 0,
               paste0("Not sufficient Data available."))
        )
      }
    })
    datatable(filteredData,
              options = list(searching = FALSE,
                             paging = TRUE,
                             info = FALSE,
                             ordering = TRUE),
              rownames = FALSE) %>% formatPercentage(ncol(final.test), 2)
  })
  
  output$bubbleChartDayOfWeek <- renderPlotly({
    # get filtered data
    filteredData <- filterData()
    
    # get only those columns which are required
    summaryData <- filteredData[, c("UserId", "Duration",
                               "TimeOfDay", "DayOfWeek", "PaymentComplete")]

    # set levels for days of the week
    summaryData$DayOfWeek <- factor(summaryData$DayOfWeek,
                                    levels = c("Sunday",
                                               "Monday",
                                               "Tuesday",
                                               "Wednesday",
                                               "Thursday",
                                               "Friday",
                                               "Saturday"))
  
    # make the opacity as 1 if policy is sold
    opacity <- ifelse(summaryData$PaymentComplete == "0", 0.1, 1)
    
    # remove payment complete column since it is no longer required
    summaryData$PaymentComplete <- NULL

    # plot the bubble chart
    p <- plot_ly(summaryData,
                 x = ~DayOfWeek,
                 y = ~TimeOfDay,
                 size = ~Duration,
                 type = "scatter",
                 mode = "markers",
                 sizes = c(as.numeric(min(summaryData$Duration)),
                           as.numeric(max(summaryData$Duration))),
                 marker = list(size = ~Duration/100,
                               opacity = opacity),
                 hoverinfo = 'text',
                 text = ~paste(Duration, 'minutes')) %>%
      layout(title = 'Day of Week vs Time of Day vs Duration (in minutes)',
             xaxis = list(showgrid = FALSE,
                          title = "Day Of Week"),
             yaxis = list(showgrid = FALSE,
                          range = c(0, 24),
                          title = "Time of Day")) 
  })
  
  output$bubbleChartDate <- renderPlotly({
    # get filtered data
    filteredData <- filterData()

    # get only those columns which are required
    summaryData <- filteredData[, c("UserId", "Date", "Duration",
                               "TimeOfDay", "PaymentComplete")]
    
    # make the opacity as 1 if policy is sold
    opacity <- ifelse(summaryData$PaymentComplete == "0", 0.1, 1)
    
    # remove payment complete column since it is no longer required
    summaryData$PaymentComplete <- NULL
    
    # plot the bubble chart
    p <- plot_ly(summaryData,
                 x = ~Date,
                 y = ~TimeOfDay,
                 size = ~Duration,
                 type = "scatter",
                 mode = "markers",
                 sizes = c(as.numeric(min(summaryData$Duration)),
                                    as.numeric(max(summaryData$Duration))),
                 marker = list(size = ~Duration/100,
                               opacity = opacity),
                 hoverinfo = 'text',
                 text = ~paste(Duration, 'minutes')) %>%
      layout(title = 'Date vs Time of Day vs Duration (in minutes)',
             xaxis = list(showgrid = FALSE,
                          range = c(as.numeric(as.POSIXct(
                            min(summaryData$Date), format="%Y-%m-%d"))*1000,
                                    as.numeric(as.POSIXct(
                                      max(summaryData$Date),
                                      format="%Y-%m-%d"))*1000),
                          type = "date",
                          title = "Date"),
             yaxis = list(showgrid = FALSE,
                          range = c(0, 24),
                          title = "Time of Day"))
  })
  
  output$sankeyDiagram <- renderGvis({
    # get filtered data
    filteredData <- filterData()
    
    # make a new data frame with values for number of sessions, when a user
    # passes from one event to another
    dat <- data.frame(From = c("Pre-Quote Portal",
                               "New to Quote Start",
                               "Quote Start",
                               "New to Retrieve Premium",
                               "Retrieve Premium",
                               "New to Bind Start",
                               "Bind Start",
                               "New to Payment Start",
                               "Payment Start",
                               "New to Payment Complete",
                               "Pre-Quote Portal",
                               "Quote Start",
                               "Retrieve Premium",
                               "Bind Start",
                               "Payment Start"),
                      To = c("Quote Start",
                             "Quote Start",
                             "Retrieve Premium",
                             "Retrieve Premium",
                             "Bind Start",
                             "Bind Start",
                             "Payment Start",
                             "Payment Start",
                             "Payment Complete",
                             "Payment Complete",
                             "Dropped After Pre-Quote Portal",
                             "Dropped After Quote Start",
                             "Dropped After Retrieve Premium",
                             "Dropped After Bind Start",
                             "Dropped After Payment Start"),
                      Sessions = c(nrow(filteredData[filteredData$PreQuotePortal
                                                     == 1, ]),
                                 nrow(subset(filteredData, 
                                             filteredData$PreQuotePortal == 0 & 
                                               filteredData$QuoteStart == 1)),
                                 nrow(filteredData[filteredData$QuoteStart
                                                   == 1, ]),
                                 nrow(subset(filteredData,
                                             filteredData$QuoteStart == 0 &
                                               filteredData$RetrievePremium 
                                             == 1)),
                                 nrow(filteredData[filteredData$RetrievePremium 
                                                   == 1, ]),
                                 nrow(subset(filteredData,
                                             filteredData$RetrievePremium == 0 &
                                               filteredData$BindStart == 1)),
                                 nrow(filteredData[filteredData$BindStart 
                                                   == 1, ]),
                                 nrow(subset(filteredData,
                                             filteredData$BindStart == 0 &
                                               filteredData$PaymentStart == 1)),
                                 nrow(filteredData[filteredData$PaymentStart == 
                                                     1, ]),
                                 nrow(subset(filteredData,
                                             filteredData$PaymentStart == 0 &
                                               filteredData$PaymentComplete == 
                                               1)),
                                 nrow(filteredData[filteredData$PreQuotePortal 
                                                   == 0, ]),
                                 nrow(filteredData[filteredData$QuoteStart 
                                                   == 0, ]),
                                 nrow(filteredData[filteredData$RetrievePremium
                                                   == 0, ]),
                                 nrow(filteredData[filteredData$BindStart 
                                                   == 0, ]),
                                 nrow(filteredData[filteredData$PaymentStart 
                                                   == 0, ])
                                ))
    
    # plot the sankey diagram
    sankeyPlot <- gvisSankey(dat, from="From", to="To", weight="Sessions",
                      options = list(width = "automatic", height = "400"))
  
    return(sankeyPlot)
  })
  
  output$sunburstPlot <- renderSunburst({
    return(sunburst(sequencesCount))
  })
  
  # suspend the observer functions when session is ended
  session$onSessionEnded(function() {
    obsStates$suspend()
    obsMobile$suspend()
    obsBrowser$suspend()
    obsResolution$suspend()
    obsDeviceBrand$suspend()
  })
})
