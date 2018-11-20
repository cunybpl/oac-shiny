#Outside Air Control Interactive Plotting 
#Daniel Vignoles, Mark Campmier

#***********This application assumes all .csv's are retrieved/exported through HOBOware***********

#------------TODOS------------#

#TODO: Detect fahrenheit / Celsius based on header column name, ie: "Temp, (*F)"

#TODO: Read/save serial number from first line of file (temperature loggers)

#TODO: Fan Status: extend trend line to beginning and end of graph when dateRange cuts off values
#disable trends in time ranges where fan-status is off

#TODO: Occupancy Scheduling

#TODO: display serial number / date-range under file upload input

#TODO: Informative Splash screen regarding App Assumptions / instructions in Plot window perior to plotting

#-------------------------------#

#Required Packages
library(shiny)
library(xts)
library(reshape)
library(lubridate)
library(plotly)
library(DT)
library(shinyjs)
source("data_prep.R")

ui <- fluidPage( 
  useShinyjs(),
  titlePanel("Outside Air Control"),
  
  sidebarLayout(position="left", sidebarPanel(
    
    div(id = "noBAS Visualization",
        
        #DAT file input
        fileInput("datFile", "Choose CSV File for Discharge Air Temperature (DAT)",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")
        ),
        textOutput('DAT_serial'), #TODO
        textOutput('DAT_dates'), #TODO
        # conditionalPanel(condition = "output.datUploaded",
        #                  checkboxInput("DATCheckbox", label = "Display DAT Plot.", value = TRUE)
        # ),
        
        #MAT file input
        fileInput("matFile", "Choose CSV File for Mixed Air Temperature (MAT)",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")
        ), 
        # conditionalPanel(condition = "output.matUploaded",
        #                  checkboxInput("MATCheckbox", label = "Display MAT Plot", value = TRUE)
        # ),
        
        #OAT file input
        fileInput("oatFile", "Choose CSV File for Outside Air Temperature (OAT)",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")
        ),
        # conditionalPanel(condition = "output.oatUploaded",
        #                  checkboxInput("OATCheckbox", label = "Display OAT Plot.", value = TRUE)
        # ),
        
        #RAT file input
        fileInput("ratFile", "Choose CSV File for Return Air Temperature (RAT)",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")
        ),
        # conditionalPanel(condition = "output.ratUploaded",
        #                  checkboxInput("RATCheckbox", label = "Display RAT Plot. ", value = TRUE)
        # ),
        
        #Fan Status file input
        fileInput("fanFile", "Choose CSV File for Fan Status",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")
        ),
        # conditionalPanel(condition = "output.fanUploaded",
        #                  checkboxInput("fan_statusCheckbox", label = "Display Fan Status.", value = TRUE)
        # ),
        #Date Range Selection
        uiOutput("date_range"),
        actionButton("update_plot",label = "Update Plot!", width = '100%')
        
    )),
    
    mainPanel(
      #Plot output
      plotlyOutput("plot",height = '500px'),
      
      #Data Table Output
      conditionalPanel(condition = "output.show_Plot_Options",
                       
                       div(style="display: inline-block;vertical-align:top; width: 160px;",
                           checkboxInput("DATCheckbox", label = "DAT", value = TRUE)
                       ),
                       div(style="display: inline-block;vertical-align:top; width: 160px;",
                           checkboxInput("MATCheckbox", label = "MAT", value = TRUE)
                       ),
                       div(style="display: inline-block;vertical-align:top; width: 160px;",
                           checkboxInput("OATCheckbox", label = "OAT", value = TRUE)
                       ),
                       div(style="display: inline-block;vertical-align:top; width: 160px;",
                           checkboxInput("RATCheckbox", label = "RAT", value = TRUE)
                       ),
                       div(style="display: inline-block;vertical-align:top; width: 160px;",
                           checkboxInput("fan_statusCheckbox", label = "FAN", value = TRUE)
                       ),
                       br(),br(),br(),
                       
                       actionButton("update_plot2", label = "Update Plot!", width = 800),
                       checkboxInput("showTable", label = "Display Data Table", value = FALSE)
      ),
      br(),
      dataTableOutput("table")
    )
  )
)

server <- function(input, output) {
  
  #Check if individual files are uploaded, used in conditionalPanel UI
  output$datUploaded <- reactive({
    return(!is.null(input$datFile))
  })
  outputOptions(output, 'datUploaded', suspendWhenHidden=FALSE)
  
  
  output$matUploaded <- reactive({
    return(!is.null(input$matFile))
  })
  outputOptions(output,'matUploaded',suspendWhenHidden=FALSE)
  
  output$oatUploaded <- reactive({
    return(!is.null(input$oatFile))
  })
  outputOptions(output,'oatUploaded',suspendWhenHidden=FALSE)
  
  output$ratUploaded <- reactive({
    return(!is.null(input$ratFile))
  })
  outputOptions(output,'ratUploaded',suspendWhenHidden=FALSE)
  
  output$fanUploaded <- reactive({
    return(!is.null(input$fanFile))
  })
  outputOptions(output,'fanUploaded',suspendWhenHidden=FALSE)
  
  #dataUploaded() returns TRUE/FALSE for whether there is data to display
  dataUploaded <- reactive({
    datNull <- is.null(input$datFile)
    matNull <- is.null(input$matFile)
    oatNull <- is.null(input$oatFile)
    ratNull <- is.null(input$ratFile)
    
    isThereData <- !datNull || !matNull || !oatNull || !ratNull
  })
  
  #combine update buttons
  updatePlot <- reactive({
    input$update_plot + input$update_plot2
  })
  
  #Determine whether or not to display "Display Data Table" checkbox, used in Conditional UI Panel
  output$show_Plot_Options <- eventReactive(updatePlot(),{
    dataUploaded()
  })
  outputOptions(output,'show_Plot_Options',suspendWhenHidden=FALSE)
  
  #Retrieve processed DAT xts object
  datData <- reactive({
    if(!is.null(input$datFile)){
      d1 <- data_sorter(input$datFile$datapath)
      colnames(d1) <- "DAT"
      return(d1)
    }
    else{
      return (NULL)
    }
  })
  
  #Retrieve processed MAT xts object
  matData <- reactive({
    if(!is.null(input$matFile)){
      d1 <- data_sorter(input$matFile$datapath)
      colnames(d1) <- "MAT"
      return(d1)
    }
    else{
      return (NULL)
    }
  })
  
  #Retrieve processed OAT xts object
  oatData <- reactive({
    if(!is.null(input$oatFile)){
      d1 <- data_sorter(input$oatFile$datapath)
      colnames(d1) <- "OAT"
      return(d1)
    }
    else{
      return (NULL)
    }
  })
  
  #Retrieve processed RAT xts object
  ratData <- reactive({
    if(!is.null(input$ratFile)){
      d1 <- data_sorter(input$ratFile$datapath)
      colnames(d1) <- "RAT"
      return(d1)
    }
    else{
      return (NULL)
    }
  })
  
  #Retrieve processed fan_status xts object
  fanData <- reactive({
    if(!is.null(input$fanFile)){
      d1 <- fan_data_sorter(input$fanFile$datapath)
      colnames(d1) <- 'fan_status'
      return(d1)
    }
  })
  
  #Disable/Enable Plot Checkboxes
  disable('DATCheckbox')
  observeEvent(datData(),enable('DATCheckbox'))
  
  disable('MATCheckbox')
  observeEvent(matData(),enable('MATCheckbox'))
  
  disable('OATCheckbox')
  observeEvent(oatData(),enable('OATCheckbox'))
  
  disable('RATCheckbox')
  observeEvent(ratData(),enable('RATCheckbox'))
  
  disable('fan_statusCheckbox')
  observeEvent(fanData(),enable('fan_statusCheckbox'))
  
  #detect when only one temperature logger file is present, returns dat/mat etc string if true for that file
  onlyOne <- reactive({
    datNull <- is.null(input$datFile)
    matNull <- is.null(input$matFile)
    oatNull <- is.null(input$oatFile)
    ratNull <- is.null(input$ratFile)
    
    #TODO: prettify this code
    if(datNull == FALSE && (matNull == TRUE) && (oatNull == TRUE) && (ratNull == TRUE)){
      "dat"
    }
    
    else if(matNull == FALSE && (datNull == TRUE) && (oatNull == TRUE) && (ratNull == TRUE)){
      "mat"
    }
    
    else if(oatNull == FALSE && (matNull == TRUE) && (datNull == TRUE) && (ratNull == TRUE)){
      "oat"
    }
    
    else if(ratNull == FALSE && (matNull == TRUE) && (oatNull == TRUE) && (datNull == TRUE)){
      "rat"
    }
    else{
      onlyOne <- FALSE
    }
  })
  
  #available date range of all uploaded files
  dateRange <- reactive({
    
    if(dataUploaded()){
      min <- NA
      max <- NA
      
      all_data <- c(datData(), matData(), oatData(), ratData(), fanData())
      
      for(data in all_data){
        data <- na.trim(data, is.na = 'all')
        
        start <- head(index(data),1)
        start <- as.Date(start)
        end <- tail(index(data),1)
        end <- as.Date(end)
        
        if(is.na(min)){
          min <- start
        }else{
          if(start < min){
            min <- start
          }
        }
        
        if(is.na(max)){
          max <- end
        }else{
          if(end > max){
            max <- end
          }
        }
      }
      return(c(min,max))
    }
  })
  
  #UI output to enter Date Range, restricted to dates available within available files
  output$date_range <- renderUI({
    dateRangeInput("date_range","Range of Dates to Plot",start = dateRange()[1], end =dateRange()[2],  min = dateRange()[1], max = dateRange()[2])
  })
  
  #Merge available xts objects
  getData <- reactive({
    if(!dataUploaded()){
      return (NULL)
    }
    else{
      #If multiple files, merge into one XTS table
      #TODO : Rework error-handling method with trycatch()(I'm new to this)
      
      #If first arguement of merge() is NULL, an error is thrown. 
      #The purpose of this block is to make sure any NULL values are not the first argument
      if(onlyOne() == FALSE){
        
        t1 <- try(d <- merge(datData(),matData(),oatData(),ratData()))
        if(class(t1) != "try-error"){
          return (t1)
        }
        
        t1 <-try(d <- merge(matData(),datData(),oatData(),ratData()))
        if(class(t1) != "try-error"){
          return (t1)
        }
        
        t1 <-try(d <- merge(oatData(),matData(),datData(),ratData()))
        if(class(t1) != "try-error"){
          return (t1)
        }
        
        t1 <-try(d <- merge(ratData(),matData(),datData(),oatData()))
        if(class(t1) != "try-error"){
          return (t1)
        }
      }else{
        
        #if only one file present, return it instead of trying to merge
        if(onlyOne() == "dat"){
          return(merge(datData()))
        }
        else if(onlyOne() == "mat"){
          return(merge(matData()))
        }
        else if(onlyOne() == "oat"){
          return(merge(oatData()))
        }
        else if(onlyOne() == "rat"){
          return(merge(ratData()))
        }
      }
    }
  })
  
  #fix fanData() so endpoints are not cutof by date_range
  fan_clean <- eventReactive(input$date_range,{
    data <- fanData()
    
    start <- head(index(data),1)
    fan_start <- as.Date(start)
    
    end <- tail(index(data),1)
    fan_end <- as.Date(end)
    
    plot_start <- dateRange()[1]
    plot_end <- dateRange()[2]
    
    #TODO
    #if plot_start is later than fan_start, insert into data
    if(plot_start > fan_start)
    {
      #get the first value in data before plot_start
    
      #insert value at plot_start
      
    }
    
    if(plot_end < fan_end){
    #if plot_end is earlier than fan_end, insert into data
    
      #get the first value in data after plot_end
    
      #insert value at plot_end
    }
    
    return(data)
  })
  
  #Merge main data with fan_clean data
  data_all <- reactive({
    
  })
  
  #subset all data to date_range
  
  data_in_date_range <- eventReactive(updatePlot(),{
    
  })
  
  
  #Subset Data to selected date_range
  Data_in_dateRange <- reactive({

    start <- as.character(input$date_range[1])
    end <- as.character(input$date_range[2])
    dr <- paste0(start,"/",end)
    data <- getData()[dr]
  })
  
  #UI availabe date range output
  output$DAT_dates <- renderText({
    #TODO:
  })
  
  #TABLE
  tableVal<- eventReactive(updatePlot(),{
    fortify(Data_in_dateRange())
  })
  
  output$table <- renderDataTable(if(input$showTable){tableVal()})
  
  #line colors
  trend_colors <- vector(mode = 'list', length = 5)
  trend_colors[1] <- 'red'
  trend_colors[2] <- 'blue'
  trend_colors[3] <- 'green'
  trend_colors[4] <- 'violet'
  trend_colors[5] <- 'darkgrey'
  names(trend_colors) <- c('DAT','MAT','OAT','RAT','fan_status')
  
  #PLOT
  plotVal <- eventReactive(updatePlot(),{
    if(dataUploaded()){
      df <- isolate(Data_in_dateRange())
      
      #converts to dataframe
      df <- fortify(df)
      
      #gets available trace names (dat,mat etc.)
      cols <- colnames(df)[-1]
      
      #Left y-axis (temperature)
      y <- list(
        title = "Temperature"
      )
      
      #Right y-axis (fan_status)
      ay <- list(
        tickfont = list(color = "red"),
        overlaying = "y",
        side = "right",
        title = "Fan Status"
      )
      
      #Empty plotly
      plt <- plot_ly() %>% layout(title = 'Outside Air Control', yaxis = y)
      
      #add available traces to plt
      for(trace in cols){
        
        #omit indexes with NA values
        keep <- subset(df, !is.na(df[trace]))
        keepTrace <- parse(text = paste0("keep$",trace))
        t_color <- parse(text = paste0("trend_colors$",trace))
        
        #get enable/disable plot checkbox value
        enabled <- parse(text = paste0("input$", trace, "Checkbox"))
        if(trace != 'fan_status'){
          if(isolate(eval(enabled))){
            plt <- plt %>% add_lines(x = keep$Index, y = eval(keepTrace), name = trace, line = list(color = eval(t_color)))
          }
        }else{
          if(isolate(eval(enabled))){
            #on <- keep['fan_status'] == 1
            fan_time <- keep$Index
            
            fan_status <- eval(keepTrace)
            
            
            plt <- plt %>% add_lines(x = fan_time, y = fan_status, yaxis = 'y2',name = 'Fan Status', line = list(color = eval(t_color), width = 4))%>%
              layout(yaxis2 = ay)
            
          }
        }
      }
      plt
    }
    else{
      return (NULL)
    }
  })
  
  output$plot <- renderPlotly(plotVal())
  
}
shinyApp(ui = ui, server = server)
