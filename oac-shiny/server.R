#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(xts)
library(reshape)
library(lubridate)
library(plotly)
library(DT)
library(shinyjs)
source("data_prep.R")

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
  
  output$start_instructions <- renderUI({
    str1 <- paste('TO GET STARTED:')
    str2 <-  paste('1. In the Left Panel, click browse under the trend (DAT, MAT, etc.) you wish to upload')
    str3 <- paste('2. In the popup window, find the .csv file that corresponds to your trend')
    str4 <- paste('3. Repeat 1-2 for as many trends as you wish')
    str5 <- paste('4. Click Update Plot to display the Plot of your uploaded data')
    
    h4(HTML(paste(str1,str2,str3,str4,str5, sep = '<br/>')))
  })
  
  url <- a("HELP: INSTRUCTIONS", href="https://docs.google.com/document/d/e/2PACX-1vSqYcgS51UgN6R32jMq1mfZteSTSWqYPOwzM8wJE9ael5R6SjuC2N-fK-I26-bOpOZtDRL8L7ibM7ku/pub",target="_blank")
  output$help_link <- renderUI({
    tagList(h4(url))
  })
  
  #Retrieve processed DAT xts object
  datData <- reactive({
    if(!is.null(input$datFile)){
      d1 <- data_sorter(input$datFile$datapath)
      colnames(d1) <- "DAT"
      return(d1)
    }
    else{
      return (NA)
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
      return (NA)
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
      return (NA)
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
      return (NA)
    }
  })
  
  #Retrieve processed fan_status xts object
  fanData <- reactive({
    if(!is.null(input$fanFile)){
      d1 <- fan_data_sorter(input$fanFile$datapath)
      colnames(d1) <- 'fan_status'
      return(d1)
    }
    else{
      return(NA)
    }
  })
  
  #Disable/Enable Plot Checkboxes
  disable('DATCheckbox')
  enable_dat <- eventReactive(datData(),{
    if(!is.na(datData())){
      return(TRUE)
    }
    else{
      return(FALSE)
    }
  })
  
  disable('MATCheckbox')
  enable_mat <- eventReactive(matData(),{
    if(!is.na(matData())){
      return(TRUE)
    }
    else{
      return(FALSE)
    }
  })
  
  disable('OATCheckbox')
  enable_oat <- eventReactive(oatData(),{
    if(!is.na(oatData())){
      return(TRUE)
    }
    else{
      return(FALSE)
    }
  })
  
  disable('RATCheckbox')
  enable_rat <- eventReactive(ratData(),{
    if(!is.na(ratData())){
      return(TRUE)
    }
    else{
      return(FALSE)
    }
  })
  
  disable('fan_statusCheckbox')
  enable_fan <- eventReactive(fanData(),{
    if(!is.na(fanData())){
      return(TRUE)
    }
    else{
      return(FALSE)
    }
  })
  
  observeEvent(updatePlot(),{
    if(enable_dat()){
      enable('DATCheckbox')
    }
    
    if(enable_mat()){
      enable('MATCheckbox')
    }
    
    if(enable_oat()){
      enable('OATCheckbox')
    }
    
    if(enable_rat()){
      enable('RATCheckbox')
    }
    
    if(enable_fan()){
      enable('fan_statusCheckbox')
    }
  })
  
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
  
  #available date range of all uploaded files return: (min date, max date)
  dt_range <- reactive({
    
    earliest_dt <- NA
    latest_dt <- NA
    
    if(dataUploaded()){
      
      dat <- datData()
      mat <- matData()
      oat <- oatData()
      rat <- ratData()
      fan <- fanData()
      
      all_data <- list(dat,mat,oat,rat,fan)
      notNA <- !is.na(all_data)
      all_data <- all_data[notNA]
      
      for(data in all_data){
        
        data <- na.trim(data, is.na = 'all')
        
        start <- head(index(data),1)
        start <- as.POSIXct(start)
        end <- tail(index(data),1)
        end <- as.POSIXct(end)
        
        if(is.na(earliest_dt)){
          earliest_dt <- start
        }else{
          if(start < earliest_dt){
            earliest_dt <- start
          }
        }
        
        if(is.na(latest_dt)){
          latest_dt <- end
        }else{
          if(end > latest_dt){
            latest_dt <- end
          }
        }
        
      }
    }
    start_end <- c(earliest_dt,latest_dt)
  })
  
  #UI output to enter Date Range, restricted to dates available within available files
  output$date_range <- renderUI({
    start_date <- dt_range()[1]
    end_date <- dt_range()[2]
    
    if(is.na(start_date)){
      start_date <- today()
    }
    else{
      start_date <- as.Date(start_date)
    }
    
    if(is.na(end_date)){
      end_date <- today()
    }
    else{
      end_date <- as.Date(end_date)
    }
    
    dateRangeInput("date_range","Range of Dates to Plot", start = start_date, end = end_date,  min = start_date, max = end_date)
  })
  
  #Merge available xts objects
  getData <- reactive({
    if(!dataUploaded()){
      return (NULL)
    }
    else{
      #If multiple files, merge into one XTS table
      if(onlyOne() == FALSE){
        data <- list(datData(),matData(),oatData(),ratData())
        notNA <- !is.na(data)
        data <- data[notNA]
        merged <- do.call(merge,data)
        return(merged)
      }else{
        
        #if only one file present, return it instead of trying to merge
        if(onlyOne() == "dat"){
          return(datData())
        }
        else if(onlyOne() == "mat"){
          return(matData())
        }
        else if(onlyOne() == "oat"){
          return(oatData())
        }
        else if(onlyOne() == "rat"){
          return(ratData())
        }
      }
    }
  })
  
  #fix fanData() so endpoints are not cutof by date_range
  fan_clean <- eventReactive(updatePlot(),{
    if(!is.na(fanData())){
      fan_fixed <- fix_fan_endpoints(fanData(),input$date_range)
    }
    else{
      return(NULL)
    }
  })
  
  #Merge main data with fan_clean data
  data_all <- reactive({
    merge(getData(),fan_clean())
  })
  
  #subset all data to date_range
  Data_in_dateRange <- eventReactive(updatePlot(),{
    start <- as.character(input$date_range[1])
    end <- as.character(input$date_range[2])
    dr <- paste0(start,"/",end)
    data <- data_all()[dr]
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

