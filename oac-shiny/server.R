#Outside Air Control Interactive Plotting 
#Daniel Vignoles, Mark Campmier

#SERVER

#***********This application assumes all .csv's are retrieved/exported through HOBOware***********

#------------TODOS------------#

#TODO:Switch from fan_data_sorter then fan_fix_endpoints to vice versa

#TODO: Occupancy Scheduling

#TODO: Hide all trends where fan_status is 0

#TODO: Hide display of "transition lines" 0 -> 1, 1 -> 0 in fan_status trend

#TODO: Detect fahrenheit / Celsius based on header column name, ie: "Temp, (*F)"

#TODO: Read/save serial number from first line of file (temperature loggers)

#TODO: display serial number / date-range under file upload input

#-------------------------------#

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
  
  ####----OCCUPANCY----####
  
  #disable/enable timeinputs for unoccupied/occupied
  
  disable('sun_start')
  disable('sun_end')
  observeEvent(input$sun_occ,{
    if(input$sun_occ == TRUE){
      enable('sun_start')
      enable('sun_end')
    }else{
      disable('sun_start')
      disable('sun_end')
    }
  })
  
  observeEvent(input$mon_occ,{
    if(input$mon_occ == FALSE){
      disable('mon_start')
      disable('mon_end')
    }else{
      enable('mon_start')
      enable('mon_end')
    }
  })

  observeEvent(input$tue_occ,{
    if(input$tue_occ == FALSE){
      disable('tue_start')
      disable('tue_end')
    }else{
      enable('tue_start')
      enable('tue_end')
    }
  })

  observeEvent(input$wed_occ,{
    if(input$wed_occ == FALSE){
      disable('wed_start')
      disable('wed_end')
    }else{
      enable('wed_start')
      enable('wed_end')
    }
  })

  observeEvent(input$thu_occ,{
    if(input$thu_occ == FALSE){
      disable('thu_start')
      disable('thu_end')
    }else{
      enable('thu_start')
      enable('thu_end')
    }
  })

  observeEvent(input$fri_occ,{
    if(input$fri_occ == FALSE){
      disable('fri_start')
      disable('fri_end')
    }else{
      enable('fri_start')
      enable('fri_end')
    }
  })

  disable('sat_start')
  disable('sat_end')
  observeEvent(input$sat_occ,{
    if(input$sat_occ == TRUE){
      enable('sat_start')
      enable('sat_end')
    }else{
      disable('sat_start')
      disable('sat_end')
    }
  })
  
  #Prep Timeinputs
  #NOTE: inputs sun_start, sun_end etc aree of clas POSIXlt
  sun <- reactive({
    if(input$sun_occ == FALSE){
      return(NA)
    }
    else{
      start <- as.POSIXct(input$sun_start)
      end <- as.POSIXct(input$sun_end)
      
      sun <- c(start,end)
      return(sun)
    }
  })
  
  mon <- reactive({
    if(input$mon_occ == FALSE){
      return(NA)
    }else{
      start <- as.POSIXct(input$mon_start)
      end <- as.POSIXct(input$mon_end)
      
      mon <- c(start,end)
      return(mon)
    }
  })
  
  tue <- reactive({
    if(input$tue_occ == FALSE){
      return(NA)
    }else{
      start <- as.POSIXct(input$tue_start)
      end <- as.POSIXct(input$tue_end)
      
      tue <- c(start,end)
      return(tue)
    }
  })
  
  wed <- reactive({
    if(input$wed_occ == FALSE){
      return(NA)
    }else{
      start <- as.POSIXct(input$wed_start)
      end <- as.POSIXct(input$wed_end)
      
      wed <- c(start,end)
      return(wed)
    }
  })
  
  thu <- reactive({
    if(input$thu_occ == FALSE){
      return(NA)
    }else{
      start <- as.POSIXct(input$thu_start)
      end <- as.POSIXct(input$thu_end)
      
      thu <- c(start,end)
      return(thu)
    }
  })
  
  fri <- reactive({
    if(input$fri_occ == FALSE){
      return(NA)
    }else{
      start <- as.POSIXct(input$fri_start)
      end <- as.POSIXct(input$fri_end)
      
      fri <- c(start,end)
      return(fri)
    }
  })
  
  sat <- reactive({
    if(input$sat_occ == FALSE){
      return(NA)
    }else{
      start <- as.POSIXct(input$sat_start)
      end <- as.POSIXct(input$sat_end)
      
      sat <- c(start,end)
      return(sat)
    }
  })
  
  #dataframe to write to file
  occupancy <- reactive({
    df <- data.frame(matrix(ncol = 3, nrow = 7))
    x <- c("day", "start", "end")
    colnames(df) <- x
    
    sunday <- c('sun',sun()[1],sun()[2])
    df[1,] <- sunday
    
    monday <- c('mon',mon()[1],mon()[2])
    df[2,] <- monday
    
    tuesday <- c('tue',tue()[1],tue()[2])
    df[3,] <- tuesday
    
    wednesday <- c('wed',wed()[1],wed()[2])
    df[4,] <- wednesday
    
    thursday <- c('thu',thu()[1],thu()[2])
    df[5,] <- thursday
    
    friday <- c('fri',fri()[1],fri()[2])
    df[6,] <- friday
    
    saturday <- c('sat',sat()[1],sat()[2])
    df[7,] <- saturday
    
    return(df)
  })

  output$occ_csv <- downloadHandler(
    filename = function(){
      paste(input$occ_filename,".csv",sep="")
    },
    content = function(file){
      write.csv(occupancy(),file,na='NA')
    },
    contentType = "text/csv")

}

