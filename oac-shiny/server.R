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

server <- function(input, output, session) {
  
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
  
  # #OCCUPANCY
  occData <- reactive({
    if(!is.null(input$occFile)){
      d1 <- input$occFile$datapath
      d1<-read.csv(d1,na.strings='NA')
      colnames(d1) <- c("ind","day","startup_start","startup_end","occ_start","occ_end")
      return(d1)
    }
    else{
      return (NA)
    }
  })

  output$occ_test_table <- renderDataTable(if(!is.na(occData())){
    occData()
  })

  occRects <- eventReactive(occData(),{
    if(is.na(occData())){
      return(NA)
    }
    else{
      df <- data.frame(dates=seq.POSIXt(from=dt_range()[1],to=dt_range()[2],by="day"))
      df$wday <- weekdays(df$dates)
      
      startup_rects <- list()
      occ_rects <- list()
      df
      
      for(row in 1:nrow(df)){
        date <- substr(df[row,'dates'],1,10)
        
        wday <- df[row,'wday']
        
        if(wday == 'Sunday' && !is.na(occData()[1,3])){
          startup_rect <- list(
            type = 'rect',
            x0 = ymd_hm(paste(date,occData()[1,3]),tz='UTC'),
            x1 = ymd_hm(paste(date,occData()[1,4]),tz='UTC'),
            y0 = 0,
            y1 = 1,
            xref = 'x',
            yref = 'paper',
            fillcolor = 'yellow'
            ,opacity = 0.2
          )
          occ_rect <- list(
            type = 'rect',
            x0 = ymd_hm(paste(date,occData()[1,5]),tz='UTC'),
            x1 = ymd_hm(paste(date,occData()[1,6]),tz='UTC'),
            y0 = 0,
            y1 = 1,
            xref = 'x',
            yref = 'paper',
            fillcolor = 'gray'
            ,opacity = 0.2
          )
          startup_rects[[row]] <- startup_rect
          occ_rects[[row]] <- occ_rect
        }
        
          else if(wday == 'Monday' && !is.na(occData()[2,3])){
            startup_rect <- list(
              type = 'rect',
              x0 = ymd_hm(paste(date,occData()[2,3]),tz='UTC'),
              x1 = ymd_hm(paste(date,occData()[2,4]),tz='UTC'),
              y0 = 0,
              y1 = 1,
              xref = 'x',
              yref = 'paper',
              fillcolor = 'yellow'
              ,opacity = 0.2
            )
            occ_rect <- list(
              type = 'rect',
              x0 = ymd_hm(paste(date,occData()[2,5]),tz='UTC'),
              x1 = ymd_hm(paste(date,occData()[2,6]),tz='UTC'),
              y0 = 0,
              y1 = 1,
              xref = 'x',
              yref = 'paper',
              fillcolor = 'gray'
              ,opacity = 0.2
            )
            startup_rects[[row]] <- startup_rect
            occ_rects[[row]] <- occ_rect
          }
        
        
          else if(wday == 'Tuesday' && !is.na(occData()[3,3])){
            startup_rect <- list(
              type = 'rect',
              x0 = ymd_hm(paste(date,occData()[3,3]),tz='UTC'),
              x1 = ymd_hm(paste(date,occData()[3,4]),tz='UTC'),
              y0 = 0,
              y1 = 1,
              xref = 'x',
              yref = 'paper',
              fillcolor = 'yellow'
              ,opacity = 0.2
            )
            occ_rect <- list(
              type = 'rect',
              x0 = ymd_hm(paste(date,occData()[3,5]),tz='UTC'),
              x1 = ymd_hm(paste(date,occData()[3,6]),tz='UTC'),
              y0 = 0,
              y1 = 1,
              xref = 'x',
              yref = 'paper',
              fillcolor = 'gray'
              ,opacity = 0.2
            )
            startup_rects[[row]] <- startup_rect
            occ_rects[[row]] <- occ_rect
          }
        
        
          else if(wday == 'Wednesday' && !is.na(occData()[4,3])){
            startup_rect <- list(
              type = 'rect',
              x0 = ymd_hm(paste(date,occData()[4,3]),tz='UTC'),
              x1 = ymd_hm(paste(date,occData()[4,4]),tz='UTC'),
              y0 = 0,
              y1 = 1,
              xref = 'x',
              yref = 'paper',
              fillcolor = 'yellow'
              ,opacity = 0.2
            )
            occ_rect <- list(
              type = 'rect',
              x0 = ymd_hm(paste(date,occData()[4,5]),tz='UTC'),
              x1 = ymd_hm(paste(date,occData()[4,6]),tz='UTC'),
              y0 = 0,
              y1 = 1,
              xref = 'x',
              yref = 'paper',
              fillcolor = 'gray'
              ,opacity = 0.2
            )
            startup_rects[[row]] <- startup_rect
            occ_rects[[row]] <- occ_rect
          }
        
        
          else if(wday == 'Thursday' && !is.na(occData()[5,3])){
            startup_rect <- list(
              type = 'rect',
              x0 = ymd_hm(paste(date,occData()[5,3]),tz='UTC'),
              x1 = ymd_hm(paste(date,occData()[5,4]),tz='UTC'),
              y0 = 0,
              y1 = 1,
              xref = 'x',
              yref = 'paper',
              fillcolor = 'yellow'
              ,opacity = 0.2
            )
            occ_rect <- list(
              type = 'rect',
              x0 = ymd_hm(paste(date,occData()[5,5]),tz='UTC'),
              x1 = ymd_hm(paste(date,occData()[5,6]),tz='UTC'),
              y0 = 0,
              y1 = 1,
              xref = 'x',
              yref = 'paper',
              fillcolor = 'gray'
              ,opacity = 0.2
            )
            startup_rects[[row]] <- startup_rect
            occ_rects[[row]] <- occ_rect
          }
        
        
          else if(wday == 'Friday' && !is.na(occData()[6,3])){
            startup_rect <- list(
              type = 'rect',
              x0 = ymd_hm(paste(date,occData()[6,3]),tz='UTC'),
              x1 = ymd_hm(paste(date,occData()[6,4]),tz='UTC'),
              y0 = 0,
              y1 = 1,
              xref = 'x',
              yref = 'paper',
              fillcolor = 'yellow'
              ,opacity = 0.2
            )
            occ_rect <- list(
              type = 'rect',
              x0 = ymd_hm(paste(date,occData()[6,5]),tz='UTC'),
              x1 = ymd_hm(paste(date,occData()[6,6]),tz='UTC'),
              y0 = 0,
              y1 = 1,
              xref = 'x',
              yref = 'paper',
              fillcolor = 'gray'
              ,opacity = 0.2
            )
            startup_rects[[row]] <- startup_rect
            occ_rects[[row]] <- occ_rect
          }
        
        
          else if(wday == 'Saturday' && !is.na(occData()[7,3])){
            startup_rect <- list(
              type = 'rect',
              x0 = ymd_hm(paste(date,occData()[7,3]),tz='UTC'),
              x1 = ymd_hm(paste(date,occData()[7,4]),tz='UTC'),
              y0 = 0,
              y1 = 1,
              xref = 'x',
              yref = 'paper',
              fillcolor = 'yellow'
              ,opacity = 0.2
            )
            occ_rect <- list(
              type = 'rect',
              x0 = ymd_hm(paste(date,occData()[7,5]),tz='UTC'),
              x1 = ymd_hm(paste(date,occData()[7,6]),tz='UTC'),
              y0 = 0,
              y1 = 1,
              xref = 'x',
              yref = 'paper',
              fillcolor = 'gray'
              ,opacity = 0.2
            )
            startup_rects[[row]] <- startup_rect
            occ_rects[[row]] <- occ_rect
          }
        
      }
      all_rects <- c(startup_rects,occ_rects)
      return(all_rects)
    }
  })
  
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
      
      if(!is.na(occRects())){
        plt <- plt %>% layout(shapes=occRects())
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
  disable('sun_slider')
  disable('sun_startup')
  observeEvent(input$sun_occ,{
    if(input$sun_occ == TRUE){
      enable('sun_slider')
      enable('sun_startup')
    }else{
      disable('sun_slider')
      disable('sun_startup')
    }
  })
 
  
  observeEvent(input$mon_occ,{
    if(input$mon_occ == FALSE){
      disable('mon_slider')
      disable('mon_startup')
    }else{
      enable('mon_slider')
      enable('mon_startup')
    }
  })

  observeEvent(input$tue_occ,{
    if(input$tue_occ == FALSE){
      disable('tue_slider')
      disable('tue_startup')
    }else{
      enable('tue_slider')
      enable('tue_startup')
    }
  })

  observeEvent(input$wed_occ,{
    if(input$wed_occ == FALSE){
      disable('wed_slider')
      disable('wed_startup')
    }else{
      enable('wed_slider')
      enable('wed_startup')
    }
  })

  observeEvent(input$thu_occ,{
    if(input$thu_occ == FALSE){
      disable('thu_slider')
      disable('thu_startup')
    }else{
      enable('thu_slider')
      enable('thu_startup')
    }
  })

  observeEvent(input$fri_occ,{
    if(input$fri_occ == FALSE){
      disable('fri_slider')
      disable('fri_startup')
    }else{
      enable('fri_slider')
      enable('fri_startup')
    }
  })

  disable('sat_slider')
  disable('sat_startup')
  observeEvent(input$sat_occ,{
    if(input$sat_occ == TRUE){
      enable('sat_slider')
      enable('sat_startup')
    }else{
      disable('sat_slider')
      disable('sat_startup')
    }
  })
  
  #Prep Timeinputs
  #NOTE: inputs sun_start, sun_end etc aree of clas POSIXlt
  sun <- reactive({
    if(input$sun_occ == FALSE){
      return(c('NA','NA','NA'))
    }
    else{
      startup_start <- strftime(input$sun_startup[1],format='%H:%M',tz=TZ)
      startup_end <- strftime(input$sun_startup[2],format='%H:%M',tz=TZ)
      start <- strftime(input$sun_slider[1],format='%H:%M',tz=TZ)
      end <- strftime(input$sun_slider[2],format='%H:%M',tz=TZ)
      
      sun <- c(startup_start,startup_end,start,end)
      return(sun)
    }
  })
  
  observeEvent(input$sun_slider,{
    updateSliderInput(session,'sun_startup',label = 'Sunday Startup Period',
                      max=input$sun_slider[1],
                      value= c(input$sun_slider[1] - 4*STEP,input$sun_slider[1]))
  })
  
  mon <- reactive({
    if(input$mon_occ == FALSE){
      return(c('NA','NA','NA'))
    }else{
      startup_start <- strftime(input$mon_startup[1],format='%H:%M',tz=TZ)
      startup_end <- strftime(input$mon_startup[2],format='%H:%M',tz=TZ)
      start <- strftime(input$mon_slider[1],format='%H:%M',tz=TZ)
      end <- strftime(input$mon_slider[2],format='%H:%M',tz=TZ)
      
      mon <- c(startup_start,startup_end,start,end)
      return(mon)
    }
  })
  
  observeEvent(input$mon_slider,{
    updateSliderInput(session,'mon_startup',label = 'Monday Startup Period',
                      max=input$mon_slider[1],
                      value= c(input$mon_slider[1] - 4*STEP,input$mon_slider[1]))
  })

  tue <- reactive({
    if(input$tue_occ == FALSE){
      return(c('NA','NA','NA'))
    }else{
      startup_start <- strftime(input$tue_startup[1],format='%H:%M',tz=TZ)
      startup_end <- strftime(input$tue_startup[2],format='%H:%M',tz=TZ)
      start <- strftime(input$tue_slider[1],format='%H:%M',tz=TZ)
      end <- strftime(input$tue_slider[2],format='%H:%M',tz=TZ)
      
      tue <- c(startup_start,startup_end,start,end)
      return(tue)
    }
  })
  
  observeEvent(input$tue_slider,{
    updateSliderInput(session,'tue_startup',label = 'Tuesday Startup Period',
                      max=input$tue_slider[1],
                      value= c(input$tue_slider[1] - 4*STEP,input$tue_slider[1]))
  })
  
  wed <- reactive({
    if(input$wed_occ == FALSE){
      return(c('NA','NA','NA'))
    }else{
      startup_start <- strftime(input$wed_startup[1],format='%H:%M',tz=TZ)
      startup_end <- strftime(input$wed_startup[2],format='%H:%M',tz=TZ)
      start <- strftime(input$wed_slider[1],format='%H:%M',tz=TZ)
      end <- strftime(input$wed_slider[2],format='%H:%M',tz=TZ)
      
      wed <- c(startup_start,startup_end,start,end)
      return(wed)
    }
  })
  
  observeEvent(input$wed_slider,{
    updateSliderInput(session,'wed_startup',label = 'Wednesday Startup Period',
                      max=input$wed_slider[1],
                      value= c(input$wed_slider[1] - 4*STEP,input$wed_slider[1]))
  })
  
  thu <- reactive({
    if(input$thu_occ == FALSE){
      return(c('NA','NA','NA'))
    }else{
      startup_start <- strftime(input$thu_startup[1],format='%H:%M',tz=TZ)
      startup_end <- strftime(input$thu_startup[2],format='%H:%M',tz=TZ)
      start <- strftime(input$thu_slider[1],format='%H:%M',tz=TZ)
      end <- strftime(input$thu_slider[2],format='%H:%M',tz=TZ)
      
      thu <- c(startup_start,startup_end,start,end)
      return(thu)
    }
  })
  
  observeEvent(input$thu_slider,{
    updateSliderInput(session,'thu_startup',label = 'Thursday Startup Period',
                      max=input$thu_slider[1],
                      value= c(input$thu_slider[1] - 4*STEP,input$thu_slider[1]))
  })
  
  fri <- reactive({
    if(input$fri_occ == FALSE){
      return(c('NA','NA','NA'))
    }else{
      startup_start <- strftime(input$fri_startup[1],format='%H:%M',tz=TZ)
      startup_end <- strftime(input$fri_startup[2],format='%H:%M',tz=TZ)
      start <- strftime(input$fri_slider[1],format='%H:%M',tz=TZ)
      end <- strftime(input$fri_slider[2],format='%H:%M',tz=TZ)
      
      fri <- c(startup_start,startup_end,start,end)
      return(fri)
    }
  })
  
  observeEvent(input$fri_slider,{
    updateSliderInput(session,'fri_startup',label = 'Friday Startup Period',
                      max=input$fri_slider[1],
                      value= c(input$fri_slider[1] - 4*STEP,input$fri_slider[1]))
  })
  
  sat <- reactive({
    if(input$sat_occ == FALSE){
      return(c('NA','NA','NA'))
    }else{
      startup_start <- strftime(input$sat_startup[1],format='%H:%M',tz=TZ)
      startup_end <- strftime(input$sat_startup[2],format='%H:%M',tz=TZ)
      start <- strftime(input$sat_slider[1],format='%H:%M',tz=TZ)
      end <- strftime(input$sat_slider[2],format='%H:%M',tz=TZ)
      
      sat <- c(startup_start,startup_end,start,end)
      return(sat)
    }
  })
  
  observeEvent(input$sat_slider,{
    updateSliderInput(session,'sat_startup',label = 'Saturday Startup Period',
                      max=input$sat_slider[1],
                      value= c(input$sat_slider[1] - 4*STEP,input$sat_slider[1]))
  })
  
  #dataframe to write to file
  occupancy <- eventReactive(input$update_preview,ignoreNULL=FALSE,{
    df <- data.frame(matrix(ncol = 5, nrow = 7))
    x <- c("day","startup_start",'startup_end','occupied_start','occupied_end')
    colnames(df) <- x
    
    #Standard Week
    sunday <- c('sun',sun()[1],sun()[2],sun()[3],sun()[4])
    df[1,] <- sunday
    
    monday <- c('mon',mon()[1],mon()[2],mon()[3],mon()[4])
    df[2,] <- monday
    
    tuesday <- c('tue',tue()[1],tue()[2],tue()[3],tue()[4])
    df[3,] <- tuesday
    
    wednesday <- c('wed',wed()[1],wed()[2],wed()[3],wed()[4])
    df[4,] <- wednesday
    
    thursday <- c('thu',thu()[1],thu()[2],thu()[3],thu()[4])
    df[5,] <- thursday
    
    friday <- c('fri',fri()[1],fri()[2],fri()[3],fri()[4])
    df[6,] <- friday
    
    saturday <- c('sat',sat()[1],sat()[2],sat()[3],sat()[4])
    df[7,] <- saturday
    
    return(df)
  })
  
  output$occ_table <- renderDataTable(occupancy())
  
  #holidays <- reactiveValues()
  # 
  # observeEvent(input$add_holiday,{
  #   holiday_name <- input$holiday_name
  #   start <- strftime(input$holiday_slider[1],format="%m/%d")
  #   end <- strftime(input$holiday_slider[2],format="%m/%d")
  #   holiday <- c(holiday_name,start,end)
  #   holidays[[holiday_name]] <- holiday
  # })
  
  #   holiday_df <- eventReactive(,{
  #   df <- data.frame()
  #   
  #   for(holiday in reactiveValuesToList(holidays)){
  #     df <- rbind(df,holiday)#TODO: order by date?
  #   }
  #   return(df)
  # })

  # occupancy_readable <- reactive({
  #   if(input$update_preview == 0){
  #     df <- occupancy()
  #     }
  #   else{
  #     df <- occupancy_holiday()
  #   }
  #   return(df)
  # })
  # 
  # output$holiday_table <- renderDataTable(holiday_df())
  
  
  # output$holiday_preview <- renderText(paste(
  #   'Starting: ',
  #   strftime(input$holiday_slider[1],format="%m/%d"),
  #   ' Ending ',
  #   strftime(input$holiday_slider[2],format="%m/%d")
  # )
  # )
  
  observe({
    toggleState('occ_csv',condition=input$occ_filename != "" | is.null(input$occ_filename))
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

