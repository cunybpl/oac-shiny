#Outside Air Control Interactive Plotting
#Daniel Vignoles,
#SERVER

#***********This application assumes all .csv's are retrieved/exported through HOBOware***********

#------------TODOS------------#

#TODO:Switch from fan_data_sorter then fan_fix_endpoints to vice versa

#TODO: Hide all trends where fan_status is 0

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

  #dataUploaded() returns TRUE/FALSE for whether there is data to display
  dataUploaded <- reactive({
    datNull <- is.null(input$datFile)
    matNull <- is.null(input$matFile)
    oatNull <- is.null(input$oatFile)
    ratNull <- is.null(input$ratFile)
    
    isThereData <- !datNull || !matNull || !oatNull || !ratNull
  })
  
  #TRUE/FALSE for whether at least on trend checkbox is checked
  dataEnabled <- reactive({
    atLeastOneCheckbox <- input$DATCheckbox || input$MATCheckbox || input$OATCheckbox || input$RATCheckbox || input$fan_statusCheckbox
  })
  
  #Disable/Enable Occupancy & Fan inputs
  disable('occFile')
  observeEvent(dataUploaded(), {
    if (dataUploaded() == TRUE) {
      enable('occFile')
    }
  })
  
  disable('fanFile')
  observeEvent(dataUploaded(), {
    if (dataUploaded() == TRUE) {
      enable('fanFile')
    }
  })
  
  #combine update buttons
  updatePlot <- reactive({
    input$update_plot + input$update_plot2
  })
  
  #Determine whether or not to display DAT,MAT,etc. checkboxes, used in Conditional UI Panel
  output$show_Plot_Options <- eventReactive(updatePlot(), {
    dataUploaded()
  })
  outputOptions(output, 'show_Plot_Options', suspendWhenHidden = FALSE)
  
  ####----DATA----####
  
  #Retrieve processed DAT xts object
  datData <- reactive({
    if (!is.null(input$datFile)) {
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
    if (!is.null(input$matFile)) {
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
    if (!is.null(input$oatFile)) {
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
    if (!is.null(input$ratFile)) {
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
    if (!is.null(input$fanFile)) {
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
  enable_dat <- eventReactive(datData(), {
    if (!all(is.na(datData()))) {
      return(TRUE)
    }
    else{
      return(FALSE)
    }
  })
  
  disable('MATCheckbox')
  enable_mat <- eventReactive(matData(), {
    if (!all(is.na(matData()))) {
      return(TRUE)
    }
    else{
      return(FALSE)
    }
  })
  
  disable('OATCheckbox')
  enable_oat <- eventReactive(oatData(), {
    if (!all(is.na(oatData()))) {
      return(TRUE)
    }
    else{
      return(FALSE)
    }
  })
  
  disable('RATCheckbox')
  enable_rat <- eventReactive(ratData(), {
    if (!all(is.na(ratData()))) {
      return(TRUE)
    }
    else{
      return(FALSE)
    }
  })
  
  disable('fan_statusCheckbox')
  enable_fan <- eventReactive(fanData(), {
    if (!all(is.na(fanData()))) {
      return(TRUE)
    }
    else{
      return(FALSE)
    }
  })
  
  #Disable/Enable checkboxes when plot refreshes
  observeEvent(updatePlot(), {
    if (enable_dat()) {
      enable('DATCheckbox')
    }
    
    if (enable_mat()) {
      enable('MATCheckbox')
    }
    
    if (enable_oat()) {
      enable('OATCheckbox')
    }
    
    if (enable_rat()) {
      enable('RATCheckbox')
    }
    
    if (enable_fan()) {
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
    if (datNull == FALSE &&
        (matNull == TRUE) && (oatNull == TRUE) && (ratNull == TRUE)) {
      "dat"
    }
    
    else if (matNull == FALSE &&
             (datNull == TRUE) && (oatNull == TRUE) && (ratNull == TRUE)) {
      "mat"
    }
    
    else if (oatNull == FALSE &&
             (matNull == TRUE) && (datNull == TRUE) && (ratNull == TRUE)) {
      "oat"
    }
    
    else if (ratNull == FALSE &&
             (matNull == TRUE) && (oatNull == TRUE) && (datNull == TRUE)) {
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
    
    if (dataUploaded()) {
      dat <- datData()
      mat <- matData()
      oat <- oatData()
      rat <- ratData()
      fan <- fanData()
      
      all_data <- list(dat, mat, oat, rat, fan)
      notNA <- !is.na(all_data)
      all_data <- all_data[notNA]
      
      for (data in all_data) {
        data <- na.trim(data, is.na = 'all')
        
        start <- head(index(data), 1)
        start <- as.POSIXct(start)
        end <- tail(index(data), 1)
        end <- as.POSIXct(end)
        
        if (is.na(earliest_dt)) {
          earliest_dt <- start
        } else{
          if (start < earliest_dt) {
            earliest_dt <- start
          }
        }
        
        if (is.na(latest_dt)) {
          latest_dt <- end
        } else{
          if (end > latest_dt) {
            latest_dt <- end
          }
        }
        
      }
    }
    start_end <- c(earliest_dt, latest_dt)
  })
  
  #UI output to enter Date Range, restricted to dates available within available files
  output$date_range <- renderUI({
    start_date <- dt_range()[1]
    end_date <- dt_range()[2]
    
    if (is.na(start_date)) {
      start_date <- today()
    }
    else{
      start_date <- as.Date(start_date)
    }
    
    if (is.na(end_date)) {
      end_date <- today()
    }
    else{
      end_date <- as.Date(end_date)
    }
    
    dateRangeInput(
      "date_range",
      "Range of Dates to Plot",
      start = start_date,
      end = end_date,
      min = start_date,
      max = end_date
    )
  })
  
  #Merge available xts objects
  getData <- reactive({
    if (!dataUploaded()) {
      return (NULL)
    }
    else{
      #If multiple files, merge into one XTS table
      if (onlyOne() == FALSE) {
        data <- list(datData(), matData(), oatData(), ratData())
        notNA <- !is.na(data)
        data <- data[notNA]
        merged <- do.call(merge, data)
        return(merged)
      } else{
        #if only one file present, return it instead of trying to merge
        if (onlyOne() == "dat") {
          return(datData())
        }
        else if (onlyOne() == "mat") {
          return(matData())
        }
        else if (onlyOne() == "oat") {
          return(oatData())
        }
        else if (onlyOne() == "rat") {
          return(ratData())
        }
      }
    }
  })
  
  #fix fanData() so endpoints are not cutof by date_range
  fan_clean <- eventReactive(updatePlot(), {
    if (all(!is.na(fanData()))) {
      fan_fixed <- fix_fan_endpoints(fanData(), input$date_range)
    }
    else{
      return(NULL)
    }
  })
  
  #Merge main data with fan_clean data
  data_all <- reactive({
    merge(getData(), fan_clean())
  })
  
  #subset all data to date_range
  Data_in_dateRange <- eventReactive(updatePlot(), {
    start <- as.character(input$date_range[1])
    end <- as.character(input$date_range[2])
    dr <- paste0(start, "/", end)
    data <- data_all()[dr]
  })
  
  occData <- reactive({
    if (!is.null(input$occFile)) {
      d1 <- input$occFile$datapath
      d1 <- read.csv(d1, na.strings = 'NA')
      colnames(d1) <-
        c("ind",
          "day",
          "startup_start",
          "startup_end",
          "occ_start",
          "occ_end")
      
      #check for empty schedule
      if (is.na(d1[1, 3]) &&
          is.na(d1[2, 3]) &&
          is.na(d1[3, 3]) &&
          is.na(d1[4, 3]) &&
          is.na(d1[5, 3]) && is.na(d1[6, 3]) && is.na(d1[7, 3])) {
        return(NA)
      }
      else{
        return(d1)
      }
    }
    else{
      return (NA)
    }
  })
  
  
  occRects <- eventReactive({
    occData()
    input$date_range
  },
  {
    if (all(is.na(occData()))) {
      return(NA)
    }
    else{
      df <-
        data.frame(dates = seq.POSIXt(
          from = as.POSIXct(input$date_range[1]),
          to = as.POSIXct(input$date_range[2]),
          by = "day"
        ))
      df$wday <- weekdays(df$dates)
      
      startup_rects <- list()
      occ_rects <- list()
      
      line_properties = list(width=0)
      
      for (row in 1:nrow(df)) {
        date <- substr(df[row, 'dates'], 1, 10)
        
        wday <- df[row, 'wday']
        
        if (wday == 'Sunday' && !is.na(occData()[1, 3])) {
          startup_rect <- list(
            type = 'rect',
            line = line_properties,
            x0 = ymd_hm(paste(date, occData()[1, 3]), tz = 'UTC'),
            x1 = ymd_hm(paste(date, occData()[1, 4]), tz = 'UTC'),
            y0 = 0,
            y1 = 1,
            xref = 'x',
            yref = 'paper',
            fillcolor = 'yellow'
            ,
            opacity = 0.5,
            layer = 'below'
          )
          occ_rect <- list(
            type = 'rect',
            line = line_properties,
            x0 = ymd_hm(paste(date, occData()[1, 5]), tz = 'UTC'),
            x1 = ymd_hm(paste(date, occData()[1, 6]), tz = 'UTC'),
            y0 = 0,
            y1 = 1,
            xref = 'x',
            yref = 'paper',
            fillcolor = '#fff'
            ,
            opacity = 1.0,
            layer = 'below'
          )
          startup_rects[[row]] <- startup_rect
          occ_rects[[row]] <- occ_rect
        }
        
        else if (wday == 'Monday' && !is.na(occData()[2, 3])) {
          startup_rect <- list(
            type = 'rect',
            line = line_properties,
            x0 = ymd_hm(paste(date, occData()[2, 3]), tz = 'UTC'),
            x1 = ymd_hm(paste(date, occData()[2, 4]), tz = 'UTC'),
            y0 = 0,
            y1 = 1,
            xref = 'x',
            yref = 'paper',
            fillcolor = 'yellow'
            ,
            opacity = 0.5,
            layer = 'below'
          )
          occ_rect <- list(
            type = 'rect',
            line = line_properties,
            x0 = ymd_hm(paste(date, occData()[2, 5]), tz = 'UTC'),
            x1 = ymd_hm(paste(date, occData()[2, 6]), tz = 'UTC'),
            y0 = 0,
            y1 = 1,
            xref = 'x',
            yref = 'paper',
            fillcolor = '#fff'
            ,
            opacity = 1.0,
            layer = 'below'
          )
          startup_rects[[row]] <- startup_rect
          occ_rects[[row]] <- occ_rect
        }
        
        
        else if (wday == 'Tuesday' && !is.na(occData()[3, 3])) {
          startup_rect <- list(
            type = 'rect',
            line = line_properties,
            x0 = ymd_hm(paste(date, occData()[3, 3]), tz = 'UTC'),
            x1 = ymd_hm(paste(date, occData()[3, 4]), tz = 'UTC'),
            y0 = 0,
            y1 = 1,
            xref = 'x',
            yref = 'paper',
            fillcolor = 'yellow'
            ,
            opacity = 0.5,
            layer = 'below'
          )
          occ_rect <- list(
            type = 'rect',
            line = line_properties,
            x0 = ymd_hm(paste(date, occData()[3, 5]), tz = 'UTC'),
            x1 = ymd_hm(paste(date, occData()[3, 6]), tz = 'UTC'),
            y0 = 0,
            y1 = 1,
            xref = 'x',
            yref = 'paper',
            fillcolor = '#fff'
            ,
            opacity = 1.0,
            layer = 'below'
          )
          startup_rects[[row]] <- startup_rect
          occ_rects[[row]] <- occ_rect
        }
        
        
        else if (wday == 'Wednesday' && !is.na(occData()[4, 3])) {
          startup_rect <- list(
            type = 'rect',
            line = line_properties,
            x0 = ymd_hm(paste(date, occData()[4, 3]), tz = 'UTC'),
            x1 = ymd_hm(paste(date, occData()[4, 4]), tz = 'UTC'),
            y0 = 0,
            y1 = 1,
            xref = 'x',
            yref = 'paper',
            fillcolor = 'yellow'
            ,
            opacity = 0.5,
            layer = 'below'
          )
          occ_rect <- list(
            type = 'rect',
            line = line_properties,
            x0 = ymd_hm(paste(date, occData()[4, 5]), tz = 'UTC'),
            x1 = ymd_hm(paste(date, occData()[4, 6]), tz = 'UTC'),
            y0 = 0,
            y1 = 1,
            xref = 'x',
            yref = 'paper',
            fillcolor = '#fff'
            ,
            opacity = 1.0,
            layer = 'below'
          )
          startup_rects[[row]] <- startup_rect
          occ_rects[[row]] <- occ_rect
        }
        
        
        else if (wday == 'Thursday' && !is.na(occData()[5, 3])) {
          startup_rect <- list(
            type = 'rect',
            line = line_properties,
            x0 = ymd_hm(paste(date, occData()[5, 3]), tz = 'UTC'),
            x1 = ymd_hm(paste(date, occData()[5, 4]), tz = 'UTC'),
            y0 = 0,
            y1 = 1,
            xref = 'x',
            yref = 'paper',
            fillcolor = 'yellow'
            ,
            opacity = 0.5,
            layer = 'below'
          )
          occ_rect <- list(
            type = 'rect',
            line = line_properties,
            x0 = ymd_hm(paste(date, occData()[5, 5]), tz = 'UTC'),
            x1 = ymd_hm(paste(date, occData()[5, 6]), tz = 'UTC'),
            y0 = 0,
            y1 = 1,
            xref = 'x',
            yref = 'paper',
            fillcolor = '#fff'
            ,
            opacity = 1.0,
            layer = 'below'
          )
          startup_rects[[row]] <- startup_rect
          occ_rects[[row]] <- occ_rect
        }
        
        
        else if (wday == 'Friday' && !is.na(occData()[6, 3])) {
          startup_rect <- list(
            type = 'rect',
            line = line_properties,
            x0 = ymd_hm(paste(date, occData()[6, 3]), tz = 'UTC'),
            x1 = ymd_hm(paste(date, occData()[6, 4]), tz = 'UTC'),
            y0 = 0,
            y1 = 1,
            xref = 'x',
            yref = 'paper',
            fillcolor = 'yellow'
            ,
            opacity = 0.5,
            layer = 'below'
          )
          occ_rect <- list(
            type = 'rect',
            line = line_properties,
            x0 = ymd_hm(paste(date, occData()[6, 5]), tz = 'UTC'),
            x1 = ymd_hm(paste(date, occData()[6, 6]), tz = 'UTC'),
            y0 = 0,
            y1 = 1,
            xref = 'x',
            yref = 'paper',
            fillcolor = '#fff'
            ,
            opacity = 1.0,
            layer = 'below'
          )
          startup_rects[[row]] <- startup_rect
          occ_rects[[row]] <- occ_rect
        }
        
        
        else if (wday == 'Saturday' && !is.na(occData()[7, 3])) {
          startup_rect <- list(
            type = 'rect',
            line = line_properties,
            x0 = ymd_hm(paste(date, occData()[7, 3]), tz = 'UTC'),
            x1 = ymd_hm(paste(date, occData()[7, 4]), tz = 'UTC'),
            y0 = 0,
            y1 = 1,
            xref = 'x',
            yref = 'paper',
            fillcolor = 'yellow'
            ,
            opacity = 0.5,
            layer = 'below'
          )
          occ_rect <- list(
            type = 'rect',
            line = line_properties,
            x0 = ymd_hm(paste(date, occData()[7, 5]), tz = 'UTC'),
            x1 = ymd_hm(paste(date, occData()[7, 6]), tz = 'UTC'),
            y0 = 0,
            y1 = 1,
            xref = 'x',
            yref = 'paper',
            fillcolor = '#fff'
            ,
            opacity = 1.0,
            layer = 'below'
          )
          startup_rects[[row]] <- startup_rect
          occ_rects[[row]] <- occ_rect
        }
        
      }
      all_rects <- c(startup_rects, occ_rects)
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
  names(trend_colors) <- c('DAT', 'MAT', 'OAT', 'RAT', 'fan_status')
  
  #PLOT
  
  plotVal <- eventReactive(updatePlot(), {
    #Left y-axis (temperature)
    y <- list(title = "Temperature")
    
    #x-axis (time)
    x <- list(nticks = 50,
              tickangle = -90)
    
    #Right y-axis (fan_status)
    ay <- list(
      tickfont = list(color = "red"),
      overlaying = "y",
      side = "right",
      title = "Fan Status"
    )
    
    logo <- list(
      list(
        source =  "https://raw.githubusercontent.com/cunybpl/oac-shiny/occupancy/bpl-logo.png?raw=true",
        xref = "paper",
        yref = "paper",
        x = 0,
        y = 1,
        sizex = 1,
        sizey = 1,
        sizing = "contain",
        opacity = 0.3,
        layer = "below"
      )
    )
    
    leg <- list()
    
    if (dataUploaded() && dataEnabled()) {
      df <- isolate(Data_in_dateRange())
      
      #converts to dataframe
      df <- fortify(df)
      
      #gets available trace names (dat,mat etc.)
      cols <- colnames(df)[-1]
      
      
      
      #Empty plotly
      plt <-
        plot_ly(type = 'scatter', mode = 'lines') %>% layout(title = input$plot_title,
                             yaxis = y,
                             xaxis = x)
      
      #add available traces to plt
      for (trace in cols) {
        #omit indexes with NA values
        keep <- subset(df,!is.na(df[trace]))
        keepTrace <- parse(text = paste0("keep$", trace))
        t_color <- parse(text = paste0("trend_colors$", trace))
        
        #get enable/disable plot checkbox value
        enabled <- parse(text = paste0("input$", trace, "Checkbox"))
        if (trace != 'fan_status') {
          if (isolate(eval(enabled))) {
            plt <-
              plt %>% add_lines(
                x = keep$Index,
                y = eval(keepTrace),
                name = trace,
                line = list(color = eval(t_color))
              )
          }
        } else{
          if (isolate(eval(enabled))) {
            #on <- keep['fan_status'] == 1
            fan_time <- keep$Index
            
            fan_status <- eval(keepTrace)
            
            
            plt <-
              plt %>% add_lines(
                x = fan_time,
                y = fan_status,
                yaxis = 'y2',
                name = 'Fan Status',
                line = list(color = eval(t_color), width = 4)
              ) %>%
              layout(yaxis2 = ay)
            
          }
        }
      }
      
      if (all(!is.na(occRects()))) {
        plt <- plt %>% layout(plot_bgcolor = "#d7d2d2", shapes = occRects())
      }
      plt
    }
    else{
      return (
        plot_ly(type = 'scatter', mode = 'lines') %>% layout(
          title = 'Outside Air Control',
          yaxis = y,
          xaxis = x,
          images = logo
        )
      )
    }
  }, ignoreNULL = FALSE)
  output$plot <- renderPlotly(plotVal())
  
  ####----OCCUPANCY----####
  
  #disable/enable timeinputs for unoccupied/occupied
  observeEvent(input$sun_occ, {
    if (input$sun_occ == FALSE) {
      disable('sun_slider')
      disable('sun_startup')
    } else{
      enable('sun_slider')
      enable('sun_startup')
    }
  })
  
  
  observeEvent(input$mon_occ, {
    if (input$mon_occ == FALSE) {
      disable('mon_slider')
      disable('mon_startup')
    } else{
      enable('mon_slider')
      enable('mon_startup')
    }
  })
  
  observeEvent(input$tue_occ, {
    if (input$tue_occ == FALSE) {
      disable('tue_slider')
      disable('tue_startup')
    } else{
      enable('tue_slider')
      enable('tue_startup')
    }
  })
  
  observeEvent(input$wed_occ, {
    if (input$wed_occ == FALSE) {
      disable('wed_slider')
      disable('wed_startup')
    } else{
      enable('wed_slider')
      enable('wed_startup')
    }
  })
  
  observeEvent(input$thu_occ, {
    if (input$thu_occ == FALSE) {
      disable('thu_slider')
      disable('thu_startup')
    } else{
      enable('thu_slider')
      enable('thu_startup')
    }
  })
  
  observeEvent(input$fri_occ, {
    if (input$fri_occ == FALSE) {
      disable('fri_slider')
      disable('fri_startup')
    } else{
      enable('fri_slider')
      enable('fri_startup')
    }
  })
  
  observeEvent(input$sat_occ, {
    if (input$sat_occ == FALSE) {
      disable('sat_slider')
      disable('sat_startup')
    } else{
      enable('sat_slider')
      enable('sat_startup')
    }
  })
  
  ####----Slider Behavior---####
  
  sun_startup_end <- reactive({
    input$sun_startup[2]
  })
  sun_occ_start <- reactive({
    input$sun_slider[1]
  })
  
  observeEvent(sun_startup_end(), {
    diff <- time_diff(input$sun_slider[1], input$sun_slider[2]) * -1
    updateSliderInput(session,
                      'sun_slider',
                      value = c(sun_startup_end(), sun_startup_end() + diff))
  })
  
  observeEvent(sun_occ_start(), {
    diff <- time_diff(input$sun_startup[1], input$sun_startup[2])
    updateSliderInput(session,
                      'sun_startup',
                      value = c(sun_occ_start() + diff, sun_occ_start()))
  })
  
  mon_startup_end <- reactive({
    input$mon_startup[2]
  })
  mon_occ_start <- reactive({
    input$mon_slider[1]
  })
  
  observeEvent(mon_startup_end(), {
    diff <- time_diff(input$mon_slider[1], input$mon_slider[2]) * -1
    updateSliderInput(session,
                      'mon_slider',
                      value = c(mon_startup_end(), mon_startup_end() + diff))
  })
  
  observeEvent(mon_occ_start(), {
    diff <- time_diff(input$mon_startup[1], input$mon_startup[2])
    updateSliderInput(session,
                      'mon_startup',
                      value = c(mon_occ_start() + diff, mon_occ_start()))
  })
  
  tue_startup_end <- reactive({
    input$tue_startup[2]
  })
  tue_occ_start <- reactive({
    input$tue_slider[1]
  })
  
  observeEvent(tue_startup_end(), {
    diff <- time_diff(input$tue_slider[1], input$tue_slider[2]) * -1
    updateSliderInput(session,
                      'tue_slider',
                      value = c(tue_startup_end(), tue_startup_end() + diff))
  })
  
  observeEvent(tue_occ_start(), {
    diff <- time_diff(input$tue_startup[1], input$tue_startup[2])
    updateSliderInput(session,
                      'tue_startup',
                      value = c(tue_occ_start() + diff, tue_occ_start()))
  })
  
  wed_startup_end <- reactive({
    input$wed_startup[2]
  })
  wed_occ_start <- reactive({
    input$wed_slider[1]
  })
  
  observeEvent(wed_startup_end(), {
    diff <- time_diff(input$wed_slider[1], input$wed_slider[2]) * -1
    updateSliderInput(session,
                      'wed_slider',
                      value = c(wed_startup_end(), wed_startup_end() + diff))
  })
  
  observeEvent(wed_occ_start(), {
    diff <- time_diff(input$wed_startup[1], input$wed_startup[2])
    updateSliderInput(session,
                      'wed_startup',
                      value = c(wed_occ_start() + diff, wed_occ_start()))
  })
  
  thu_startup_end <- reactive({
    input$thu_startup[2]
  })
  thu_occ_start <- reactive({
    input$thu_slider[1]
  })
  
  observeEvent(thu_startup_end(), {
    diff <- time_diff(input$thu_slider[1], input$thu_slider[2]) * -1
    updateSliderInput(session,
                      'thu_slider',
                      value = c(thu_startup_end(), thu_startup_end() + diff))
  })
  
  observeEvent(thu_occ_start(), {
    diff <- time_diff(input$thu_startup[1], input$thu_startup[2])
    updateSliderInput(session,
                      'thu_startup',
                      value = c(thu_occ_start() + diff, thu_occ_start()))
  })
  
  fri_startup_end <- reactive({
    input$fri_startup[2]
  })
  fri_occ_start <- reactive({
    input$fri_slider[1]
  })
  
  observeEvent(fri_startup_end(), {
    diff <- time_diff(input$fri_slider[1], input$fri_slider[2]) * -1
    updateSliderInput(session,
                      'fri_slider',
                      value = c(fri_startup_end(), fri_startup_end() + diff))
  })
  
  observeEvent(fri_occ_start(), {
    diff <- time_diff(input$fri_startup[1], input$fri_startup[2])
    updateSliderInput(session,
                      'fri_startup',
                      value = c(fri_occ_start() + diff, fri_occ_start()))
  })
  
  sat_startup_end <- reactive({
    input$sat_startup[2]
  })
  sat_occ_start <- reactive({
    input$sat_slider[1]
  })
  
  observeEvent(sat_startup_end(), {
    diff <- time_diff(input$sat_slider[1], input$sat_slider[2]) * -1
    updateSliderInput(session,
                      'sat_slider',
                      value = c(sat_startup_end(), sat_startup_end() + diff))
  })
  
  observeEvent(sat_occ_start(), {
    diff <- time_diff(input$sat_startup[1], input$sat_startup[2])
    updateSliderInput(session,
                      'sat_startup',
                      value = c(sat_occ_start() + diff, sat_occ_start()))
  })
  
  #Prep Timeinputs
  #NOTE: inputs sun_start, sun_end etc aree of class POSIXlt
  sun <- reactive({
    if (input$sun_occ == FALSE) {
      return(c('NA', 'NA', 'NA'))
    }
    else{
      startup_start <-
        strftime(input$sun_startup[1], format = TIME_FORM, tz = TZ)
      startup_end <-
        strftime(input$sun_startup[2], format = TIME_FORM, tz = TZ)
      start <- strftime(input$sun_slider[1], format = TIME_FORM, tz = TZ)
      end <- strftime(input$sun_slider[2], format = TIME_FORM, tz = TZ)
      
      sun <- c(startup_start, startup_end, start, end)
      return(sun)
    }
  })
  
  mon <- reactive({
    if (input$mon_occ == FALSE) {
      return(c('NA', 'NA', 'NA'))
    } else{
      startup_start <-
        strftime(input$mon_startup[1], format = TIME_FORM, tz = TZ)
      startup_end <-
        strftime(input$mon_startup[2], format = TIME_FORM, tz = TZ)
      start <- strftime(input$mon_slider[1], format = TIME_FORM, tz = TZ)
      end <- strftime(input$mon_slider[2], format = TIME_FORM, tz = TZ)
      
      mon <- c(startup_start, startup_end, start, end)
      return(mon)
    }
  })
  
  tue <- reactive({
    if (input$tue_occ == FALSE) {
      return(c('NA', 'NA', 'NA'))
    } else{
      startup_start <-
        strftime(input$tue_startup[1], format = TIME_FORM, tz = TZ)
      startup_end <-
        strftime(input$tue_startup[2], format = TIME_FORM, tz = TZ)
      start <- strftime(input$tue_slider[1], format = TIME_FORM, tz = TZ)
      end <- strftime(input$tue_slider[2], format = TIME_FORM, tz = TZ)
      
      tue <- c(startup_start, startup_end, start, end)
      return(tue)
    }
  })
  
  wed <- reactive({
    if (input$wed_occ == FALSE) {
      return(c('NA', 'NA', 'NA'))
    } else{
      startup_start <-
        strftime(input$wed_startup[1], format = TIME_FORM, tz = TZ)
      startup_end <-
        strftime(input$wed_startup[2], format = TIME_FORM, tz = TZ)
      start <- strftime(input$wed_slider[1], format = TIME_FORM, tz = TZ)
      end <- strftime(input$wed_slider[2], format = TIME_FORM, tz = TZ)
      
      wed <- c(startup_start, startup_end, start, end)
      return(wed)
    }
  })
  
  thu <- reactive({
    if (input$thu_occ == FALSE) {
      return(c('NA', 'NA', 'NA'))
    } else{
      startup_start <-
        strftime(input$thu_startup[1], format = TIME_FORM, tz = TZ)
      startup_end <-
        strftime(input$thu_startup[2], format = TIME_FORM, tz = TZ)
      start <- strftime(input$thu_slider[1], format = TIME_FORM, tz = TZ)
      end <- strftime(input$thu_slider[2], format = TIME_FORM, tz = TZ)
      
      thu <- c(startup_start, startup_end, start, end)
      return(thu)
    }
  })
  
  fri <- reactive({
    if (input$fri_occ == FALSE) {
      return(c('NA', 'NA', 'NA'))
    } else{
      startup_start <-
        strftime(input$fri_startup[1], format = TIME_FORM, tz = TZ)
      startup_end <-
        strftime(input$fri_startup[2], format = TIME_FORM, tz = TZ)
      start <- strftime(input$fri_slider[1], format = TIME_FORM, tz = TZ)
      end <- strftime(input$fri_slider[2], format = TIME_FORM, tz = TZ)
      
      fri <- c(startup_start, startup_end, start, end)
      return(fri)
    }
  })
  
  sat <- reactive({
    if (input$sat_occ == FALSE) {
      return(c('NA', 'NA', 'NA'))
    } else{
      startup_start <-
        strftime(input$sat_startup[1], format = TIME_FORM, tz = TZ)
      startup_end <-
        strftime(input$sat_startup[2], format = TIME_FORM, tz = TZ)
      start <- strftime(input$sat_slider[1], format = TIME_FORM, tz = TZ)
      end <- strftime(input$sat_slider[2], format = TIME_FORM, tz = TZ)
      
      sat <- c(startup_start, startup_end, start, end)
      return(sat)
    }
  })
  
  #Flag for when download_button pressed
  rv <- reactiveValues(download_flag = 0)
  
  #dataframe to write to file
  occupancy_preview <- eventReactive({
    input$update_preview
    rv$download_flag
  }
  , ignoreNULL = FALSE, {
    df <- data.frame(matrix(ncol = 5, nrow = 7))
    x <-
      c("day",
        "startup_start",
        'startup_end',
        'occupied_start',
        'occupied_end')
    colnames(df) <- x
    
    #Standard Week
    sunday <- c('sun', sun()[1], sun()[2], sun()[3], sun()[4])
    df[1, ] <- sunday
    
    monday <- c('mon', mon()[1], mon()[2], mon()[3], mon()[4])
    df[2, ] <- monday
    
    tuesday <- c('tue', tue()[1], tue()[2], tue()[3], tue()[4])
    df[3, ] <- tuesday
    
    wednesday <- c('wed', wed()[1], wed()[2], wed()[3], wed()[4])
    df[4, ] <- wednesday
    
    thursday <- c('thu', thu()[1], thu()[2], thu()[3], thu()[4])
    df[5, ] <- thursday
    
    friday <- c('fri', fri()[1], fri()[2], fri()[3], fri()[4])
    df[6, ] <- friday
    
    saturday <- c('sat', sat()[1], sat()[2], sat()[3], sat()[4])
    df[7, ] <- saturday
    
    return(df)
  })
  
  occupancy <- reactive({
    df <- data.frame(matrix(ncol = 5, nrow = 7))
    x <-
      c("day",
        "startup_start",
        'startup_end',
        'occupied_start',
        'occupied_end')
    colnames(df) <- x
    
    #Standard Week
    sunday <- c('sun', sun()[1], sun()[2], sun()[3], sun()[4])
    df[1, ] <- sunday
    
    monday <- c('mon', mon()[1], mon()[2], mon()[3], mon()[4])
    df[2, ] <- monday
    
    tuesday <- c('tue', tue()[1], tue()[2], tue()[3], tue()[4])
    df[3, ] <- tuesday
    
    wednesday <- c('wed', wed()[1], wed()[2], wed()[3], wed()[4])
    df[4, ] <- wednesday
    
    thursday <- c('thu', thu()[1], thu()[2], thu()[3], thu()[4])
    df[5, ] <- thursday
    
    friday <- c('fri', fri()[1], fri()[2], fri()[3], fri()[4])
    df[6, ] <- friday
    
    saturday <- c('sat', sat()[1], sat()[2], sat()[3], sat()[4])
    df[7, ] <- saturday
    
    return(df)
  })
  
  output$occ_table <-
    renderDataTable(occupancy_preview(), options = list(bLengthChange = F))
  
  observe({
    toggleState('occ_csv',
                condition = input$occ_filename != "" | is.null(input$occ_filename))
  })
  output$occ_csv <- downloadHandler(
    filename = function() {
      paste(input$occ_filename, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(occupancy(), file, na = 'NA')
      
      #update occupancy_preview
      rv$download_flag <- rv$download_flag + 1
    },
    contentType = "text/csv"
  )
}
