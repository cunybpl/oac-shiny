#Convert first two columns of dataframe datain (time,value) to xts object
#data_sorter is intended for use with temperature logger files only

find_start <- function(column,formats){
  count <- 1
  for(entry in column){
    
    if(count > 50){
      return(NA)
    }
    
    result <- parse_date_time(entry,formats)
    
    if(!is.na(result)){
      return(count)
    }
    count <- count + 1
  }
}

get_date_times <- function(data){
  formats <- c('Ymd HM','Ymd HMS','mdy IMS p')
  
  col1 <- data[[1]]
  #col2 <- data[[2]]
  
  start <- find_start(col1,formats)
  
  if(!is.na(start)){
    col <- tail(col1, -start)
    dates <- parse_date_time(col,orders = formats)
    
    result <- list('datetimes' = dates,'start_pos' = start)
    return(result)
  }else{
    return(NA)
  }
  
  #TODO:Enable this block to support 2nd column datetimes(fan data)
  # else{
  #   
  #   start <- find_start(col2,formats)
  #   col <- tail(col2, -start)
  #   dates <- parse_date_time(col,orders = formats, truncated = 1)
  #   return(dates)
  # }
}

data_sorter <- function(filein){
  
  if(is.null(filein)){
    return (NULL)
  }
  else{
    #return data frame with empty strings as NA, header = 3 row (HOBOWARE)
    datain <- read.csv(filein, skip = 1, na.strings = "") #can add more NA strings here to improve cleaning
    cnames = c("time","temp")
    colnames(datain) <- cnames
    
    #subset datain with no NA values in time/temp columns
    complete <- datain[complete.cases(datain[,1:2]),]
    
    #convert string datetimes to POSIX, assumes year,month,day,hour,minute,second format (HOBOWARE)
    gtd <- get_date_times(complete)
    
    if(all(is.na(gtd))){ #first column not datetime column
      complete <- datain[complete.cases(datain[,2:3]),]
      
      complete <- subset(complete, select = -1)
      
      colnames(complete) <- cnames #fix colnames 
      
      gtd <- get_date_times(complete)
    }
    
    datetimes <- gtd$datetimes
    
    start <- gtd$start_pos
    
    #Time series value          
    values <- complete$temp
    values <- tail(values,-start)
    
    xtsdata <- xts(values,order.by = datetimes)
    
    #Account for Periodicty < 15 minutes
    p <- periodicity(xtsdata)
    if(p['frequency'] < 15 && p['units'] == 'mins'){
      xtsdata <- to.minutes15(xtsdata)[,1]
    }
    
    return(xtsdata)
  }
}

#Same function as data_sorter, but specifically for fan_status .csv's
fan_data_sorter <- function(filein){
  if(is.null(filein)){
    return (NULL)
  }
  else{
    datain <- read.csv(filein, skip = 2, header = TRUE, na.strings = "")
    cnames <- c("num","time","value")
    colnames(datain) <- cnames
    
    #subset datain with no NA values in time/temp columns
    complete <- datain[complete.cases(datain[,2:3]),]
    
    #convert string datetimes to POSIX
    datetimes <- complete$time
    datetimes <- mdy_hms(datetimes)
    
    #Time series value          
    values <- complete$value
    
    #add duplicate datapoints to fill in space between state changes
    #1,0,1 -> 1,1,0,0,1,1
    #Necessary in order to plot duration of fan status rather than just points where status changes
    #TODO: clarify/redesign this block
    xtsdata <- xts(values,order.by = datetimes)
    times <- index(xtsdata)
    times <- times - 1
    times <- times[-1]
    nas <- rep(NA,length(times))
    insert <- xts(nas,times)
    xtsdata <- rbind(xtsdata,insert)
    xtsdata <- na.locf(xtsdata)
    
    return(xtsdata)
  }
}

#Plug in values for endpoints of plot which cutoff fan_data
fix_fan_endpoints <- function(fan_data, date_range){
  
  fan_data <- fan_data
  index_fan <- index(fan_data)
  core_fan <- coredata(fan_data)
  
  start <- date_range[1]
  end <- date_range[2]
  
  fan_start <- head(index(fan_data),1)
  fan_end <- tail(index(fan_data),1)
  
  #if no endpoints cutoff
  if(fan_start > start && fan_end < end){
    return(fan_data)
  }
  
  #if beginning cutoff
  if(fan_start < start){
    prev <- index_fan[1]
    prev_ind = 1
    for(dt in index_fan[-1]){
      if(dt > start){
        break
      }
      prev <- dt
      prev_ind <- prev_ind + 1
    }
    prev_val <- core_fan[prev_ind]
    to_combine <- xts(prev_val, order.by = start)
    fan_data <- rbind(fan_data, to_combine)
  }
  
  #if end cutoff
  if(fan_end > end){
    
    ind <- length(fan_data)
    prev <- index_fan[ind]
    
    ind <- ind -1
    prev_ind <- ind
    
    for(i in ind:1){
      dt <- index_fan[i]
      if(dt < end){
        break
      }
      prev <- dt
      prev_ind <- i
    }
    prev_val <- core_fan[prev_ind]
    to_combine <- xts(prev_val, order.by = end)
    fan_data <- rbind(fan_data, to_combine)
  }
  
  #return fan_data with insertted endpoint values
  return(fan_data)
}

#return a layout object of occupancy rectangles to add to main plot
occupancy_rects <- function(schedule,date_range,ymax,ymin){
  
}

#return diff_time in seconds between two times
time_diff <- function(time1,time2){
  as.integer(difftime(time1,time2,units='secs'))
}
