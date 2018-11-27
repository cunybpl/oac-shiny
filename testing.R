library(shiny)
library(shinyjs)
library(zoo)
library(xts)
library(reshape)
library(lubridate)
library(ggplot2)
library(plotly)
library(DT)
library(shinyTime)

data_sorter <- function(filein){
  #TODO: Account for obvious outlier values (see RAT.csv)
  
  if(is.null(filein)){
    return (NULL)
  }
  else{
    #return data frame with empty striings as NA
    datain <- read.csv(filein, skip = 2, header = TRUE, na.strings = "")
    cnames = c("time","temp")
    colnames(datain) <- cnames
    
    #subset datain with no NA values in time/temp columns
    complete <- datain[complete.cases(datain[,1:2]),]
    
    #convert string datetimes to POSIX
    datetimes <- complete$time
    datetimes <- ymd_hms(datetimes)
    
    #Time series value          
    values <- complete$temp
    
    xtsdata <- xts(values,order.by = datetimes)
    
    #Account for Periodicty < 15 minutes
    p <- periodicity(xtsdata)
    if(p['frequency'] < 15 && p['units'] == 'mins'){
      xtsdata <- to.minutes15(xtsdata)[,1]
    }
    
    return(xtsdata)
  }
}


fan_dataIn <- function(filein){
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

fanFile <- "C:\\Users\\dvign\\Desktop\\bpl\\oac_data\\dsf\\DSF_4th_F45__Fan_Status.csv"
ratFile <- "C:\\Users\\dvign\\Desktop\\bpl\\oac_data\\dsf\\DSF-4fl-AHU44-RAT 2018-03-01 14-04-53 -0500.csv"
matFile <- "C:\\Users\\dvign\\Desktop\\bpl\\oac_data\\dsf\\DSF-5fl-H1-MAT 2018-03-01 12-54-18 -0500.csv"

fan_data <- fan_dataIn(fanFile)
rat <- data_sorter(ratFile)
mat <- data_sorter(matFile)

first_date <- NA
last_date <- NA
    
all_data <- list(mat, rat, fan_data)

for(data in all_data){
  data <- na.trim(data, is.na = 'all')
  
  start <- head(index(data),1)
  start <- as.POSIXct(start)
  end <- tail(index(data),1)
  end <- as.POSIXct(end)
  
  if(is.na(first_date)){
    first_date <- start
  }else{
    if(start < first_date){
      first_date <- start
    }
  }
  
  if(is.na(last_date)){
    last_date <- end
  }else{
    if(end > last_date){
      last_date <- end
    }
  }
}
dt_range <- c(first_date,last_date)


##########################################

find_close_before <- function(xtsdata, start){
  index_data <- index(xtsdata)
  core_data <- coredata(xtsdata)
  
  prev <- index_data[1]
  for(dt in index_data[-1]){
    if(dt > start){
      break
    }
    prev <- dt
  }
  
  #prev is the datetime in fan_data which is right before the start time
  #prev_val is the value at the prev datetime within xtsdata
  prev_ind <- which(index_data == prev)
  prev_val <- core_data[prev_ind]
  
  #insert into xtsdata at datetime start prev_val
  xtsdata[start] <- prev_val
  return(xtsdata)
}





#FAN CLEAN
fan_data <- fan_data

fan_start <- head(index(data),1)
fan_end <- tail(index(data),1)

plot_start <- dt_range[1]
plot_end <- dt_range[2]

# #TODO
# #if plot_start is later than fan_start, insert into data
# if(plot_start > fan_start)
# {
#   #get the first value in data before plot_start
#   index_at_plot_start <- which(index_fan == plot_start)
#   index_before_plot_start <- index_at_plot_start -1
#   
#   #insert value at plot_start
#   
# }
# 
# #if plot_end is earlier than fan_end, insert into data
# if(plot_end < fan_end){
#   #get the first value in data after plot_end
#   index_at_plot_end <- which(index_fan == plot_end)
#   index_after_plot_end <- index_at_plot_end + 1
#   #insert value at plot_end
# }
# 
# fan_clean <- data




