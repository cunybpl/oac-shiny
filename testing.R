#Misc. testing



library(shiny)
library(shinyjs)
library(zoo)
library(xts)
library(reshape)
library(lubridate)
library(ggplot2)
library(plotly)
library(DT)
#library(shinyTime)

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


######################################################
dateRange <- function(all_data){
  
  first_date <- NA
  last_date <- NA
  
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
  return(c(first_date,last_date))
}

##########################################

#fan within bounds of mat/rat
case_1_fan <- "C:\\Users\\dvign\\Desktop\\bpl\\oac_data\\oac_data\\fan_testing\\case_1_fan.csv"
case_1_rat <- "C:\\Users\\dvign\\Desktop\\bpl\\oac_data\\oac_data\\fan_testing\\case_1_rat.csv"
case_1_mat <- "C:\\Users\\dvign\\Desktop\\bpl\\oac_data\\oac_data\\fan_testing\\case_1_mat.csv"

case_1_fan <- fan_dataIn(case_1_fan)
case_1_rat <- data_sorter(case_1_rat)
case_1_mat <- data_sorter(case_1_mat)

case1 <- list(case_1_rat, case_1_mat)
dr_case1 <- dateRange(case1)

#beginning of fan outside range of mat/rat
case_2_fan <- "C:\\Users\\dvign\\Desktop\\bpl\\oac_data\\oac_data\\fan_testing\\case_2_fan.csv"
case_2_rat <- "C:\\Users\\dvign\\Desktop\\bpl\\oac_data\\oac_data\\fan_testing\\case_2_rat.csv"
case_2_mat <- "C:\\Users\\dvign\\Desktop\\bpl\\oac_data\\oac_data\\fan_testing\\case_2_mat.csv"

case_2_fan <- fan_dataIn(case_2_fan)
case_2_rat <- data_sorter(case_2_rat)
case_2_mat <- data_sorter(case_2_mat)

case2 <- list(case_2_rat, case_2_mat)
dr_case2 <- dateRange(case2)

#end of fan outside mat/rat
case_3_fan <- "C:\\Users\\dvign\\Desktop\\bpl\\oac_data\\oac_data\\fan_testing\\case_3_fan.csv"
case_3_rat <- "C:\\Users\\dvign\\Desktop\\bpl\\oac_data\\oac_data\\fan_testing\\case_3_rat.csv"
case_3_mat <- "C:\\Users\\dvign\\Desktop\\bpl\\oac_data\\oac_data\\fan_testing\\case_3_mat.csv"

case_3_fan <- fan_dataIn(case_3_fan)
case_3_rat <- data_sorter(case_3_rat)
case_3_mat <- data_sorter(case_3_mat)

case3 <- list(case_3_rat, case_3_mat)
dr_case3 <- dateRange(case3)

#both beginning/end outside range of mat/rat
case_4_fan <- "C:\\Users\\dvign\\Desktop\\bpl\\oac_data\\oac_data\\fan_testing\\case_4_fan.csv"
case_4_rat <- "C:\\Users\\dvign\\Desktop\\bpl\\oac_data\\oac_data\\fan_testing\\case_4_rat.csv"
case_4_mat <- "C:\\Users\\dvign\\Desktop\\bpl\\oac_data\\oac_data\\fan_testing\\case_4_mat.csv"

case_4_fan <- fan_dataIn(case_4_fan)
case_4_rat <- data_sorter(case_4_rat)
case_4_mat <- data_sorter(case_4_mat)

case4 <- list(case_4_rat, case_4_mat)
dr_case4 <- dateRange(case4)

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
  
  return(fan_data)
}

test <- fix_fan_endpoints(case_4_fan,dr_case4)


data <- list(NA)
data_index <- 1

foo <- function(file_dp){
  data[data_index] <- file_dp
  data_index <- data_index + 1
}