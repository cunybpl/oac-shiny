#Convert first two columns of dataframe datain (time,value) to xts object
#data_sorter is intended for use with temperature logger files only
data_sorter <- function(filein){
  
  if(is.null(filein)){
    return (NULL)
  }
  else{
    #return data frame with empty strings as NA, header = 3 row (HOBOWARE)
    datain <- read.csv(filein, skip = 2, header = TRUE, na.strings = "") #can add more NA strings here to improve cleaning
    cnames = c("time","temp")
    colnames(datain) <- cnames
    
    #subset datain with no NA values in time/temp columns
    complete <- datain[complete.cases(datain[,1:2]),]
    
    #convert string datetimes to POSIX, assumes year,month,day,hour,minute,second format (HOBOWARE)
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