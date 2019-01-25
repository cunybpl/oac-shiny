library(plotly)
library(lubridate)

#occupancy data.frame setup
s1 <- as.POSIXlt("09:00",tz="","%H:%M")
s2 <- as.POSIXlt("12:00",tz="","%H:%M")

e1 <- as.POSIXlt("17:00",tz="","%H:%M")
e2 <- as.POSIXlt("20:00",tz="","%H:%M")

start <- c(s1,s1,s2,s2,s2,s1,NA)
end <- c(e1,e2,e1,e2,e1,e2,NA)

days <- c('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday')

occ <- data.frame(start,end,row.names=days)
occ$wday <- days

#data setup
trace_0 <- rnorm(100, mean = 90)
trace_1 <- rnorm(100, mean = 80)
trace_2 <- rnorm(100, mean = 70)

dstart <- ymd('2019-01-01',tz='UTC')
dend <- ymd('2019-01-31',tz='UTC')

x <- seq.POSIXt(from=dstart,to=dend,length.out=100)

data <- data.frame(x, trace_0, trace_1, trace_2)


#example rectangles
r1 <- list(
  type = 'rect',
  x0 = ymd('2019-01-01',tz='UTC'),
  x1 = ymd('2019-01-05',tz='UTC'),
  y0 = 0,
  y1 = 1,
  xref = 'x',
  yref = 'paper',
  fillcolor = 'blue',
  opacity = 0.2
)

r2 <- list(
  type = 'rect',
  x0 = ymd('2019-01-07',tz='UTC'),
  x1 = ymd('2019-01-12',tz='UTC'),
  y0 = 0,
  y1 = 1,
  xref = 'x',
  yref = 'paper',
  fillcolor = 'blue'
  ,opacity = 0.2
)

rects <- list(r1,r2)

#plotting
p <- plot_ly(data=data, x = ~x) %>%
  add_trace(y = ~trace_0, name = 'trace 0',type = 'scatter',mode = 'lines') %>%
  add_trace(y = ~trace_1, name = 'trace 1',type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~trace_2, name = 'trace 2',type = 'scatter', mode = 'lines') %>% layout(shapes=rects)

heading <- list("day","startup_start","startup_end","occupied_start","occupied_end")
sun <- list("sun","00:00","00:30","01:00","23:59")
mon<-list("mon","02:00","03:30","03:30","21:00")
tue <- list("tue","03:00","07:30","09:00","22:00")
wed <- list("wed","06:30","09:00","09:00","22:00")
thu <- list("thu","01:30","04:15","06:00","18:30")
fri<- list("fri","08:00","09:00","09:00","17:00")
sat<-list("sat","04:30","08:45","09:00","20:30")

#rectangle generation based on occ schedule
df <- data.frame(dates=seq.POSIXt(from=dstart,to=dend,by="day"))
df$wday <- weekdays(df$dates)

startup_rects <- list()
occ_rects <- list()
for(row in 1:nrow(df)){
  date <- substr(df[row,'dates'],1,10)
  wday <- df[row,'wday']
  
  if(wday == 'Sunday'){
    startup_rect <- list(
      type = 'rect',
      x0 = ymd_hm(paste(date,sun[2]),tz='UTC'),
      x1 = ymd_hm(paste(date,sun[3]),tz='UTC'),
      y0 = 0,
      y1 = 1,
      xref = 'x',
      yref = 'paper',
      fillcolor = 'red'
      ,opacity = 0.2
    )
    occ_rect <- list(
      type = 'rect',
      x0 = ymd_hm(paste(date,sun[4]),tz='UTC'),
      x1 = ymd_hm(paste(date,sun[5]),tz='UTC'),
      y0 = 0,
      y1 = 1,
      xref = 'x',
      yref = 'paper',
      fillcolor = 'blue'
      ,opacity = 0.2
    )
    startup_rects[[row]] <- startup_rect
    occ_rects[[row]] <- occ_rect
  }
  
  if(wday == 'Monday'){
    startup_rect <- list(
      type = 'rect',
      x0 = ymd_hm(paste(date,mon[2]),tz='UTC'),
      x1 = ymd_hm(paste(date,mon[3]),tz='UTC'),
      y0 = 0,
      y1 = 1,
      xref = 'x',
      yref = 'paper',
      fillcolor = 'red'
      ,opacity = 0.2
    )
    occ_rect <- list(
      type = 'rect',
      x0 = ymd_hm(paste(date,mon[4]),tz='UTC'),
      x1 = ymd_hm(paste(date,mon[5]),tz='UTC'),
      y0 = 0,
      y1 = 1,
      xref = 'x',
      yref = 'paper',
      fillcolor = 'blue'
      ,opacity = 0.2
    )
    startup_rects[[row]] <- startup_rect
    occ_rects[[row]] <- occ_rect
  }
  
  if(wday == 'Tuesday'){
    startup_rect <- list(
      type = 'rect',
      x0 = ymd_hm(paste(date,tue[2]),tz='UTC'),
      x1 = ymd_hm(paste(date,tue[3]),tz='UTC'),
      y0 = 0,
      y1 = 1,
      xref = 'x',
      yref = 'paper',
      fillcolor = 'red'
      ,opacity = 0.2
    )
    occ_rect <- list(
      type = 'rect',
      x0 = ymd_hm(paste(date,tue[4]),tz='UTC'),
      x1 = ymd_hm(paste(date,tue[5]),tz='UTC'),
      y0 = 0,
      y1 = 1,
      xref = 'x',
      yref = 'paper',
      fillcolor = 'blue'
      ,opacity = 0.2
    )
    startup_rects[[row]] <- startup_rect
    occ_rects[[row]] <- occ_rect
  }
  
  if(wday == 'Wednesday'){
    startup_rect <- list(
      type = 'rect',
      x0 = ymd_hm(paste(date,wed[2]),tz='UTC'),
      x1 = ymd_hm(paste(date,wed[3]),tz='UTC'),
      y0 = 0,
      y1 = 1,
      xref = 'x',
      yref = 'paper',
      fillcolor = 'red'
      ,opacity = 0.2
    )
    occ_rect <- list(
      type = 'rect',
      x0 = ymd_hm(paste(date,wed[4]),tz='UTC'),
      x1 = ymd_hm(paste(date,wed[5]),tz='UTC'),
      y0 = 0,
      y1 = 1,
      xref = 'x',
      yref = 'paper',
      fillcolor = 'blue'
      ,opacity = 0.2
    )
    startup_rects[[row]] <- startup_rect
    occ_rects[[row]] <- occ_rect
  }
  
  if(wday == 'Thursday'){
    startup_rect <- list(
      type = 'rect',
      x0 = ymd_hm(paste(date,thu[2]),tz='UTC'),
      x1 = ymd_hm(paste(date,thu[3]),tz='UTC'),
      y0 = 0,
      y1 = 1,
      xref = 'x',
      yref = 'paper',
      fillcolor = 'red'
      ,opacity = 0.2
    )
    occ_rect <- list(
      type = 'rect',
      x0 = ymd_hm(paste(date,thu[4]),tz='UTC'),
      x1 = ymd_hm(paste(date,thu[5]),tz='UTC'),
      y0 = 0,
      y1 = 1,
      xref = 'x',
      yref = 'paper',
      fillcolor = 'blue'
      ,opacity = 0.2
    )
    startup_rects[[row]] <- startup_rect
    occ_rects[[row]] <- occ_rect
  }
  
  if(wday == 'Friday'){
    startup_rect <- list(
      type = 'rect',
      x0 = ymd_hm(paste(date,fri[2]),tz='UTC'),
      x1 = ymd_hm(paste(date,fri[3]),tz='UTC'),
      y0 = 0,
      y1 = 1,
      xref = 'x',
      yref = 'paper',
      fillcolor = 'red'
      ,opacity = 0.2
    )
    occ_rect <- list(
      type = 'rect',
      x0 = ymd_hm(paste(date,fri[4]),tz='UTC'),
      x1 = ymd_hm(paste(date,fri[5]),tz='UTC'),
      y0 = 0,
      y1 = 1,
      xref = 'x',
      yref = 'paper',
      fillcolor = 'blue'
      ,opacity = 0.2
    )
    startup_rects[[row]] <- startup_rect
    occ_rects[[row]] <- occ_rect
  }
  
  if(wday == 'Saturday'){
    startup_rect <- list(
      type = 'rect',
      x0 = ymd_hm(paste(date,sat[2]),tz='UTC'),
      x1 = ymd_hm(paste(date,sat[3]),tz='UTC'),
      y0 = 0,
      y1 = 1,
      xref = 'x',
      yref = 'paper',
      fillcolor = 'red'
      ,opacity = 0.2
    )
    occ_rect <- list(
      type = 'rect',
      x0 = ymd_hm(paste(date,sat[4]),tz='UTC'),
      x1 = ymd_hm(paste(date,sat[5]),tz='UTC'),
      y0 = 0,
      y1 = 1,
      xref = 'x',
      yref = 'paper',
      fillcolor = 'blue'
      ,opacity = 0.2
    )
    startup_rects[[row]] <- startup_rect
    occ_rects[[row]] <- occ_rect
  }
}

all_rects <- c(startup_rects,occ_rects)

q <- plot_ly(data=data, x = ~x) %>%
  add_trace(y = ~trace_0, name = 'trace 0',type = 'scatter',mode = 'lines') %>%
  add_trace(y = ~trace_1, name = 'trace 1',type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~trace_2, name = 'trace 2',type = 'scatter', mode = 'lines') %>% layout(shapes=all_rects)

  
