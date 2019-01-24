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

dstart <- ymd('2019-01-01',tz='EST')
dend <- ymd('2019-01-31',tz='EST')

x <- seq.POSIXt(from=dstart,to=dend,length.out=100)

data <- data.frame(x, trace_0, trace_1, trace_2)


#example rectangles
r1 <- list(
  type = 'rect',
  x0 = ymd('2019-01-01',tz='EST'),
  x1 = ymd('2019-01-05',tz='EST'),
  y0 = 0,
  y1 = 1,
  xref = 'x',
  yref = 'paper',
  fillcolor = 'blue',
  opacity = 0.2
)

r2 <- list(
  type = 'rect',
  x0 = ymd('2019-01-07',tz='EST'),
  x1 = ymd('2019-01-12',tz='EST'),
  y0 = 0,
  y1 = 1,
  xref = 'x',
  yref = 'paper',
  fillcolor = 'blue'
  ,opacity = 0.2
)

rects <- list(r1,r2)

#rectangle generation based on occ schedule
df <- data.frame(dates=seq.POSIXt(from=dstart,to=dend,by="day"))
df$wday <- weekdays(df$dates)




#plotting
p <- plot_ly(data=data, x = ~x) %>%
  add_trace(y = ~trace_0, name = 'trace 0',type = 'scatter',mode = 'lines') %>%
  add_trace(y = ~trace_1, name = 'trace 1',type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~trace_2, name = 'trace 2',type = 'scatter', mode = 'lines') %>% layout(shapes=rects)


