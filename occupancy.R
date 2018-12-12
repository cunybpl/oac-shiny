#UI for Occupancy Scheduling in a tabPanel

library(shiny)
library(shinyTime)
library(DT)

DAY_START_DEFAULT = strptime('09:00', format = '%H:%M')
DAY_END_DEFAULT = strptime('17:00', format = '%H:%M')

occupancyTab <- tabPanel('Occupancy',
                         splitLayout(
                           verticalLayout(
                             wellPanel(                  
                               fluidRow(
                                 column(2,checkboxInput(inputId = 'sun_occ',label = 'occupied',value=FALSE)),
                                 column(10,sliderInput(inputId='sun_slider',label='Sunday Occupied Hours',
                                                       min= as.POSIXlt("00:00",tz="","%H:%M"),max=as.POSIXlt("23:59",tz="","%H:%M"),
                                                       value=c(as.POSIXlt("09:00",tz="","%H:%M"),as.POSIXlt("17:00",tz="","%H:%M")),
                                                       step=1800,timeFormat="%H:%M"))
                               )
                             ),
                             
                             wellPanel(
                               fluidRow(
                                 column(2,checkboxInput(inputId = 'mon_occ',label = 'occupied',value=TRUE)),
                                 column(10,sliderInput(inputId='mon_slider',label='Monday Occupied Hours',
                                                       min= as.POSIXlt("00:00",tz="","%H:%M"),max=as.POSIXlt("23:59",tz="","%H:%M"),
                                                       value=c(as.POSIXlt("09:00",tz="","%H:%M"),as.POSIXlt("17:00",tz="","%H:%M")),
                                                       step=1800,timeFormat="%H:%M"))
                               )
                             ),
                             
                             wellPanel(
                               fluidRow(
                                 column(2,checkboxInput(inputId = 'tue_occ',label = 'occupied',value=TRUE)),
                                 column(10,sliderInput(inputId='tue_slider',label='Tuesday Occupied Hours',
                                                       min= as.POSIXlt("00:00",tz="","%H:%M"),max=as.POSIXlt("23:59",tz="","%H:%M"),
                                                       value=c(as.POSIXlt("09:00",tz="","%H:%M"),as.POSIXlt("17:00",tz="","%H:%M")),
                                                       step=1800,timeFormat="%H:%M"))
                               )
                             ),
                             
                             wellPanel(
                               fluidRow(
                                 column(2,checkboxInput(inputId = 'wed_occ',label = 'occupied',value=TRUE)),
                                 column(10,sliderInput(inputId='wed_slider',label='Wednesday Occupied Hours',
                                                       min= as.POSIXlt("00:00",tz="","%H:%M"),max=as.POSIXlt("23:59",tz="","%H:%M"),
                                                       value=c(as.POSIXlt("09:00",tz="","%H:%M"),as.POSIXlt("17:00",tz="","%H:%M")),
                                                       step=1800,timeFormat="%H:%M"))
                               )
                             ),
                             
                             wellPanel(
                               fluidRow(
                                 column(2,checkboxInput(inputId = 'thu_occ',label = 'occupied',value=TRUE)),
                                 column(10,sliderInput(inputId='thu_slider',label='Thursday Occupied Hours',
                                                       min= as.POSIXlt("00:00",tz="","%H:%M"),max=as.POSIXlt("23:59",tz="","%H:%M"),
                                                       value=c(as.POSIXlt("09:00",tz="","%H:%M"),as.POSIXlt("17:00",tz="","%H:%M")),
                                                       step=1800,timeFormat="%H:%M"))
                               )
                             ),
                             
                             wellPanel(
                               fluidRow(
                                 column(2,checkboxInput(inputId = 'fri_occ',label = 'occupied',value=TRUE)),
                                 column(10,sliderInput(inputId='fri_slider',label='Friday Occupied Hours',
                                                       min= as.POSIXlt("00:00",tz="","%H:%M"),max=as.POSIXlt("23:59",tz="","%H:%M"),
                                                       value=c(as.POSIXlt("09:00",tz="","%H:%M"),as.POSIXlt("17:00",tz="","%H:%M")),
                                                       step=1800,timeFormat="%H:%M"))
                               )
                             ),
                             
                             wellPanel(
                               fluidRow(
                                 column(2,checkboxInput(inputId = 'sat_occ',label = 'occupied',value=FALSE)),
                                 column(10,sliderInput(inputId='sat_slider',label='Saturday Occupied Hours',
                                                       min= as.POSIXlt("00:00",tz="","%H:%M"),max=as.POSIXlt("23:59",tz="","%H:%M"),
                                                       value=c(as.POSIXlt("09:00",tz="","%H:%M"),as.POSIXlt("17:00",tz="","%H:%M")),
                                                       step=1800,timeFormat="%H:%M"))
                               )
                             )
                             
                           ),#1st split
                           verticalLayout(
                             wellPanel(
                               h3("Holidays"),
                               fluidRow(
                                 column(2,
                                        selectInput('holiday_month','Month',
                                                    choices=c('JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','NOV','DEC')
                                        )
                                 ),
                                 column(10,#TODO: Account for Date Validation
                                        sliderInput('holiday_days','Day(s)',min =1,max=31,value=c(1,1),step=1)
                                 )
                               ),
                               actionButton('add_holiday','Add Holiday',width='100%')
                               
                             ),
                             wellPanel(
                               h3("Schedule Preview"),
                               dataTableOutput('occ_table'),
                               actionButton('update_preview','Update Preview',width='100%')
                             ),
                             wellPanel(
                               h3("Download"),
                               downloadButton('occ_csv',label='Download Occupancy CSV',width='100%')
                             )
                             
                           )
                         )
)
