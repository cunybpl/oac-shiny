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
                               fluidRow(
                                 column(6,textInput('occ_filename',label="Filename",value='occupancy_schedule')),
                                 column(6,downloadButton('occ_csv',label='Download Occupancy CSV'))
                               )
                             ),
                             wellPanel(
                               dataTableOutput('occ_table'),
                               actionButton('update_preview','Update Preview',width='100%')
                             )
                           )
                         )
)
