#UI for Occupancy Scheduling in a tabPanel

library(shiny)
library(shinyTime)
library(DT)

DAY_MIN <- as.POSIXlt("00:00",tz="","%H:%M")
DAY_MAX <- as.POSIXlt("23:59",tz="","%H:%M")
DAY_START = as.POSIXlt("09:00",tz="","%H:%M")
DAY_END = as.POSIXlt("17:00",tz="","%H:%M")
STEP <- 900 #15 minutes

occupancyTab <- tabPanel('Occupancy',
                         splitLayout(
                           verticalLayout(
                             wellPanel(                  
                               fluidRow(
                                 column(2,checkboxInput(inputId = 'sun_occ',label = 'occupied',value=FALSE)),
                                 column(10,sliderInput(inputId='sun_slider',label='Sunday Occupied Hours',
                                                       min= DAY_MIN,max=DAY_MAX,
                                                       value=c(DAY_START,DAY_END),
                                                       step=STEP*2,timeFormat="%H:%M"))
                               ),
                               fluidRow(
                                 column(10,
                                        sliderInput(inputId='sun_startup',label='Sunday Startup Period',
                                                    min= DAY_MIN,max=DAY_MAX,
                                                    value= DAY_START + 4*STEP,
                                                    step=STEP,timeFormat="%H:%M")
                                        ,offset = 2)
                               )
                             ),
                             
                             wellPanel(
                               fluidRow(
                                 column(2,checkboxInput(inputId = 'mon_occ',label = 'occupied',value=TRUE)),
                                 column(10,sliderInput(inputId='mon_slider',label='Monday Occupied Hours',
                                                       min= DAY_MIN,max=DAY_MAX,
                                                       value=c(DAY_START,DAY_END),
                                                       step=STEP*2,timeFormat="%H:%M"))
                               ),
                               fluidRow(
                                 column(10,
                                        sliderInput(inputId='mon_startup',label='Monday Startup Period',
                                                    min= DAY_MIN,max=DAY_MAX,
                                                    value= DAY_START + 4*STEP,
                                                    step=STEP,timeFormat="%H:%M")
                                        ,offset = 2)
                               )
                             ),
                             
                             wellPanel(
                               fluidRow(
                                 column(2,checkboxInput(inputId = 'tue_occ',label = 'occupied',value=TRUE)),
                                 column(10,sliderInput(inputId='tue_slider',label='Tuesday Occupied Hours',
                                                       min= DAY_MIN,max=DAY_MAX,
                                                       value=c(DAY_START,DAY_END),
                                                       step=STEP*2,timeFormat="%H:%M"))
                               ),
                               fluidRow(
                                 column(10,
                                        sliderInput(inputId='tue_startup',label='Tuesday Startup Period',
                                                    min= DAY_MIN,max=DAY_MAX,
                                                    value= DAY_START + 4*STEP,
                                                    step=STEP,timeFormat="%H:%M")
                                        ,offset = 2)
                               )
                             ),
                             
                             wellPanel(
                               fluidRow(
                                 column(2,checkboxInput(inputId = 'wed_occ',label = 'occupied',value=TRUE)),
                                 column(10,sliderInput(inputId='wed_slider',label='Wednesday Occupied Hours',
                                                       min= DAY_MIN,max=DAY_MAX,
                                                       value=c(DAY_START,DAY_END),
                                                       step=STEP*2,timeFormat="%H:%M"))
                               ),
                               fluidRow(
                                 column(10,
                                        sliderInput(inputId='wed_startup',label='Wednesday Startup Period',
                                                    min= DAY_MIN,max=DAY_MAX,
                                                    value= DAY_START + 4*STEP,
                                                    step=STEP,timeFormat="%H:%M")
                                        ,offset = 2)
                               )
                             ),
                             
                             wellPanel(
                               fluidRow(
                                 column(2,checkboxInput(inputId = 'thu_occ',label = 'occupied',value=TRUE)),
                                 column(10,sliderInput(inputId='thu_slider',label='Thursday Occupied Hours',
                                                       min= DAY_MIN,max=DAY_MAX,
                                                       value=c(DAY_START,DAY_END),
                                                       step=STEP*2,timeFormat="%H:%M"))
                               ),
                               fluidRow(
                                 column(10,
                                        sliderInput(inputId='thu_startup',label='Thursday Startup Period',
                                                    min= DAY_MIN,max=DAY_MAX,
                                                    value= DAY_START + 4*STEP,
                                                    step=STEP,timeFormat="%H:%M")
                                        ,offset = 2)
                               )
                             ),
                             
                             wellPanel(
                               fluidRow(
                                 column(2,checkboxInput(inputId = 'fri_occ',label = 'occupied',value=TRUE)),
                                 column(10,sliderInput(inputId='fri_slider',label='Friday Occupied Hours',
                                                       min= DAY_MIN,max=DAY_MAX,
                                                       value=c(DAY_START,DAY_END),
                                                       step=STEP*2,timeFormat="%H:%M"))
                               ),
                               fluidRow(
                                 column(10,
                                        sliderInput(inputId='fri_startup',label='Friday Startup Period',
                                                    min= DAY_MIN,max=DAY_MAX,
                                                    value= DAY_START + 4*STEP,
                                                    step=STEP,timeFormat="%H:%M")
                                        ,offset = 2)
                               )
                             ),
                             
                             wellPanel(
                               fluidRow(
                                 column(2,checkboxInput(inputId = 'sat_occ',label = 'occupied',value=FALSE)),
                                 column(10,sliderInput(inputId='sat_slider',label='Saturday Occupied Hours',
                                                       min= DAY_MIN,max=DAY_MAX,
                                                       value=c(DAY_START,DAY_END),
                                                       step=STEP*2,timeFormat="%H:%M"))
                               ),
                               fluidRow(
                                 column(10,
                                        sliderInput(inputId='sat_startup',label='Saturday Startup Period',
                                                    min= DAY_MIN,max=DAY_MAX,
                                                    value= DAY_START + 4*STEP,
                                                    step=STEP,timeFormat="%H:%M")
                                        ,offset = 2)
                               )
                             )
                             
                           ),#1st split
                           verticalLayout(
                             # wellPanel(
                             #   h3("Holidays"),
                             #   sliderInput(inputId='holiday_slider',label='Add Holidays',
                             #               min= as.POSIXlt("01/01",tz="","%m/%d"),max=as.POSIXlt("12/31",tz="","%m/%d"),
                             #               value=c(as.POSIXlt("12/25",tz="","%m/%d"),as.POSIXlt("12/26",tz="","%m/%d")),
                             #               step=86400,timeFormat="%m/%d"),
                             #   textOutput('holiday_preview'),
                             #   textInput('holiday_name','Holiday Name',value="Christmas"),
                             #   actionButton('add_holiday','Add Holiday',width='100%'),
                             #   dataTableOutput('holiday_table')
                             # ),
                             wellPanel(
                               h3("Schedule Preview"),
                               dataTableOutput('occ_table'),
                               actionButton('update_preview','Update Preview',width='100%')
                             ),
                             wellPanel(
                               h3("Download"),
                               textInput('occ_filename','Filename',placeholder='occupancy_schedule'),
                               downloadButton('occ_csv',label='Download Occupancy CSV',width='100%')
                             )
                             
                           )
                         )
)
