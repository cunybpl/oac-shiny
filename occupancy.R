#UI for Occupancy Scheduling in a tabPanel

library(shiny)
library(shinyTime)

DAY_START_DEFAULT = strptime('09:00', format = '%H:%M')
DAY_END_DEFAULT = strptime('17:00', format = '%H:%M')

occupancyTab <- tabPanel('Occupancy',
                         splitLayout(
                           wellPanel(
                             wellPanel(                  
                               fluidRow(
                                 column(2,h4("Sunday")),
                                 column(2,checkboxInput(inputId = 'sun_occ',label = 'occupied',value=FALSE)),
                                 column(4,timeInput(inputId='sun_start',label='Start',seconds=FALSE,value=DAY_START_DEFAULT)),
                                 column(4,timeInput(inputId='sun_end',label='End',seconds=FALSE,value=DAY_END_DEFAULT))
                               )
                             ),
                             
                             wellPanel(
                               fluidRow(
                                 column(2,h4("Monday")),
                                 column(2,checkboxInput(inputId = 'mon_occ',label = 'occupied',value=TRUE)),
                                 column(4,timeInput(inputId='mon_start',label='Start',seconds=FALSE,value=DAY_START_DEFAULT)),
                                 column(4,timeInput(inputId='mon_end',label='End',seconds=FALSE,value=DAY_END_DEFAULT))
                               )
                             ),
                             
                             wellPanel(
                               fluidRow(
                                 column(2,h4("Tuesday")),
                                 column(2,checkboxInput(inputId = 'tue_occ',label = 'occupied',value=TRUE)),
                                 column(4,timeInput(inputId='tue_start',label='Start',seconds=FALSE,value=DAY_START_DEFAULT)),
                                 column(4,timeInput(inputId='tue_end',label='End',seconds=FALSE,value=DAY_END_DEFAULT))
                               )
                             ),
                             
                             wellPanel(
                               fluidRow(
                                 column(2,h4("Wednesday")),
                                 column(2,checkboxInput(inputId = 'wed_occ',label = 'occupied',value=TRUE)),
                                 column(4,timeInput(inputId='wed_start',label='Start',seconds=FALSE,value=DAY_START_DEFAULT)),
                                 column(4,timeInput(inputId='wed_end',label='End',seconds=FALSE,value=DAY_END_DEFAULT))
                               )
                             ),
                             
                             wellPanel(
                               fluidRow(
                                 column(2,h4("Thursday")),
                                 column(2,checkboxInput(inputId = 'thu_occ',label = 'occupied',value=TRUE)),
                                 column(4,timeInput(inputId='thu_start',label='Start',seconds=FALSE,value=DAY_START_DEFAULT)),
                                 column(4,timeInput(inputId='thu_end',label='End',seconds=FALSE,value=DAY_END_DEFAULT))
                               )
                             ),
                             
                             wellPanel(
                               fluidRow(
                                 column(2,h4("Friday")),
                                 column(2,checkboxInput(inputId = 'fri_occ',label = 'occupied',value=TRUE)),
                                 column(4,timeInput(inputId='fri_start',label='Start',seconds=FALSE,value=DAY_START_DEFAULT)),
                                 column(4,timeInput(inputId='fri_end',label='End',seconds=FALSE,value=DAY_END_DEFAULT))
                               )
                             ),
                             
                             wellPanel(
                               fluidRow(
                                 column(2,h4("Saturday")),
                                 column(2,checkboxInput(inputId = 'sat_occ',label = 'occupied',value=FALSE)),
                                 column(4,timeInput(inputId='sat_start',label='Start',seconds=FALSE,value=DAY_START_DEFAULT)),
                                 column(4,timeInput(inputId='sat_end',label='End',seconds=FALSE,value=DAY_END_DEFAULT))
                               )
                             )
                             
                           ),#1st split
                           wellPanel(
                             textInput('occ_filename',label='Filename',value='occupancy_schedule'),
                             downloadButton('occ_csv',label='Download Occupancy CSV')
                             #TODO: Occupancy Schedule Previe Panel 
                           )
                         )
                         
)
