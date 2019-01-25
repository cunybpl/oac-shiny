#Outside Air Control Interactive Plotting 
#Daniel Vignoles, Mark Campmier

#UI

library(shiny)
library(shinythemes)
library(xts)
library(reshape)
library(lubridate)
library(plotly)
library(DT)
library(shinyjs)
source("data_prep.R")
source("occupancy_ui.R")

PAGE_TITLE <- 'NO-BAS OAC'

ui <- 
  navbarPage(theme=shinytheme('yeti'),title="NOBAS OAC",selected='Plotting',
  useShinyjs(),
  
  tabPanel('Plotting',
    sidebarPanel(
      div(id = "noBAS Visualization",
          
          #DAT file input
          fileInput("datFile", "Choose CSV File for Discharge Air Temperature (DAT)",
                    accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv")
          ),
          textOutput('DAT_serial'), #TODO
          textOutput('DAT_dates'), #TODO
          # conditionalPanel(condition = "output.datUploaded",
          #                  checkboxInput("DATCheckbox", label = "Display DAT Plot.", value = TRUE)
          # ),
          
          #MAT file input
          fileInput("matFile", "Choose CSV File for Mixed Air Temperature (MAT)",
                    accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv")
          ), 
          # conditionalPanel(condition = "output.matUploaded",
          #                  checkboxInput("MATCheckbox", label = "Display MAT Plot", value = TRUE)
          # ),
          
          #OAT file input
          fileInput("oatFile", "Choose CSV File for Outside Air Temperature (OAT)",
                    accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv")
          ),
          # conditionalPanel(condition = "output.oatUploaded",
          #                  checkboxInput("OATCheckbox", label = "Display OAT Plot.", value = TRUE)
          # ),
          
          #RAT file input
          fileInput("ratFile", "Choose CSV File for Return Air Temperature (RAT)",
                    accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv")
          ),
          # conditionalPanel(condition = "output.ratUploaded",
          #                  checkboxInput("RATCheckbox", label = "Display RAT Plot. ", value = TRUE)
          # ),
          
          #Fan Status file input
          fileInput("fanFile", "Choose CSV File for Fan Status",
                    accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv")
          ),
          fileInput("occFile", "Choose CSV File for Occupancy Schedule",
                    accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv")
          ),
          # conditionalPanel(condition = "output.fanUploaded",
          #                  checkboxInput("fan_statusCheckbox", label = "Display Fan Status.", value = TRUE)
          # ),
          #Date Range Selection
          uiOutput("date_range"),
          actionButton("update_plot",label = "Update Plot!", width = '100%'),
          br(),br(),uiOutput("help_link")
          
      )#div
    ),#sidebarPanel
    
    mainPanel(
      conditionalPanel(condition = "output.plot == null",
                       htmlOutput('start_instructions')),
      
      
      #Plot output
      plotlyOutput("plot",height = '500px'),
      
      #Data Table Output
      conditionalPanel(condition = "output.show_Plot_Options",
                       
                       div(style="display: inline-block;vertical-align:top; width: 160px;",
                           checkboxInput("DATCheckbox", label = "DAT", value = TRUE)
                       ),
                       div(style="display: inline-block;vertical-align:top; width: 160px;",
                           checkboxInput("MATCheckbox", label = "MAT", value = TRUE)
                       ),
                       div(style="display: inline-block;vertical-align:top; width: 160px;",
                           checkboxInput("OATCheckbox", label = "OAT", value = TRUE)
                       ),
                       div(style="display: inline-block;vertical-align:top; width: 160px;",
                           checkboxInput("RATCheckbox", label = "RAT", value = TRUE)
                       ),
                       div(style="display: inline-block;vertical-align:top; width: 160px;",
                           checkboxInput("fan_statusCheckbox", label = "FAN", value = TRUE)
                       ),
                       br(),br(),br(),
                       textInput('plot_title','Plot Title',value='Outside Air Control',placeholder='Your Plot Title Here'),
                       
                       actionButton("update_plot2", label = "Update Plot!", width = 800),
                       checkboxInput("showTable", label = "Display Data Table", value = FALSE)
      ),
      br(),
      dataTableOutput("table")
    )
  ),
  
  occupancyTab
  
)
