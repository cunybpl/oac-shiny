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
library(markdown)
source("data_prep.R")
source("occupancy_ui.R")

ui <-
  navbarPage(
    theme = shinytheme('yeti'),
    title = "noBAS OAC",
    selected = 'Plotting',
    useShinyjs(),
    
    #Surpress Search Bar in DataTable
    tags$head(
      tags$style(type = "text/css", ".dataTables_filter {display: none;    }")
    ),
    tabPanel('Tutorial',
             fluidPage(column(
               8, offset = 2,
               includeMarkdown("oac-tutorial.md")
             ))),
    tabPanel(
      'Plotting',
      sidebarPanel(
        div(
          id = "noBAS Visualization",
          
          #DAT file input
          fileInput(
            "datFile",
            "Choose CSV File for Discharge Air Temperature (DAT)",
            accept = c("text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv")
          ),
          #MAT file input
          fileInput(
            "matFile",
            "Choose CSV File for Mixed Air Temperature (MAT)",
            accept = c("text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv")
          ),
          #OAT file input
          fileInput(
            "oatFile",
            "Choose CSV File for Outside Air Temperature (OAT)",
            accept = c("text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv")
          ),
          #RAT file input
          fileInput(
            "ratFile",
            "Choose CSV File for Return Air Temperature (RAT)",
            accept = c("text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv")
          ),
          #Fan Status file input
          fileInput(
            "fanFile",
            "Choose CSV File for Fan Status",
            accept = c("text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv")
          ),
          #Occupancy file input
          fileInput(
            "occFile",
            "Choose CSV File for Occupancy Schedule",
            accept = c("text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv")
          ),
          #Date Range Selection
          uiOutput("date_range"),
          actionButton("update_plot", label = "Update Plot!", width = '100%')
          
        )#div
      ),
      #sidebarPanel
      
      mainPanel(
        conditionalPanel(condition = "output.plot == null",
                         htmlOutput('start_instructions')),
        
        
        #Plot output
        plotlyOutput("plot", height = '500px'),
        
        #Trend Checkboxes
        conditionalPanel(
          condition = "output.show_Plot_Options",
          
          div(style = "display: inline-block;vertical-align:top; width: 133px;",
              checkboxInput(
                "DATCheckbox", label = "DAT", value = TRUE
              )),
          div(style = "display: inline-block;vertical-align:top; width: 133px;",
              checkboxInput(
                "MATCheckbox", label = "MAT", value = TRUE
              )),
          div(style = "display: inline-block;vertical-align:top; width: 133px;",
              checkboxInput(
                "OATCheckbox", label = "OAT", value = TRUE
              )),
          div(style = "display: inline-block;vertical-align:top; width: 133px;",
              checkboxInput(
                "RATCheckbox", label = "RAT", value = TRUE
              )),
          div(
            style = "display: inline-block;vertical-align:top; width: 133px;",
            checkboxInput("fan_statusCheckbox", label = "FAN", value = TRUE)
          ),
          
          div(
            style = "display: inline-block;vertical-align:top; width: 133px;",
            conditionalPanel(
              condition = "output.show_occ_legend",
              tags$img(
                src = "https://raw.githubusercontent.com/cunybpl/oac-shiny/master/oac-shiny/img/occ_legend.png",
                height =
                  "150px",
                width = "133px"
              )
            )
          ),
          textInput(
            'plot_title',
            'Plot Title',
            value = 'Outside Air Control',
            placeholder = 'Your Plot Title Here'
          ),
          
          actionButton("update_plot2", label = "Update Plot!", width = 800)
        ),
        
        br()
      )
    ),
    
    occupancyTab
    
  )
