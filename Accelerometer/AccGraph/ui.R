library(shiny)
library(plotly)
library(ggplot2)
library(shinyjs)

ui <- fluidPage(
    useShinyjs(),
    titlePanel("Processing Raw Accelerometer Data"),

    sidebarLayout(
        sidebarPanel(
          tags$style(HTML("
            .box {
              border: 1px solid #ccc;
              padding: 10px;
              margin-bottom: 5px;
            }
            .box-title {
              font-weight: bold;
              margin-bottom: 5px;
            }
            
          ")),
          
          div(class = "box",
              div(class = "box-title", "Upload File"),
              fileInput("file1", "Choose CSV File",
                        accept = c("text/csv",
                                   "text/comma-separated-values,text/plain",
                                   ".csv")
              )
          ),
          
          div(class = "box",
              div(class = "box-title", "Acceleration Adjustment"),      
              
              actionButton("apply_base_modal", "Apply Adjustment"),
              actionButton("base_range_modal", "Add Peaks to Remove"),
        
              uiOutput("range_checklist"),
              actionButton("remove_selected", "Remove Selected Ranges"),
              
              actionButton("remove_peaks", "Remove Selected Peaks"),
              actionButton("reset_graph", "Reset Graph"),
              actionButton("calc_metrics", "Calculate Metrics")
          )
              
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotlyOutput("plot"),
            verbatimTextOutput("metrics")
        )
    )
)
