library(shiny)
library(plotly)
library(ggplot2)

ui <- fluidPage(
    titlePanel("Processing Raw Accelerometer Data"),

    sidebarLayout(
        sidebarPanel(
          tags$style(HTML("
            .box {
              border: 1px solid #ccc;
              padding: 10px;
              margin-bottom: 10px;
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
              div(class = "box-title", "Baseline Acceleration"),      
              #textInput("start_time", "Start Time (HH:MM:SS)", "00:00:00"),
              #textInput("end_time", "End Time (HH:MM:SS)", "00:00:00"),
              
              actionButton("base_range_modal", "Select Baseline Acceleration"),
              verbatimTextOutput("baseline_range"),
              actionButton("apply_base_modal", "Apply Acceleration Adjustment"),
          )
              
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotlyOutput("plot"),
            verbatimTextOutput("baseline_acceleration")
        )
    )
)
