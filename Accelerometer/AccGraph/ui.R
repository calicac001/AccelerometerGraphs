library(shiny)
library(plotly)
library(ggplot2)
library(shinyjs)
library(bslib)

ui <- navbarPage(
    title = "Processing Raw Accelerometer Data",
    theme = bs_theme(bootswatch = "flatly"),
    
    header = tagList(
      useShinyjs(),
      
      tags$script(HTML("
        $(document).on('shiny:bound', function(event) {
          $('#file1_progress .progress-bar').addClass('bg-success');
        });
      "))
    ),
    
    tabPanel("Home",
      sidebarLayout(
          sidebarPanel(
            card(
              class = "card text-white bg-info mb-3",
              card_body(
                fileInput("file1", "Choose File",
                          accept = c("text/csv",
                                     "text/comma-separated-values","text/plain",
                                     ".csv", ".txt"))
                
              )
            ),
            
            card(
              class = "card text-white bg-secondary mb-3",
              card_header("Acceleration Adjustment"),
              card_body(  
                div(class = "d-flex justify-content-between",
                  actionButton("apply_base_modal", "Apply", class = "btn-success"),
                  actionButton("reset_graph", "Reset", class = "btn-danger")
                ),
              )
            ),
            
            card(
              class = "card text-white bg-secondary mb-3",
              card_header("Peak Selection"),
              card_body(
                div(class = "d-flex justify-content-between",
                  actionButton("base_range_modal", "Add Range", class = "btn-success"),
                  actionButton("remove_selected", "Delete Selected", class = "btn-danger"),
                ),
                uiOutput("range_checklist"),
                actionButton("remove_peaks", "Remove From Graph", class = "btn-info"),
              )
            ),
            
            card(
              class = "card text-white bg-primary mb-3",
              card_body(
                actionButton("calc_metrics", "Calculate Metrics")
              )
            )
          ),

          # Show a plot of the generated distribution
          mainPanel(
            plotlyOutput("plot"),
            verbatimTextOutput("metrics")
          )
      )
    ),
    
    tabPanel("Instructions",
             verticalLayout(
               card(
                 class = "card text-white bg-secondary mb-3",
                 card_header("Uploading a File"),
                 card_body(
                   tags$ul(
                     tags$li("Raw files from WitMotion can be uploaded directly. 
                             No preprocessing of data necessary."), 
                     tags$li("Note that only CSV files are accepted formats."), 
                     tags$li("Once a file has been uploaded, a plot of the raw 
                             net accelation is displayed. To download it, hover
                             over the plot and click the camera icon on the top
                             right."),
                     div(
                       tags$img(src="https://e24150541d5a474ca2894ad78887b85e.app.posit.cloud/file_show?path=%2Fcloud%2Fproject%2FAccelerometer%2Fcamera.png", width = "50%"),
                       style = "text-align: center;"
                     )
                   )
                 )
               ),
               card(
                 class = "card text-white bg-secondary mb-3",
                 card_header("Acceleration Adjustment"),
                 card_body(
                   tags$ul(
                     tags$li("Baseline adjustment is done automatically by 
                             clicking ", tags$mark("Apply"), "."),
                     tags$li("Click ", tags$mark("Reset"), " to return to the raw plot.")
                   )
                 )
               ),
               card(
                 class = "card text-white bg-secondary mb-3",
                 card_header("Peak Selection"),
                 card_body(
                   tags$ul(
                     tags$li("This option allows the user to select peaks that 
                             they want to exclude from the graph i.e. 
                             acceleration recorded at the beginning when the 
                             accelerometer is being placed in the PTS."),
                     tags$li("Button Options:"),
                     tags$ul(
                       tags$li(tags$mark("Add Range"), "- will open a dialog box of the plot
                               wherein the user can select the peak to exclude.
                               Selected ranges will then be displayed as 
                               toggleable checkboxes."),
                       tags$li(tags$mark("Delete Selected"), "- selected ranges 
                                will be removed from the list."),
                       tags$li(tags$mark("Remove from Graph"), "- selected
                               ranges will be removed from the plot and set 
                               to zero.")
                     ),
                     tags$li("Note: Peak selection must be done by dragging the 
                             crosshair across the x-axis/time range of the 
                             chosen peak in order to capture all the datapoints."),
                     tags$ul(
                       tags$li("Correct selection (left photo) is indicated by 
                              two white rectangles in the border of the selection."),
                       tags$li("Incorrecet selection (right photo) does not 
                               capture all points and does not show two rectangles 
                               in the border of the selection box.")
                     ),
                     div(
                       tags$img(src="https://e24150541d5a474ca2894ad78887b85e.app.posit.cloud/file_show?path=%2Fcloud%2Fproject%2FAccelerometer%2Fselection.png", width = "50%"),
                       style = "text-align: center;"
                     ),
                     tags$li("To return a removed peak, the plot must be reverted 
                             to the original by clicking", tags$mark("Reset"), 
                             "in Acceleration Adjustment")
                   )
                 )
               ),
               card(
                 class = "card text-white bg-secondary mb-3",
                 card_header("Metric Calculations"),
                 card_body(
                   tags$ul(
                     tags$li("Clicking ", tags$mark("Calculate Metrics"), "gives 
                             five outputs: AUC, Mean Acceleration, Median
                             Acceleration, Max Acceleration, and Time."),
                     tags$li("Calculations are based only on the peaks and not
                             where the acceleration is equal to zero.")
                   )
                 )
               )
             ))
)
