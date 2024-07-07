library(shiny)
library(dplyr)
library(tidyr)
library(pracma)
library(plotly)

server <- function(input, output, session) {
  
  # Read the csv file inputted by user
  data <- reactive({
    req(input$file1)
    read.csv(input$file1$datapath, sep=",", header=TRUE)
  })
  
  # Process CSV data into a dataframe
  csv_to_df <- reactive({
    req(data())
    
    df <- data()
    date_str <- paste(Sys.Date()," 00:")
    
    # Calculate net acceleration
    df <- df[, 3:6] %>%
      rename(Time = Chip.Time.., Acc_X = Acceleration.X.g., 
             Acc_Y = Acceleration.Y.g., Acc_Z = Acceleration.Z.g.) %>%
      mutate(Time = as.POSIXct(paste(date_str, Time), 
                               format = "%Y-%m-%d %H:%M:%OS")) %>%
      mutate(net_acceleration = sqrt(Acc_X^2 + Acc_Y^2 + Acc_Z^2)) 
    
    df <- na.omit(df)
    df
  })
  
  # Render the Net Acceleration Plot
  output$plot <- renderPlotly({
    req(csv_to_df())
    df <- csv_to_df()
    
    plot_ly(df, x = ~Time, y= ~net_acceleration, 
            type = 'scatter', mode = 'lines') %>%
      layout(title = "Net Acceleration Over Time",
             xaxis = list(title = "Time"),
             yaxis = list(title = "Acceleration (g)"))
  })
  
  #############################################################################
  
  observeEvent(input$base_range_modal, {
    showModal(modalDialog(
      title = "Select Baseline Time Range",
      plotlyOutput("base_range_plot"),
      easyClose = TRUE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("apply_time_range", "Apply")
      )
    ))
  })
  
  # Render the plot within the modal
  output$base_range_plot <- renderPlotly({
    df <- csv_to_df()
    req(df)
    
    # Initially, mode=lines but for some reason, plotly_selected doesn't
    # recognize the selected points. Upon further investigation, discovered
    # that points aren't recognized if we're using mode=lines.
    
    # Workaround is using mode="lines+markers" but making the markers invisible
    # by setting the opacity property to 0.
    p <- plot_ly(df, x = ~Time, y = ~net_acceleration, type = 'scatter', 
                 mode='lines+markers', marker=list(opacity=0)) %>%
      layout(title = "Select Baseline Range", xaxis = list(title = "Time"), 
             yaxis = list(title = "Acceleration (g)"), dragmode = "select")
    
    event_register(p, "plotly_selected")
    
    p
  })
  
  # Capture selected data range
  selected_data <- reactive({
    event_data("plotly_selected")
  })
  
  
  vals <- reactiveValues(baseline_range = NULL)
  
  # Apply selected time range as baseline
  observeEvent(input$apply_time_range, {
    vals$baseline_range <- selected_data()
    removeModal()
  })
  
  output$baseline_range <- renderPrint({
    sel_data <- vals$baseline_range
    if (!is.null(sel_data) && nrow(sel_data) > 0) {
      range <- sel_data$x
      start_time <- format(as.POSIXct(range[1]), "%H:%M:%S")
      end_time <- format(as.POSIXct(range[2]), "%H:%M:%S")
      cat("Selected time range: \n")
      cat(paste(start_time, "to", end_time))
    } else {
      cat("No time range selected")
    }
  })
  
  # Calculate and display baseline acceleration
  output$baseline_acceleration <- renderPrint({
    df <- csv_to_df()
    req(df)
    
    sel_data <- vals$baseline_range
    if (!is.null(sel_data) && nrow(sel_data) > 0) {
      range <- range(sel_data$x)
      baseline_df <- df %>% filter(Time >= range[1] & Time <= range[2])
      baseline_acc <- mean(baseline_df$net_acceleration, na.rm = TRUE)
      cat(paste("Baseline acceleration (mean):", round(baseline_acc, 4), "g"))
    } else {
      cat("No baseline range selected")
    }
  })
  
  # Get baseline acceleration from selected range and apply to the plot
  # Problem with this implementation: doesn't account for dynamic baseline so
  # once the acceleration is applied, not all the flat lines go to zero.
  
  # observeEvent(input$apply_base_modal, {
  #   req(csv_to_df())
  #   df <- csv_to_df()
  #   req(vals$baseline_range)
  #   
  #   # get the baseline data and format date so it matches the one in the df
  #   baseline_data <- vals$baseline_range %>%
  #     mutate(x = as.POSIXct(x, format = "%Y-%m-%d %H:%M:%OS"))
  #   
  #   # filter the dataframe for only the values in selected baseline
  #   df_baseline <- df[df$Time %in% baseline_data$x, ]
  #   
  #   # Get the baseline acceleration of the selected range separately for each axis
  #   mean_acc_x <- mean(df_baseline$Acc_X)
  #   mean_acc_y <- mean(df_baseline$Acc_Y)
  #   mean_acc_z <- mean(df_baseline$Acc_Z)
  #   
  #   # apply the baseline correction to all the values in each axis
  #   df$Adj_Acc_X <- df$Acc_X - mean_acc_x
  #   df$Adj_Acc_Y <- df$Acc_Y - mean_acc_y
  #   df$Adj_Acc_Z <- df$Acc_Z - mean_acc_z
  #   
  #   # calculate adjusted net acceleration
  #   df$Adj_Net_Acc <- sqrt(df$Adj_Acc_Z^2 + df$Adj_Acc_Y^2 + df$Adj_Acc_X^2)
  #   
  #   # update the plot with the adjusted acceleration
  #   output$plot <- renderPlotly({
  #     plot_ly(df, x = ~Time, y= ~Adj_Net_Acc, 
  #             type = 'scatter', mode = 'lines') %>%
  #       layout(title = "Adjusted Net Acceleration Over Time",
  #              xaxis = list(title = "Time"),
  #              yaxis = list(title = "Acceleration (g)"))
  #   })
  # })
  
  observeEvent(input$apply_base_modal, {
    req(csv_to_df())
    df <- csv_to_df()
    req(vals$baseline_range)
    
    num_segment = 50
    for (i in 1:num_segment){
      # filter the dataframe for only the values in the selected range
      start_row <- (nrow(df) %/% num_segment) * (i - 1) + 1
      
      end_row <- min((nrow(df) %/% num_segment) * i, nrow(df))
      
      df_sub <- df[start_row:end_row, ]
      
      # Get the baseline acceleration of the selected range separately for each axis
      condition <- abs(df_sub$net_acceleration - 1) < 0.01
      mean_acc_x <- mean(df_sub$Acc_X[condition])
      mean_acc_y <- mean(df_sub$Acc_Y[condition])
      mean_acc_z <- mean(df_sub$Acc_Z[condition])
      
      # apply the baseline correction to all the values in each axis
      df[start_row:end_row, "Adj_Acc_X"] <- df[start_row:end_row, "Acc_X"] - mean_acc_x
      df[start_row:end_row, "Adj_Acc_Y"] <- df[start_row:end_row, "Acc_Y"] - mean_acc_y
      df[start_row:end_row, "Adj_Acc_Z"] <- df[start_row:end_row, "Acc_Z"] - mean_acc_z
    }
    # calculate adjusted net acceleration
    df$Adj_Net_Acc <- sqrt(df$Adj_Acc_Z^2 + df$Adj_Acc_Y^2 + df$Adj_Acc_X^2)
    
    # update the plot with the adjusted acceleration
    output$plot <- renderPlotly({
      plot_ly(df, x = ~Time, y= ~Adj_Net_Acc,
              type = 'scatter', mode = 'lines') %>%
        layout(title = "Adjusted Net Acceleration Over Time",
               xaxis = list(title = "Time"),
               yaxis = list(title = "Acceleration (g)"))
    })
      
  })
}
