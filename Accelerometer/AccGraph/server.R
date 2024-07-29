library(shiny)
library(dplyr)
library(tidyr)
library(pracma)
library(plotly)
library(shinyalert)

server <- function(input, output, session) {
  
  df <- reactiveVal(NULL)
  
  # Read the csv file inputted by user
  observe({
    req(input$file1)
    
    raw_data <- read.csv(input$file1$datapath, sep=",", header=TRUE)
  
    date_str <- paste(Sys.Date()," 00:")
    
    # Calculate net acceleration
    raw_data <- raw_data[, 3:6] %>%
      rename(Time = Chip.Time.., Acc_X = Acceleration.X.g., 
             Acc_Y = Acceleration.Y.g., Acc_Z = Acceleration.Z.g.) %>%
      mutate(Time = as.POSIXct(paste(date_str, Time), 
                               format = "%Y-%m-%d %H:%M:%OS")) %>%
      mutate(net_acceleration = sqrt(Acc_X^2 + Acc_Y^2 + Acc_Z^2)) 
    
    raw_data <- na.omit(raw_data)
    
    df(raw_data)
  })
  
  # Render the Net Acceleration Plot
  output$plot <- renderPlotly({
    req(df())
    df_plot <- df()
    
    plot_ly(df_plot, x = ~Time, y= ~net_acceleration, 
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
  
  
  observeEvent(input$apply_base_modal, {
    req(df())
    df <- df()
    
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
    
    # round to 0 if number is really small to account for leftovers of baseline adjustment
    df$Adj_Net_Acc[df$Adj_Net_Acc <= 0.1] <- 0
    
    # update the plot with the adjusted acceleration
    output$plot <- renderPlotly({
      plot_ly(df, x = ~Time, y= ~Adj_Net_Acc,
              type = 'scatter', mode = 'lines') %>%
        layout(title = "Adjusted Net Acceleration Over Time",
               xaxis = list(title = "Time"),
               yaxis = list(title = "Acceleration (g)"))
    })
    
    df(df)  
  })
  
  # Render the plot within the modal
  output$base_range_plot <- renderPlotly({
    df <- df()
    req(df)
    
    # Initially, mode=lines but for some reason, plotly_selected doesn't
    # recognize the selected points. Upon further investigation, discovered
    # that points aren't recognized if we're using mode=lines.
    
    # Workaround is using mode="lines+markers" but making the markers invisible
    # by setting the opacity property to 0.
    p <- plot_ly(df, x = ~Time, y = ~Adj_Net_Acc, type = 'scatter', 
                 mode='lines+markers', marker=list(opacity=0)) %>%
      layout(title = "Select Peaks to Remove", xaxis = list(title = "Time"), 
             yaxis = list(title = "Acceleration (g)"), dragmode = "select")
    
    event_register(p, "plotly_selected")
    
    p
  })
  
  #############################################################################
  
  ranges_to_remove <- reactiveValues(selected = list())
  
  # Capture selected data range
  observeEvent(input$apply_time_range, {
    selected_data <- event_data("plotly_selected")
    
    if (!is.null(selected_data) && nrow(selected_data) > 0) {
      raw_range <- range(selected_data$x)
      start_time <- as.POSIXct(raw_range[1])
      end_time <- as.POSIXct(raw_range[2])
      
      #all_range <- seq(from=start_time, to=end_time, by=0.01)
      all_range <- c(start_time, end_time)
      ranges_to_remove$selected <- c(ranges_to_remove$selected, list(all_range))
    }
    removeModal()
  })
  
  
  output$baseline_range <- renderText({
    sel_data <- ranges_to_remove$selected
    
    if (length(sel_data) > 0) {
      result <- "Selected time range(s):\n"
    
      result <- paste0(result, 
                      paste(unlist(lapply(sel_data, function(data) {
                        start_time <- format(as.POSIXct(data[1]), format="%H:%M:%OS")
                        end_time <- format(as.POSIXct(data[2]), format="%H:%M:%OS")
                        paste(start_time, "to", end_time)
                      })), collapse = "\n"), 
                      collapse = "\n")
      return(result)
      
    } else {
      return("No time range selected")
    }
  })
  
  observeEvent(input$remove_peaks, {
    df <- df()
    req(df)
    
    sel_data <- ranges_to_remove$selected
    
    if (length(sel_data) > 0) {
      to_remove <- bind_rows(lapply(sel_data, function(data) {
        start_time <- as.POSIXct(data[1], format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")
        end_time <- as.POSIXct(data[2], format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")
        data.frame(time_point = seq(from = start_time, to = end_time, by = 0.1))
      }))
      
      # Columns to be modified
      cols_to_modify <- c("Adj_Acc_X", "Adj_Acc_Y", "Adj_Acc_Z", "Adj_Net_Acc")
  
      # Update the df data frame based on the to_remove time ranges
      for (col in cols_to_modify) {
        df[[col]][df$Time %in% to_remove$time_point] <- 0
      }
      
      # Save the updated df
      df(df)
      
      output$plot <- renderPlotly({
        plot_ly(df, x = ~Time, y= ~Adj_Net_Acc,
                type = 'scatter', mode = 'lines') %>%
          layout(title = "Adjusted Net Acceleration Over Time",
                 xaxis = list(title = "Time", tickformat = "%H:%M:%S"),
                 yaxis = list(title = "Acceleration (g)"))
      })
    
    } else {
      shinyalert(
        title = "Error",
        text = "No ranges selected to be removed!",
        type = "error")
    }
  })
}
