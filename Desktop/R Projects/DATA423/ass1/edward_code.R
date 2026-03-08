
output$rv_categorical_filters <- renderUI({
  req(input$rv_categorical_vars)
  
  tagList(lapply(input$rv_categorical_vars, function(var) {
    
    # Get levels including NA
    var_levels <- levels(df[[var]])
    
    # Add explicit NA label if NA exists
    if (any(is.na(df[[var]]))) {
      var_levels <- c(var_levels, "NA")
    }
    
    # Default selection: all EXCEPT NA
    default_selected <- var_levels[var_levels != "NA"]
    
    choices <- levels(df[[var]])
    
    if (any(is.na(df[[var]]))) {
      choices <- c(choices, "NA")
    }
    
    checkboxGroupInput(
      paste0("rv_filter_", var),
      paste("Select levels for", var),
      choices = choices,
      selected = choices
    )
  }))
})


# Filtered data for rising values (full data for obs count)
rv_obs_filtered_data <- reactive({
  data <- df
  
  data <- apply_categorical_filters(data, input$rv_categorical_vars, "rv_filter_")
  data <- apply_date_filter(data, "rv_date_range")
  
  data
})


# Filtered numeric data for plotting
rv_filtered_data <- reactive({
  rv_obs_filtered_data()[, input$rv_numeric_vars, drop = FALSE]
})


# Render rising values plot - FIXED to handle single variable
output$rv_plot <- renderPlot({
  
  data <- rv_filtered_data()
  req(ncol(data) > 0)
  
  data <- data[, sapply(data, is.numeric), drop = FALSE]
  req(ncol(data) > 0)
  
  # Handle single variable case
  if (ncol(data) == 1) {
    
    sorted_vals <- sort(na.omit(data[,1]))
    
    if (length(sorted_vals) < 2) {
      plot(0, 0, type = "n",
           xlab = "Percentile",
           ylab = "Value",
           main = "Insufficient data for Rising Value Plot")
      text(0, 0, "Need at least 2 non-NA values")
      return()
    }
    
    if (input$rv_value_type == "zscore") {
      plot_vals <- scale(sorted_vals)
      ylab_text <- "Standardised Values (Z-score)"
      title_suffix <- "(Standardised Z-Score Values)"
    } else {
      plot_vals <- sorted_vals
      ylab_text <- "Raw Values"
      title_suffix <- "(Raw Values)"
    }
    
    x_vals <- seq(0, 100, length.out = length(plot_vals))
    
    plot(x = x_vals,
         y = plot_vals,
         type = "l",
         lwd = 2,
         col = "blue",
         xlab = "Percentile",
         ylab = ylab_text,
         main = paste("Rising Value Chart -",
                      names(data)[1],
                      title_suffix))
    
  } else {
    
    # Multiple variables - original logic
    data <- data[rowSums(is.na(data)) < ncol(data), ]
    req(nrow(data) > 1)
    
    sorted_list <- lapply(data, function(col) {
      sort(na.omit(col))
    })
    
    min_len <- min(sapply(sorted_list, length))
    sorted_trimmed <- sapply(sorted_list, function(col) col[1:min_len])
    
    if (input$rv_value_type == "zscore") {
      plot_data <- scale(sorted_trimmed)
      ylab_text <- "Standardised Values (Z-score)"
      title_suffix <- "(Standardised Z-Score Values)"
    } else {
      plot_data <- sorted_trimmed
      ylab_text <- "Raw Values"
      title_suffix <- "(Raw Values)"
    }
    
    main_title <- paste("Rising Value Chart", title_suffix)
    x_vals <- seq(1, 100, length.out = nrow(plot_data))
    mypalette <- rainbow(ncol(plot_data))
    
    layout(matrix(c(1, 2), ncol = 2), widths = c(4, 1))
    
    # Main plot
    par(mar = c(5, 4, 4, 2))
    matplot(x = x_vals,
            y = plot_data,
            type = "l",
            lty = 1,
            lwd = 1.5,
            col = mypalette,
            xlab = "Percentile",
            ylab = ylab_text,
            main = main_title)
    
    # Legend
    par(mar = c(5, 0, 4, 2))
    plot.new()
    legend("topleft",
           legend = colnames(plot_data),
           col = mypalette,
           lty = 1,
           lwd = 1.5,
           title = "Key:",
           bty = "n",
           cex = 0.85)
  }
})


output$rv_obs_count <- renderText({
  paste("Showing",
        nrow(rv_obs_filtered_data()),
        "out of",
        nrow(df),
        "observations")
})


# =======