
shinyServer(function(input, output, session) {
  
# ================================================================================
#       RESETTING INPUTS
# ================================================================================
  
  #Action button event to reset all inputs across whole page
  observeEvent(input$reset_input_all, {
    reset("")
  })
  
  #Helper function to reset all filter columns in a page
  reset_filters <- function(label){
    for (col in filter_cols){
      input <- paste0("selected_", tolower(col), "_", label)
      reset(input)
    }
  }

# ================================================================================
#       REACTIVE DATASETS
# ================================================================================
    reactive_dataset <- function(label) {
      
      reactive({
        
        df <- data
        
        # Build dynamic input IDs
        cat_id <- paste0("selected_vars_categorical_", label)
        num_id <- paste0("selected_vars_numeric_", label)
        

        # Select chosen columns
        cat_cols <-  input[[cat_id]]
        num_cols <-  input[[num_id]]
        
        
        #If cat_col input is null, all categorical variables are available for filtering
        if (is.null(cat_cols)){cat_cols <-  filter_cols}
        

        # Apply filters to dataset 
        for (col in filter_cols) {
          
          #Different filter for date 
          if (col == "Date"){
             input_name <- paste0("selected_", tolower(col), "_", label)
             selected_vals <- input[[input_name]]
             
             #If there is a date filter applied
             if (!is.null(selected_vals) && length(selected_vals) > 0){
                   df <- df %>%
                     filter(
                         (.data[[col]] >= selected_vals[1]) &
                         (.data[[col]] <= selected_vals[2]))
                 }
             next
          }
          
          input_name <- paste0("selected_", tolower(col), "_", label)
          selected_vals <- input[[input_name]]

          if (!is.null(selected_vals) && length(selected_vals) > 0) {
            df <- df %>%
              filter(
                (.data[[col]] %in% selected_vals) |
                  (is.na(.data[[col]]) & "NA" %in% selected_vals)
              )
          }
        }
  

        
        
        #Special condition for ggpairs plot
        if (label == "ggpairs"){
        #Selected columns to be return for plot
          #Unique() used to remove double ups of colour/input[[cat_id]]
        selected_vars <- unique(c(
          input[[paste0("selected_vars_date_", label)]],
          input[[cat_id]],
          num_cols,
          #If a colourby variable is selected, return that
          if (input$ggpairs_colourby != "None") input$ggpairs_colourby
        )
        )
        } else if(label == "mosaic"){

          #Only return categorical cols
          selected_vars <- unlist(c(
            cat_cols
          ), use.names = FALSE)
        }
        else if(label == "missing"){
          # Returning date variable always for missing
          selected_vars <-unlist(c(
            "Date",
            input[[cat_id]],
            num_cols
          ), use.names = FALSE)}
        else if (label == "counts_over_time"){
          #Selected columns to be return for plot
          selected_vars <- unlist(c(
            "Date",
            cat_cols,
            num_cols
          ), use.names = FALSE)}
        else {
          #Selected columns to be return for plot
          selected_vars <- unlist(c(
            input[[paste0("selected_vars_date_", label)]],
            input[[cat_id]],
            num_cols
          ), use.names = FALSE)}


        if (length(selected_vars) == 0) {
          return(df[, 0, drop = FALSE])
        }

        df %>% select(all_of(selected_vars))
      })
    }
    
    reactive_missing <- reactive_dataset("missing")
    reactive_rising <- reactive_dataset("rising")
    reactive_ggpairs <- reactive_dataset("ggpairs")
    reactive_boxplot <- reactive_dataset("boxplot")
    reactive_mosaic <- reactive_dataset("mosaic")
    reactive_data <- reactive_dataset("data")
    reactive_counts_over_time <- reactive_dataset("counts_over_time")
    
    #Reactive data table for checking data and testing functions
    output$reactive_table <- renderDataTable({
      reactive_mosaic()
    })
    
  # ================================================================================
  #       DYNAMIC VARIABLE FILTERS
  # ================================================================================
  
    #Function to make dynamic filters
    make_dynamic_filters <- function(label) {

      tagList(lapply(filter_cols, function(col) {
        input_id <- paste0("selected_", tolower(col), "_", label)
        if (col == "Date") {
          dateRangeInput(
            inputId = input_id,
            label = "Date Range",
            start = min(data$Date, na.rm = TRUE),
            end = max(data$Date,na.rm = TRUE),
            min = min(data$Date, na.rm = TRUE),
            max = max(data$Date, na.rm = TRUE)
          )
          
        } else {
          selectizeInput(
            inputId = input_id,
            label = col,
            choices = unique(data[[col]]),
            multiple = TRUE,
            selected = NULL
          )}
      }))
    }

  #UI outputs using dynamic filters function for different tabs
  output$dynamic_filters_missing <- renderUI(make_dynamic_filters("missing"))
  output$dynamic_filters_rising  <- renderUI(make_dynamic_filters("rising"))
  output$dynamic_filters_ggpairs <- renderUI(make_dynamic_filters("ggpairs"))
  output$dynamic_filters_boxplot <- renderUI(make_dynamic_filters("boxplot"))
  output$dynamic_filters_mosaic <- renderUI(make_dynamic_filters("mosaic"))
  output$dynamic_filters_data <- renderUI(make_dynamic_filters("data"))
  output$dynamic_filters_counts_over_time <- renderUI(make_dynamic_filters("counts_over_time"))
  
  
  # ================================================================================
  #       SUMMARIES
  # ================================================================================
  
   output$summary_table <- renderUI({
     data %>% 
       summarytools::dfSummary(col.widths = c(10,80,150,120,120,180,220)) %>%
       summarytools::view(, method = "render")
   })

    output$variable_summary <- renderUI({
      layout_columns(
        value_box("Rows", nrow(data)),
        value_box("Variables", ncol(data)),
        value_box("Numeric Variables", sum(sapply(data, is.numeric))),
        value_box("Categorical Variables", sum(sapply(data, is.factor))),
        value_box("Date Variables", sum(sapply(data, is.Date))),
        col_widths = c(2, 2, 2, 2, 2),
        gap = "10px"
        
      )
    })
    
    output$numeric_summary <- renderTable({
      data %>%
        summarise(across(
          where(is.numeric),
          list(
            Mean = ~ mean(.x, na.rm = TRUE),
            SD   = ~ sd(.x, na.rm = TRUE),
            Min  = ~ min(.x, na.rm = TRUE),
            Max  = ~ max(.x, na.rm = TRUE)
          )
        )) %>%
        pivot_longer(
          everything(),
          names_to = c("Variable", ".value"),
          names_sep = "_"
        ) %>%
        mutate(across(where(is.numeric), ~ round(.x, 2)))
    })
    
    output$data <- renderDataTable({data})
    
    
    output$categorical_summary <- renderDataTable({
      datatable(
      {data %>%
        select(where(~ is.factor(.x))) %>%
        summarise(across(
          everything(),
          list(
            Distinct_Count = ~ n_distinct(.x),
            Missing_Values = ~sum(is.na(.x)),
            Unique_Values = ~ paste(sort(unique(na.omit(.x))), collapse = ", ")
          )
        )) %>%
        tidyr::pivot_longer(
          everything(),
          names_to = c("Variable", ".value"),
          names_sep = "_"
        )},
      
      options = list(pageLength = 12)
      )
      })
    
    
    output$date_summary1 <-renderText({
            date_range <- range(data$Date)
            paste('The date range in this dataset is from', date_range[1], 'to', date_range[2])
    })
    
    output$date_summary2 <-renderText({
      

            date_range <- range(data$Date)
            paste('Over a total of',date_range[2] - date_range[1], 'days')
    }) 
    
# ================================================================================
#       Data Export
# ================================================================================

    data_export_table <- reactive({
      
      df <- reactive_data()
      
      if (input$center_data_export || input$scale_data_export) {
        
        num_cols <- sapply(df, is.numeric)
        
        df[num_cols] <- as.data.frame(
          scale(
            df[num_cols],
            center = input$center_data_export,
            scale  = input$scale_data_export
          )
        )
      }
      
      df
    })
    
    
    #Different export options for datatypes
    
    #Export as csv file
    output$data_export_csv <- downloadHandler(
      filename = function() {
        # Dynamically generate the filename
        paste("Ass1_Shiny_Export-", Sys.Date(), ".csv")
      },
      
      content = function(file) {
        # Write the content to a temporary file path provided by Shiny
        write.csv(data_export_table(), file, row.names = FALSE)
      }
    )
    
    #Export as excel workboox file
    output$data_export_xlsx <- downloadHandler(
      filename = function() {
        # Dynamically generate the filename
        paste("Ass1_Shiny_Export-", Sys.Date(), ".xlsx")
      },
      
      content = function(file) {
        # Write the content to a temporary file path provided by Shiny
        write.xlsx(data_export_table(), file)
      }
    )
    
    #Export as .sav file for SPSS
    output$data_export_spss <- downloadHandler(
      filename = function() {
        # Dynamically generate the filename
        paste("Ass1_Shiny_Export-", Sys.Date(), ".sav")
      },
      
      content = function(file) {
        # Write the content to a temporary file path provided by Shiny
        write_sav(data_export_table(), file)
      }
    )
    
    output$data_export_tsv <- downloadHandler(
      filename = function() {
        # Dynamically generate the filename
        paste("Ass1_Shiny_Export-", Sys.Date(), ".tsv")
      },
      
      content = function(file) {
        # Write the content to a temporary file path provided by Shiny
        write_tsv(data_export_table(), file)
      }
    )
    
    output$data_export_rds <- downloadHandler(
      filename = function() {
        # Dynamically generate the filename
        paste("Ass1_Shiny_Export-", Sys.Date(), ".rds")
      },
      
      content = function(file){
        saveRDS(data_export_table(), file)
      }
    )
    
    #Data table output for viewing
    output$data <- renderDataTable({
      data_export_table()
    })
    
    #Reset data export inputs
    observeEvent(input$reset_input_data, {
      reset("center_data_export")
      reset("scale_data_export")
      reset("selected_vars_numeric_data")
      reset("selected_vars_categorical_data")
      reset_filters("data")
    })
    
    #Reset only filter inputs
    observeEvent(input$reset_filter_input_data, {
      reset_filters("data")
    })
    

    
# ================================================================================
#       CORRELATION CHART
# ================================================================================

    output$corr_chart <- renderPlot({
      
      # req(data_reactive())
      
      df <- data
      
      corrgram(df, 
               order = input$corr_order, 
               abs = input$corr_absolute, 
               cor.method = input$corr_method, 
               main = "Correlation Chart",
               lower.panel = NULL,
               col.regions=colorRampPalette(c("blue","white", "red")),
               label.pos = c(0.5, 0.5),
               text.panel=panel.txt)
    })
    
    #Reset data export inputs
    observeEvent(input$reset_input_corr, {
      reset("corr_method")
      reset("corr_order")
      reset("corr_absolute")
    })
  
    
# ================================================================================
#       Missing Values
# ================================================================================
    
    output$missing_data <- renderPlot({
      
      req(reactive_missing())
      

      df <- reactive_missing()
      
      period_min <- min(df$Date)
      period_max <- max(df$Date)
      
      #Removing date column from dataset if it is not selected
      if (is.null(input$selected_vars_date_missing)){
        df$Date <- NULL
      }
      

      date_cols     <- names(df)[sapply(df, inherits, "Date")]
      factor_cols   <- names(df)[sapply(df, is.factor) & !sapply(df, is.ordered)]
      ordered_cols  <- names(df)[sapply(df, is.ordered)]
      numeric_cols  <- names(df)[sapply(df, is.numeric)]
      
      #Calculating missing % for each variable
      missing_pct <- colMeans(is.na(df)) * 100
      
      #Ordering each group of columns by missing %
      date_cols     <- date_cols[order(missing_pct[date_cols], decreasing = TRUE)]
      factor_cols   <- factor_cols[order(missing_pct[factor_cols], decreasing = TRUE)]
      ordered_cols  <- ordered_cols[order(missing_pct[ordered_cols], decreasing = TRUE)]
      numeric_cols  <- numeric_cols[order(missing_pct[numeric_cols], decreasing = TRUE)]
      
      #Specifying specific order so factor cols and ordered factor cols are together
      new_order <- c(date_cols, factor_cols, ordered_cols, numeric_cols)
      
      #Applying my new special order
      df <- df[, new_order]


      #Setting colour/legend inputs depending on if distinct datatypes selected or not
      if (input$distinct_datatypes_missing){
        dtype_colours <-  datatype_colours
        na_value <- "red"
        
        legend_position <- "right"
        
        legend_breaks <- names(dtype_colours)
        #Adjusting column labels to include missing %
        names(df) <-  paste0(names(df)," (", round(missing_pct[names(df)], 0),"%)")
        
        
        viz <- vis_dat(df, sort_type = FALSE)
        scale_fill <- scale_fill_manual(
          
          values = dtype_colours,
          
          breaks = legend_breaks,
          
          na.value = na_value)
        

        
      } else{
        na_value = "red"
        legend_position <- "right"
        scale_fill <-  scale_fill_manual(
          values = c("FALSE" = "grey80", "TRUE" = "red"),
          labels = c("Not Missing", "Missing"),
          name = "Value Status"
        )
        
        viz <- vis_miss(df, sort_miss = FALSE, show_perc = FALSE)
      }
      
        
      
      #Visualizing dataframe with new order and sort_type = FALSE
      #This way factor/ordered factors are side by side in the final plot
    viz +
        labs(title = paste0("Missing Data \n",
                            period_min,
                            " - ",
                            period_max)) +
        scale_fill + #datatype colours list found in global.r
        
      theme(
        axis.text.x = element_text(
          angle = 70,
          hjust = 1,
          vjust = 0.5),
        plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
        plot.margin = margin(t = 50),
        legend.position = legend_position) 

      

    })
    
    #Reset missing chart inputs
    observeEvent(input$reset_input_missing, {
      reset("distinct_datatypes_missing")
      reset("selected_vars_numeric_missing")
      reset("selected_vars_categorical_missing")
      reset_filters("missing")
    })
    
    #Reset only filter inputs
    observeEvent(input$reset_filter_input_missing, {
      reset_filters("missing")
    })
    

    
# ================================================================================
#       RISING VALUE CHART
# ================================================================================
    #Object for selecting variables in ui

    output$rising_value <- renderPlot({

      req(reactive_rising())
      df <- reactive_rising()

      

      #Different y-axis label depending on scale input
      if (input$center_data == TRUE && input$scale_data == TRUE){
        y_label = "Standardised Values (Z-Score)"
      }
      else if (input$center_data == TRUE && input$scale_data == FALSE){
        y_label = "Centered Values"
      }
      else if (input$center_data == FALSE && input$scale_data == TRUE){
        y_label = "Scaled Values"
      }
      else{y_label = "Unadjusted Values"}
      
      #Plot titles/labels
      title = paste0("Rising Value Chart \n", "Jump Threshold = ", input$jump_threshold, " | ", y_label)
      x_label = "Percentile (%)"

      #Plot if no variables are selected
      if(ncol(df) == 0) {
        #Plot with same axis as others
        plot(
          x = c(0, 100),
          y = c(-3, 3),
          type = "n",
          xlab = x_label,
          ylab = y_label,
          main = title
        )

        #Adding text to plot telling people to select a variable
        text(
          x = 50,
          y = 0,
          labels = "PLEASE SELECT A VARIABLE",
          cex = 1.5,
          font = 2
        )
        return()
      }
      
      

        #Ensuring dataframe has only numeric columns
        df <- df[, sapply(df, is.numeric), drop = FALSE]

        #Ordering each column in ascending order with NA last
        for (col in 1:ncol(df)) {
          df[,col] <- df[order(df[,col]),col] 
        }

        #Scaling data 
        df <- scale(
          df,
          center = input$center_data,
          scale  = input$scale_data
        )
        
        #Only keeping variables with a jump in values
        jump_threshold <- input$jump_threshold
        
        #Function to filter out columns that don't meet the jump threshold
        #Default - 0.5
        keep_cols <- sapply(1:ncol(df), function(i) {
          
          col <- na.omit(df[, i])

          col_sorted <- sort(col)
          
          max_jump <- max(diff(col_sorted))
          sd_col   <- sd(col_sorted)
          
          jump_score <- max_jump / sd_col   # scale-independent
          
          return(jump_score > jump_threshold)
        })
        
        #Removing columns that don't meet jump threshold
        df <- df[, keep_cols, drop = FALSE]

        # x_vals <- (1:nrow(df)) / nrow(df) * 100
        x_vals <- seq(0, 100, length.out = nrow(df))
        

      mypalette <- rainbow(ncol(df))

      #Plotting function for single selected variable
      if (ncol(df) == 1){
        plot(x = x_vals,
                y = df,
                type = "l",
                xlab = x_label,
                ylab = y_label,
                lty = 1,
                lwd = 1,
                col = mypalette,
                main = title,
                sub = paste0("Variable - ", colnames(df))
             ) + 
          theme(
            plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.margin = margin(t = 50)
          )
        
        #Control if no variables are selected
      } else { #PLotting for all other variables
      
      matplot(x = x_vals,
              y = df,
              type = "l",
              xlab = x_label,
              ylab = y_label,
              lty = 1,
              lwd = 1,
              col = mypalette,
              main = title,
              sub = paste0(ncol(df), " Variables")) +
          theme(
            plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.margin = margin(t = 50)
          )
        

      legend(legend = colnames(df),
             x = "topleft",
             y = "top",
             lty = 1,
             lwd = 1,
             col = mypalette,
             ncol = round(ncol(df)^0.3))
            }
    })
    
    #Reset rising value inputs
    observeEvent(input$reset_input_rising, {
      reset("jump_threshold")
      reset("selected_vars_numeric_rising")
      reset("center_data")
      reset("scale_data")
      reset_filters("rising")
    })
    
    #Reset only filter inputs
    observeEvent(input$reset_filter_input_rising, {
      reset_filters("rising")
    })
# =================================================================================
#       GG PAIRS PLOT
# =================================================================================

    output$ggpairs <- renderPlot({
      
      df <- reactive_ggpairs()
      
      req(ncol(df) > 0)
      title <-  paste0("Pairs Plot of Data \n", ncol(df), "/31 Variables Selected")
      
      colour_var <- input$ggpairs_colourby
      
      theme <- theme(
                plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
                plot.margin = margin(t = 50)
                    ) 
      
      if (colour_var != "None") {
        
        cols_to_plot <- setdiff(names(df), colour_var)
        
        ggpairs(
          df,
          columns = cols_to_plot,
          mapping = aes(colour = .data[[colour_var]]),
          progress = FALSE,
          title = title
        ) +
          theme
        
      } else {
        
        ggpairs(
          df,
          progress = FALSE,
          title = title
        ) +
          theme
      }
      

    })
    
    #Reset ggpairs value inputs
    observeEvent(input$reset_input_ggpairs, {
      reset("selected_vars_numeric_ggpairs")
      reset("selected_vars_categorical_ggpairs")
      reset("ggpairs_colourby")
      reset_filters("ggpairs")
    })
    
    #Reset only filter inputs
    observeEvent(input$reset_filter_input_ggpairs, {
      reset_filters("ggpairs")
    })
    
# =================================================================================
        # BOXPLOT
# =================================================================================
  output$boxplot <-  renderPlotly({
    df <-  reactive_boxplot()
    
    #Removing categorical variables
    df <- df[, sapply(df, is.numeric), drop = FALSE]
    req(ncol(df) > 0)
    
    #Different y-axis label depending on scale input
    if (input$center_data_boxplot == TRUE && input$scale_data_boxplot == TRUE){
      x_label = "Standardised Values (Z-Score)"
    }
    else if (input$center_data_boxplot == TRUE && input$scale_data_boxplot == FALSE){
      x_label = "Centered Values"
    }
    else if (input$center_data_boxplot == FALSE && input$scale_data_boxplot == TRUE){
      x_label = "Scaled Values"
    }
    else{x_label = "Unadjusted Values"}
    
    #Plot titles/labels
    y_label = paste0("Variable (", ncol(df), "/31 Selected)")
    
    title = paste0("Rising Value Chart \n", x_label, " | ", ncol(df), "/31 Variables Selected")


    #Scaling/centering data depending on input
    if(input$center_data_boxplot || input$scale_data_boxplot) {
      df <- as.data.frame(scale(df, 
                                  center = input$center_data_boxplot, 
                                  scale = input$scale_data_boxplot))
    }
    
    iqr_multiplier <-  input$iqr_boxplot
    

    
    # Convert to long format
    df_long <- tidyr::pivot_longer(
      df,
      cols = everything(),
      names_to = "Variable",
      values_to = "Value"
    )
    

    
    iqr_multiplier <- input$iqr_boxplot
    
    #Calculating IQR ranges manually using multiplier input
    df_stats <- df_long %>%
      group_by(Variable) %>%
      summarise(
        q1 = quantile(Value, 0.25, na.rm = TRUE),
        q3 = quantile(Value, 0.75, na.rm = TRUE),
        median = median(Value, na.rm = TRUE),
        iqr = IQR(Value, na.rm = TRUE),
        lower = q1 - iqr_multiplier * iqr,
        upper = q3 + iqr_multiplier * iqr
      )
    
    
    #Identifying outliers based on IQR multiplier range
    outliers <- df_long %>%
      left_join(df_stats, by = "Variable") %>%
      filter(Value < lower | Value > upper)
    
    #Plotting using plotly 
    plot_ly(
      type = "box",
      orientation = "h",
      q1 = df_stats$q1,
      median = df_stats$median,
      q3 = df_stats$q3,
      lowerfence = df_stats$lower,
      upperfence = df_stats$upper,
      y = df_stats$Variable
    ) %>%
      #Adding IQR determined outliers
      add_markers(
        data = outliers,
        x = ~Value,
        y = ~Variable,
        marker = list(color = "red", size = 6),
        name = "Outliers",
        inherit = FALSE
      ) %>% 
      #Formatting layout of visual
      layout(
        title = list(
                    text = paste0("<b>", title, "</b>"),
                    x = 0.5
                     ),
        xaxis = list(
                     title = list(text = x_label, standoff = 30)
                    ),
        yaxis = list(
                    title = list(text = y_label)
                    ),
        margin = list(t = 80)
      )
    
  })
    #Reset boxplot value inputs
    observeEvent(input$reset_input_boxplot, {
      reset("selected_vars_numeric_boxplot")
      reset("center_data_boxplot")
      reset("scale_data_boxplot")
      reset("iqr_boxplot")
      reset_filters("boxplot")
    })
    
    #Reset only filter inputs
    observeEvent(input$reset_filter_input_boxplot, {
      reset_filters("boxplot")
    })
    
# =================================================================================
#         MOSAIC
# =================================================================================
      output$mosaic <-  renderPlot({
        
        df <- reactive_mosaic()
        req(!is.null(df))
        
        #Defining formula for mosaic plot
        if (input$selected_vars_mosaic_z == "None" && is.null(input$selected_vars_mosaic_xyz)){
        formula <- as.formula(
                            paste("~",
                            input$selected_vars_mosaic_x,
                            "+",
                            input$selected_vars_mosaic_y)
                             )
        #If a third variable is selected 
        } else if (input$selected_vars_mosaic_z != "None" &&  is.null(input$selected_vars_mosaic_xyz)){
          formula <- as.formula(
            paste("~",
                  input$selected_vars_mosaic_x,
                  "+",
                  input$selected_vars_mosaic_y,
                  "+",
                  input$selected_vars_mosaic_z)
          )
          
          #If 4+ variables are selected
        }else if (!is.null(input$selected_vars_mosaic_z) && !is.null(input$selected_vars_mosaic_xyz)){
          extra_vars <- 
          
          formula <- as.formula(
            paste("~",
                  input$selected_vars_mosaic_x,
                  "+",
                  input$selected_vars_mosaic_y,
                  "+",
                  input$selected_vars_mosaic_z,
                  "+",
                  paste(input$selected_vars_mosaic_xyz, collapse = " + "))
          )
        } 
        
        plot <- xtabs(formula, data = df)
        

        #Plotting mosaic
        mosaic(plot,
               shade = TRUE,
               legend = TRUE,
               # split_vertical = TRUE, 
               # highlighting = input$selected_vars_mosaic_x,
               main = paste("Mosaic Plot",
                            paste(deparse(formula)))
        )
          
          # theme_minimal()
      })
    
    #Reset mosaic value inputs
    observeEvent(input$reset_input_mosaic, {
      reset("selected_vars_mosaic_x")
      reset("selected_vars_mosaic_y")
      reset("selected_vars_mosaic_z")
      reset("selected_vars_mosaic_xyz")
      reset_filters("mosaic")
    })
    
    #Reset only filter inputs
    observeEvent(input$reset_filter_input_mosaic, {
      reset_filters("mosaic")
    })
    
# =================================================================================
# Scatter Plot
# =================================================================================
    output$counts_over_time <-  renderPlotly({
      df <-  reactive_counts_over_time()
      period_min <-  min(df$Date)
      period_max <-  max(df$Date)
      


      plot_data <- df %>%
        tidyr::pivot_longer(
          cols = where(is.numeric),
          names_to = "Variable",
          values_to = "Value"
        )
      
      #Colour of Points
      colour <-  input$counts_colourby
      
      #If no colour selected, mapping aesthetic has no colour
      if (input$counts_colourby == "None"){
        plot <-  ggplot(plot_data,
                        aes(x = Date,
                            y = Value
                        )) 
        #If colour is selected, mapping colours points based on selected variable
      } else{
        plot <-  ggplot(plot_data,
                        aes(x = Date,
                            y = Value,
                            colour = .data[[colour]]
                        )) 
      }
      
      #Creating final plot
      plot  <-  plot +
      
        geom_point() +
        
        labs(
          x = "Date",
          y = "Value",
          title = paste0("Numeric Variable Values by Date \n", period_min, " - ", period_max)
        ) +
        scale_x_date(
          date_breaks = "6 months",
          date_labels = "%b\n%Y"
        ) +
        theme_minimal()+
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.margin = margin(t = 50)
        )
      
      #Rendering plot as plotly object
    ggplotly(plot) 
    
    })
    
    #Reset boxplot value inputs
    observeEvent(input$reset_input_counts_over_time, {
      reset("selected_vars_numeric_counts_over_time")
      reset("selected_vars_categorical_counts_over_time")
      reset("selected_vars_date_counts_over_time")
      reset("selected_vars_date_counts_over_time")
      reset_filters("counts_colourby")

    })
    
    #Reset only filter inputs
    observeEvent(input$reset_filter_input_counts_over_time, {
      reset_filters("counts_over_time")
    })
} 

)
