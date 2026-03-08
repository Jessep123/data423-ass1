#DUmping ground

#OLD rising value chart transformation
# Removing NA per column and sort
sorted_list <- lapply(df, function(col) {
  sort(na.omit(col))
})

# Remove empty columns
sorted_list <- sorted_list[sapply(sorted_list, length) > 0]
req(length(sorted_list) > 0)

#Shortest column length (column with most NA values)
min_len <- min(sapply(sorted_list, length))
browser()

df <- sapply(sorted_list, function(col) col[1:min_len], simplify = "matrix")

#Old reactive dataset
# data_reactive <- reactive({
#   df <- data
#   
#   #Selecting filter columns that are also in the selected dataframe
#   active_filter_columns <- intersect(filter_cols, input$selected_vars_categorical)
# 
#   
#   #Iterating through list in global.r of variable filter objects
#     for (col in active_filter_columns){
# 
#     input_name <- paste0("selected_", tolower(col))
#     if(!is.null(input[[input_name]])){
#       selected_vals <- input[[input_name]]
#   
#        #Filtering dataframe for selected variable values and controlling for NA as well
#       df <- df %>%  filter((.data[[col]] %in% selected_vals)| (is.na(.data[[col]]) & "NA" %in% selected_vals))
# 
#     }
#     }
#   
#   #Filtering for selected columns in dataframe
#   selected_vars <-  c(input$selected_vars_categorical, input$selected_vars_numeric)
#   
#   df <-  df %>% select(all_of(selected_vars))
# })
