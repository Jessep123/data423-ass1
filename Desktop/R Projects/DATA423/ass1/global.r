
library(shiny)
library(shinyjs)
library(bslib)
library(ggplot2)
# install.packages("DT")
library(DT)
library(dplyr)
# install.packages("psych")
library(psych)
# install.packages("gtsummary")
library(gtsummary)
# install.packages("vtable")
library(vtable)
library(tidyr)
library(readr)
library(lubridate)
library(summarytools)
library(corrgram)
library(visdat)
library(shinyWidgets)
library(GGally)
library(vcd)
library(openxlsx)
library(haven)
library(sass)
library(plotly)
#Reading in dataset
data <- read.csv('Ass1Data.csv',
                 stringsAsFactors = TRUE)

#Setting ID column as data index
# row.names(data) <- data$ID

#Setting and formatting date column
data$Date <- as.Date(data$Date, format = "%d/%m/%Y")

#Setting order priority for ordinal categorical values
data$Priority <- factor(
  data$Priority,
  levels = c("Low", "Medium", "High"),
  ordered = TRUE
)

data$Price <- factor(
  data$Price,
  levels = c("Cheap", "Fair", "Expensive"),
  ordered = TRUE
)

data$Speed <- factor(
  data$Speed,
  levels = c("Slow", "Medium", "Fast"),
  ordered = TRUE
)

data$Duration <- factor(
  data$Duration,
  levels = c("Short", "Long", "Very Long"),
  ordered = TRUE
)

data$Temp <- factor(
  data$Temp,
  levels = c("Cold", "Warm", "Hot"),
  ordered = TRUE
)


# Global datatype colours to be used in server/ui
datatype_colours <- c(
  "factor"    = "#2ca02c",
  "numeric"   = "#1f77b4",
  "integer"   = "#1f77b4",
  "Date"      = "#9467bd",
  "ordered\nfactor" = "#74c476"
)
datatype_colours_grey <- c(
  "character" = "grey",
  "factor"    = "grey",
  "numeric"   = "grey",
  "integer"   = "grey",
  "Date"      = "grey",
  "ordered\nfactor" = "grey"
)



#Object for selecting variables in ui
cat_vars <- data[, sapply(data, is.factor), drop =FALSE]
select_variables_categorical <- pickerInput(
  inputId = "selected_vars_categorical",
  label = "Categorical Variables",
  choices = names(cat_vars),
  selected = names(cat_vars),
  multiple = TRUE,
  options = list(
    `actions-box` = TRUE,   # adds Select All / Deselect All
    `live-search` = TRUE    # search bar
  )
)

#Object for selecting variables in ui
non_cat_vars <- data[, (sapply(data, is.numeric)), drop =FALSE]
select_variables_numeric <- pickerInput(
  inputId = "selected_vars_numeric",
  label = "Numeric Variables",
  choices = names(non_cat_vars),
  selected = names(non_cat_vars),
  multiple = TRUE,
  options = list(
    `actions-box` = TRUE,
    `live-search` = TRUE
  )
)


#Globally defining filter columns for use in generating tables
filter_cols <-c("Date",
                "Operator",
                "Surface",
                "State",
                "Class",
                "Agreed",
                "Priority",
                "Price",
                "Speed",
                "Duration",
                "Temp",
                "Location"
                )


# =================================================================================
#   Rising Value Chart Inputs
# ================================================================================

select_variables_numeric_rising <- pickerInput(
  inputId = "selected_vars_numeric_rising",
  label = "Numeric Variables",
  choices = names(non_cat_vars),
  selected = names(non_cat_vars),
  multiple = TRUE,
  options = list(
    `actions-box` = TRUE,
    `live-search` = TRUE
  )
)
select_variables_categorical_rising <- pickerInput(
  inputId = "selected_vars_categorical_rising",
  label = "Numeric Variables",
  choices = names(cat_vars),
  selected = names(cat_vars),
  multiple = TRUE,
  options = list(
    `actions-box` = TRUE,
    `live-search` = TRUE
  )
)
# =================================================================================
#   Missing Value Chart Inputs
# ================================================================================
select_variables_date_missing <- pickerInput(
  inputId = "selected_vars_date_missing",
  label = "Date",
  choices = "Date",
  selected = "Date",
  multiple = TRUE,
  options = list(
    `actions-box` = FALSE,
    `live-search` = FALSE)
)
select_variables_numeric_missing <- pickerInput(
  inputId = "selected_vars_numeric_missing",
  label = "Numeric Variables",
  choices = names(non_cat_vars),
  selected = names(non_cat_vars),
  multiple = TRUE,
  options = list(
    `actions-box` = TRUE,
    `live-search` = TRUE
  )
)
select_variables_categorical_missing <- pickerInput(
  inputId = "selected_vars_categorical_missing",
  label = "Categorical Variables",
  choices = names(cat_vars),
  selected = names(cat_vars),
  multiple = TRUE,
  options = list(
    `actions-box` = TRUE,   # adds Select All / Deselect All
    `live-search` = TRUE    # search bar
  )
)
# =================================================================================
#   GGPAIRS Inputs 
# ================================================================================
select_variables_date_ggpairs <- pickerInput(
  inputId = "selected_vars_date_ggpairs",
  label = "Date",
  choices = "Date",
  selected = NULL,
  multiple = TRUE,
  options = list(
    `actions-box` = FALSE,
    `live-search` = FALSE)
)
select_variables_numeric_ggpairs <- pickerInput(
  inputId = "selected_vars_numeric_ggpairs",
  label = "Numeric Variables",
  choices = names(non_cat_vars),
  selected = c("sensor4", "sensor8", "sensor11", "sensor16", "sensor22", "sensor24", "sensor28"), #Defaults set to sensors with high rising order jumps
  multiple = TRUE,
  options = list(
    `actions-box` = TRUE,
    `live-search` = TRUE
  )
)
select_variables_categorical_ggpairs <- pickerInput(
  inputId = "selected_vars_categorical_ggpairs",
  label = "Categorical Variables",
  choices = names(cat_vars),
  selected = NULL,
  multiple = TRUE,
  options = list(
    `actions-box` = TRUE,  
    `live-search` = TRUE    
  )
)
# =================================================================================
#   Boxplot Inputs 
# ================================================================================

select_variables_numeric_boxplot <- pickerInput(
  inputId = "selected_vars_numeric_boxplot",
  label = "Numeric Variables",
  choices =  names(non_cat_vars),
  selected = names(non_cat_vars),
  multiple = TRUE,
  options = list(
    `actions-box` = TRUE,
    `live-search` = TRUE
  )
)

select_variables_categorical_boxplot <- pickerInput(
  inputId = "selected_vars_categorical_boxplot",
  label = "Categorical Variables",
  choices = names(cat_vars),
  selected = names(cat_vars),
  multiple = TRUE,
  options = list(
    `actions-box` = TRUE,  
    `live-search` = TRUE    
  )
)
# =================================================================================
#   Mosaic Inputs 
# ================================================================================


select_variables_numeric_mosaic <- pickerInput(
  inputId = "selected_vars_numeric_mosaic",
  label = "Numeric Variables",
  choices = names(non_cat_vars),
  selected = names(non_cat_vars),
  multiple = TRUE,
  options = list(
    `actions-box` = TRUE,
    `live-search` = TRUE
  )
)
select_variables_categorical_mosaic <- pickerInput(
  inputId = "selected_vars_categorical_mosaic",
  label = "Categorical Variables",
  choices = names(cat_vars),
  selected = c("Operator", "Location", "Price"),
  multiple = TRUE,
  options = list(
    `actions-box` = TRUE,  
    `live-search` = TRUE    
  )
)
select_variables_mosaic_x <- pickerInput(
  inputId = "selected_vars_mosaic_x",
  label = "Select X-Axis Variable",
  choices = names(cat_vars),
  selected = "Location",
  multiple = FALSE,
  options = list(
    `actions-box` = TRUE,  
    `live-search` = TRUE    
  )
)

select_variables_mosaic_y <- pickerInput(
  inputId = "selected_vars_mosaic_y",
  label = "Select Y-Axis Variable",
  choices = names(cat_vars),
  selected = "Price",
  multiple = FALSE,
  options = list(
    `actions-box` = TRUE,  
    `live-search` = TRUE    
  )
)

select_variables_mosaic_z <- pickerInput(
  inputId = "selected_vars_mosaic_z",
  label = "Select 3rd Variable (Optional)",
  choices = names(cat_vars),
  selected = "Operator",
  multiple = FALSE,
  options = list(
    `actions-box` = TRUE,  
    `live-search` = TRUE    
  )
  
)
select_variables_mosaic_xyz <- pickerInput(
  inputId = "selected_vars_mosaic_xyz",
  label = "Addtional Variables (Optional - Not Recommended)",
  choices = names(cat_vars),
  selected = NULL,
  multiple = TRUE,
  options = list(
    `actions-box` = TRUE,  
    `live-search` = TRUE    
  )
)
# =================================================================================
#   Data Table Export 
# ================================================================================

select_variables_numeric_data <- pickerInput(
  inputId = "selected_vars_numeric_data",
  label = "Numeric Variables",
  choices = names(non_cat_vars),
  selected = names(non_cat_vars),
  multiple = TRUE,
  options = list(
    `actions-box` = TRUE,
    `live-search` = TRUE
  )
)

select_variables_date_data <- pickerInput(
  inputId = "selected_vars_date_data",
  label = "Date",
  choices = "Date",
  selected = "Date",
  multiple = TRUE,
  options = list(
    `actions-box` = FALSE,
    `live-search` = FALSE)
)

select_variables_categorical_data <- pickerInput(
  inputId = "selected_vars_categorical_data",
  label = "Categorical Variables",
  choices = names(cat_vars),
  selected = names(cat_vars),
  multiple = TRUE,
  options = list(
    `actions-box` = TRUE,  
    `live-search` = TRUE    
  )
)