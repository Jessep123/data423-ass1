shinyUI(
  page_navbar(
    title = "DATA423 - Assignment 1",
    
    #Setting theme and font
    theme = bs_theme(
      bootswatch = "flatly",
      base_font = "Times New Roman"
      ),
    
    useShinyjs(),
    
    tabPanel('Introduction',
             
             card(
             h3('Welcome to my app!'),
             p("• This app was developed for Data 423 - Data Science in Industry Assignment 1."),
             p("• Each tab has been engineered to encourage dynamic exploratory data analysis of the assignment dataset."),
             p("• The ability to manually select, remove and filter relevant features from visualizations is available where necessary.")
             ),
             
             card(
             h4('Resetting inputs'),
             p("• All tabs contain a 'reset inputs' button in the sidebar. This will reset all inputs to their default values, which are specified below."),
             p("• A 'reset filters' button is also avaiable, allowing for filters to be reset independantly of other parameter adjustments (e.g. scaling/centering)."),
             p("• The red 'Reset Inputs Across all Tabs' button in the top right does exactly what it says. All tabs will be returned to their default setting.")
               ),
             
             card(
             h4('Tab Information'),
             p("• A full breakdown of each tab can be found down below."),
             
             accordion(
               open = FALSE,
               accordion_panel("Summary",
                               p("• The summary panel provides a general overview of the assignment dataset."),
                               p("• Users can find how many rows there are, how many variables, and a breakdown of variable datatypes here."),
                               p("• Further summaries are provided for each datatype as well."),
                               p("• The numeric summary shows mean, standard deviation, minimum, and maximum values for each numeric variable."),
                               p("• The categorical summary shows distinct values for each categorical variable and how many rows are N/A."),
                               p("• The date summary details the date range for the dataset."),
                               p("• An overall summary table has also been plotted using the summarytools::dfSummary package."),
                               p("    • This contains information regarding distinct, valid, and missing values for each variable."),
                               p("    • Value distributions have also been plotted for all variables.")
                               ),
               accordion_panel("Missing Values",
                               p("• This tab displays a missing values plot using viz_dat()."),
                               p("• Values are ordered by date, from earliest to latest, top to bottom."),
                               p("• Any variable can be selected/removed from visual. Default selection is all variables"),
                               p("• Variables ordered by datatypes (date, factor, numeric) with each datatype ordered by % of values missing."),
                               p("• Filters for factor variables or date range can be applied to shrink the visual."),
                               p("• The 'Display Distinct Datatypes' will apply colour coding to visualize different datatypes within the visual. All currently applied filters/selected variables will be kept.")
                               ),
               accordion_panel("Scatter Plot",
                               p("• This tab displays a scatter plot of numeric variable datapoints plotted against date."),
                               p("• By default, all numeric variables are selected."),
                               p("• Datapoints can be coloured by any level of a categorical factor OR what numeric variable they belong to ('Variable' option)."),
                               p("• By default, datapoints are coloured by Operator.")
                                ),
               accordion_panel("Rising Value Chart",
                               p("• This tab displays a rising value chart to visualize any gaps in numeric variable ranges."),
                               p("• By default, all variables have been scaled and centered to a standardized z-score, but this can be adjusted if necessary."),
                               p("• Only numeric variables are available in this visual, but the underlying data can still be filtered by any factor/date variables."),
                               p("• The 'jump threshold' removes variables that don't meet the specified threshold input."),
                               p("• Jump threshold is calculated by dividing the largest gap between values in the variables range by its standard deviation (largest jump / variable standard deviation)."),
                               p("• This provides a standardised jump threshold, regardless of if values are standardised or not."),
                               p("• Default jump threshold = 0.5.")
                              ),
               accordion_panel("Correlations",
                               p("• This tab displays correlations of numeric variables using corrgram()."),
                               p("• Shades of red indicate varying strengths positive correlation, whereas shades of blue indicate varying strengths of negative correlation."),
                               p("• The deeper the shade of red/blue, the stronger the respective correlation."),
                               p("• Correlation type can be either Pearson, Spearman, or Kendall (Default = Pearson)."),
                               p("• Values can be ordered by PCA, OLO, GW, or HC (Default = No order)."),
                               p("• By default, values are standardised, but can be set to original, absolute values if desired.")
                              ),
               accordion_panel("Boxplot",
                               p("• This tab displays a plotly box and whisker plot."),
                               p("• Only numeric variables are available to be selected/removed from the visual, but underlying dataset can still be filtered by date/factor variables."),
                               p("• By default, values have not been standardized, but can be if desired."),
                               p("• The interquartile range (IQR) multiplier can be adjusted to change the threshold at which a value is considered an outlier."),
                               p("• By default the IQR multiplier is set to 1.5")
                                ),
               accordion_panel("ggPairs",
                               p("• This tab displays a ggpairs plot of selected variables."),
                               p("• Any categorical or numeric variable can be selected for this plot."),
                               p("• However, due to the high computational cost of ggPairs plot, default selected variables are limited to seven numeric variables that have a jump threshold higher than 1 in the rising value chart."),
                               p("• The plot can be coloured by any factor variable in the dataset. By default this is set to operator.")
                              ),
               accordion_panel("Mosaic Chart",
                               p("• This tab displays a mosaic chart to identify trends in the frequencies of categorical variables."),
                               p("• Mosaic tiles shaded red represent a combination of categorical variables that are observed more frequently than would be expected if variables were normally distributed."),
                               p("• Alternatively, mosaic tiles shaded blue represent a combination that is observed less frequently than expected."),
                               p("• Only categorical variables are available to be selected for this plot."),
                               p("• Any number of variables can be selected, but the default is set to three."),
                               p("• While you can select more variables to be displayed, it is not recommeneded to do more than four at most.")
                              ),
               accordion_panel("Export Chart",
                               p("• This tab allows for exporting of the assignment dataset."),
                               p("• Any numeric/categorical variables can be selected."),
                               p("• Variables can also be filtered for specific values."),
                               p("• Numeric variables can be scaled and centered to return standardized z-scores."),
                               p("• The data can be exported as either a .csv, .xlsx, .sav, .tsv, or .rds file type."),
                               p("• The dataset which will be exported is also rendered to check desired output.")
                               )
               
             )),
             
             
             ),
    
    tabPanel("Summary",
             h3("Data Summary"),
             uiOutput('variable_summary'),
             accordion(open = FALSE,
               accordion_panel('Numeric Variables Summary', tableOutput('numeric_summary')),
               accordion_panel('Categorical Variables Summary', DTOutput('categorical_summary')),
               accordion_panel('Date Variable Summary', 
                               textOutput('date_summary1'),
                               textOutput('date_summary2')
                               ),
               accordion_panel('Summary', htmlOutput('summary_table'))
               )
    ),

    tabPanel("Missing Values",
             h3("Variable Selection"),
             layout_sidebar(
               
               sidebar = sidebar(
                 title = '',
                 actionButton("reset_input_missing","Reset Inputs"),
                 accordion(open = FALSE,
                           accordion_panel(
                             "Select Variables",
                             select_variables_numeric_missing,
                             select_variables_categorical_missing,
                             select_variables_date_missing),
                           accordion_panel(
                             "Filter Variables",
                             uiOutput("dynamic_filters_missing"),
                             actionButton("reset_filter_input_missing", "Reset Filters"),
                             
                                          )
                          ),
                
                 checkboxInput("distinct_datatypes_missing",
                               "Display Distinct Datatypes",
                               value = FALSE),
                 
                 open = "open",
                 width = "350px"),
               
              plotOutput("missing_data")
             )

    ),
    
    tabPanel("Scatter Plot",
             h3("Scatter Plot"),
             layout_sidebar(
               sidebar = sidebar(
                 title = '',
                 actionButton("reset_input_counts_over_time", "Reset Inputs"),
                 accordion(open = FALSE,
                           accordion_panel(
                             "Select Variables",
                             select_variables_numeric_counts_over_time
                           ),
                           accordion_panel(
                             "Filter Variables",
                             uiOutput("dynamic_filters_counts_over_time"),
                             actionButton("reset_filter_input_counts_over_time", "Reset Filters")
                           )
                           
                 ),
                 
                 pickerInput(
                   inputId = "counts_colourby",
                   label = "Colour By",
                   choices = c("None", "Variable", names(cat_vars)),
                   selected = "Operator",
                   multiple = FALSE,
                   options = list(
                     `actions-box` = TRUE,  
                     `live-search` = TRUE    
                   )
                 ),

                 
                 open = "open",
                 width = "350px"),
               
               plotlyOutput("counts_over_time")
             )
    ),
    
    
    tabPanel("Rising Order",
             h3("Rising Value Chart"),
             layout_sidebar(
               sidebar = sidebar(
                 title = '',
                 actionButton("reset_input_rising", "Reset Inputs"),
                 sliderInput("jump_threshold", 
                             "Jump Threshold",
                             min = 0.1,
                             max = 2,
                             value = 0.5),
                 checkboxInput("center_data", 
                               "Center Data",
                               value = TRUE),
                 checkboxInput("scale_data", 
                               "Scale Data",
                               value = TRUE),
                 accordion(open = FALSE,
                           accordion_panel(
                             "Select Variables",
                             select_variables_numeric_rising),
                           accordion_panel(
                             "Filter Variables",
                             uiOutput("dynamic_filters_rising"),
                             actionButton("reset_filter_input_rising", "Reset Filters")
                           )
                 ),
                 
                 open = "open",
                 width = "350px"),
               
               plotOutput("rising_value")

             )
    ),
    
    tabPanel("Correlations",
             h3("Correlations"),
             layout_sidebar(
               sidebar = sidebar(
                 actionButton("reset_input_corr", "Reset Inputs"),
                 radioButtons("corr_method",
                              "Correlation Type",
                              c("Pearson" = "pearson",
                                "Spearman" = "spearman",
                                "Kendall" = "kendall"
                              )),
                 radioButtons("corr_order",
                              "Order Variables:",
                              c("No Order" = FALSE,
                                "PCA",
                                "OLO",
                                "GW",
                                "HC")
                 ),
                 radioButtons("corr_absolute",
                              "Use Absolute Values?",
                              c("No" = FALSE,
                                "Yes" = TRUE)
                 ),
                 open = "open",
                 width = "350px"
               ),
               plotOutput("corr_chart")             
               
             )
    ),
    
    
    tabPanel("Boxplot",
             h3("Boxplot"),
             layout_sidebar(
               sidebar = sidebar(
                 title = '',
                 actionButton("reset_input_boxplot", "Reset Input"),
                 sliderInput("iqr_boxplot", 
                             "Interquartile Range Multiplier",
                             min = 0.1,
                             max = 3,
                             value = 1.5),
                 checkboxInput("center_data_boxplot", 
                               "Center Data",
                               value = FALSE),
                 checkboxInput("scale_data_boxplot", 
                               "Scale Data",
                               value = FALSE),
                 accordion(open = FALSE,
                           accordion_panel(
                             "Select Variables",
                             select_variables_numeric_boxplot),
                           accordion_panel(
                             "Filter Variables",
                             uiOutput("dynamic_filters_boxplot"),
                             actionButton("reset_filter_input_boxplot", "Reset Filters")
                           )
                 ),
                 
                 open = "open",
                 width = "350px"),
               
               plotlyOutput("boxplot")
             )
             
    ),
    
    
    tabPanel("ggPairs Plot",
             h3("ggPairs"),
             layout_sidebar(
               sidebar = sidebar(
                 title = '',
                 actionButton("reset_input_ggpairs", "Reset Inputs"),
                 accordion(open = FALSE,
                           accordion_panel(
                             "Select Variables",
                             select_variables_numeric_ggpairs,
                             select_variables_categorical_ggpairs,
                             select_variables_date_ggpairs),
                           accordion_panel(
                             "Filter Variables",
                             uiOutput("dynamic_filters_ggpairs"),
                             actionButton("reset_filter_input_ggpairs", "Reset Filters")
                           )
                 ),
                 pickerInput(
                   inputId = "ggpairs_colourby",
                   label = "Colour By",
                   choices = c("None", names(cat_vars)),
                   selected = "Operator",
                   multiple = FALSE,
                   options = list(
                     `actions-box` = TRUE,  
                     `live-search` = TRUE    
                   )
                 ),
                 open = "open",
                 width = "350px"),
               
               
               plotOutput("ggpairs")
             )
    ),
    
    tabPanel("Mosaic Chart",
             h3("Mosaic"),
             layout_sidebar(
               sidebar = sidebar(
                 title = '',
                 actionButton("reset_input_mosaic", "Reset Inputs"),
                 accordion(open = FALSE,
                           accordion_panel(
                             "Select Variables",
                             select_variables_mosaic_x,
                             select_variables_mosaic_y,
                             select_variables_mosaic_z,
                             select_variables_mosaic_xyz
                             ),
                           accordion_panel(
                             "Filter Variables",
                             uiOutput("dynamic_filters_mosaic"),
                             actionButton("reset_filter_input_mosaic", "Reset Filters")
                           )
                 ),
                 
                 open = "open",
                 width = "350px"),
               
               plotOutput("mosaic")
             )
             ),


             
    tabPanel("Export Data",
             h3("Export Data"),
             layout_sidebar(
               sidebar = sidebar(
                 title = 'Adjust Export',
                 actionButton("reset_input_data", "Reset Inputs"),
                 checkboxInput("center_data_export", 
                               "Center Numeric Data",
                               value = FALSE),
                 checkboxInput("scale_data_export", 
                               "Scale Numeric Data",
                               value = FALSE),
                 accordion(open = FALSE,
                           accordion_panel(
                             "Select Variables",
                             select_variables_categorical_data,
                             select_variables_numeric_data,
                             select_variables_date_data),
                           accordion_panel(
                             "Filter Variables",
                             uiOutput("dynamic_filters_data"),
                             actionButton("reset_filter_input_data", "Reset Filters"),
                           ),
                           
                           accordion_panel(
                             "Export Data",
                             downloadButton('data_export_csv', 'Export as .csv'),
                             
                             downloadButton('data_export_xlsx', 'Export as .xlsx'),
                             
                             downloadButton('data_export_spss', 'Export as .sav (for SPSS)'),
                             
                             downloadButton('data_export_tsv', 'Export as .tsv'),
                             
                             downloadButton('data_export_rds', 'Export as .rds (R Data File)')
                           )
                 ),
                 open = "open",
                 width = "350px"),
                 DTOutput("data")
             )
          ),
    
    nav_spacer(),
    nav_item(
      actionButton("reset_input_all", 
                   "Reset Inputs Across All Tabs",
                   class = "btn btn-danger btn-sm")
    )  )
)
