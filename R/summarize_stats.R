#' ----------------------------------------------------------------------------
#' Calculate grouped summary statistics.
#'
#' Computes sample size, mean, standard deviation, standard error,
#' coefficient of variation, and confidence interval for one or more groups.
#'
#' Inputs:
#'   data : input data frame.
#'   group_cols : One or more columns used for grouping, e.g.
#'                "Genotype" or c("Genotype", "Treatment") or
#'                c("Genotype", "Treatment", "Location").
#'                
#'   value_col  : Numeric column to summarize.,
#'                e.g. "Value", "Plant_Height", or "Leaf_Angle".
#'                
#'   conf_level : confidence level for the confidence interval.
#'                (default = 0.95, e.g. 0.90, 0.95, or 0.99).
#'
#' Returns:
#'   a data frame containing the grouping variables together with
#'   n, mean, sd, se, cv, and ci.
#'   
#' ----------------------------------------------------------------------------
summarize_stats <- function(data, group_cols, value_col = "Value", 
                       conf_level = 0.95) {
  data %>%
    
    # Group observations by the specified columns
    dplyr::group_by(dplyr::across(tidyselect::all_of(group_cols))) %>%
    
    # Calculate summary statistics for each group
    dplyr::summarise(
      n = sum(!is.na(.data[[value_col]])), 
      mean = mean(.data[[value_col]], na.rm = TRUE),
      sd = stats::sd(.data[[value_col]], na.rm = TRUE), se = sd / sqrt(n),
      
      # Coefficient of variation (%)
      cv = dplyr::if_else(mean !=0, (sd / mean)*100, NA_real_),
      
      # Confidence interval
      ci = dplyr::if_else(n >1,  
                          stats::qt((1 + conf_level) / 2, df = n - 1) * se,
                          NA_real_), 
      
      # Return an ungrouped data
      .groups = "drop"
    )
}