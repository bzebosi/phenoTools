#' #----------------------------------------------------------------------------
#' Convert a data frame from wide format to long format.
#' 
#' data: input data frame.
#' id_cols:  columns  that identify each observation and kept remain unchanged.
#' names_to: the name of the new column containing the original column names.
#' values_to: the name of the new column that will hold the values.
#' 
#' Details:
#'   rows with missing measurements are removed during the pivot 
#'   values_drop_na = TRUE.
#' 
#' Returns: a data frame in long format.
#' #----------------------------------------------------------------------------

reshape_longdata <- function(data, id_cols = "Genotype", names_to = "Traits", 
                      values_to = "Value", values_drop_na = TRUE){
  data %>% pivot_longer(cols = -tidyselect::all_of(id_cols), 
                        values_drop_na = values_drop_na,
                        names_to = names_to, values_to = values_to)
}



