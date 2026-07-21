#' ----------------------------------------------------------------------------
#' Process leaf angle data
#' converts wide-format leaf angle data into a long-format data frame for analysis.
#' 
#' Inputs:
#' data : input data frame
#' genotype_levels : desired genotype order
#' keep_genotypes : optional subset of genotypes
#' 
#' Returns:
#' Tidy data frame with: Genotype, Leaf, Trait, Value
#' ----------------------------------------------------------------------------
process_phenotype <- function(
    data, group_cols = "Genotype", group_levels = NULL, id_cols = NULL, 
    trait_cols = NULL, names_to = "Trait", values_to = "Value", 
    transform = NULL) {
  
  # order grouping variables and convert them to a factor
  if(!is.null(group_levels)){
    for (cx in names(group_levels)){
      data[[cx]] <- factor(data[[cx]], levels = group_levels[[cx]])
    }
  }
  
  # Keep only required columns
  keep_cols <- unique(c(id_cols, group_cols, trait_cols))
  data <- dplyr::select(data, dplyr::all_of(keep_cols))
  
  # Convert from wide to long format
  data <- tidyr::pivot_longer(data = data, cols = dplyr::all_of(trait_cols), 
                              names_to = names_to, values_to = values_to, 
                              values_drop_na = TRUE)
  
  # set trait order unless another order is provided
  data[[names_to]] <- factor(data[[names_to]], levels = trait_cols)
  
  # Optional transformation
  if (!is.null(transform)) { data[[values_to]] <- transform(data[[values_to]]) }
  
  data
}
