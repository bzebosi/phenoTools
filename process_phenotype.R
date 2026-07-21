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

process_leafangle <- function(
    data, genotype_levels, keep_genotypes = NULL, genotype_col = "Genotype", 
    leaf_cols = paste0("L", 2:7), names_to = "Leaf", 
    values_to = "Value", trait_name = "Angle", transform_angle = TRUE) {
  
  # keep all genotypes unless a subset is provided
  if(is.null(keep_genotypes)){keep_genotypes <- genotype_levels}
  
  # helper for optional angle transformation
  adjust_angle <- function(x, transform = TRUE) {
    if (transform) 90 - x else x
  }
  
  data %>% 
    
    # Convert the genotype column to a factor with the specified level order
    dplyr::mutate(!!genotype_col := factor(.data[[genotype_col]], 
                                            levels = genotype_levels)) %>%
    
    # Remove missing genotypes and optionally keep a subset
    dplyr::filter(!is.na(.data[[genotype_col]]), 
                  .data[[genotype_col]] %in% keep_genotypes) %>%
    
    # Keep genotype and selected leaf measurement columns
    dplyr::select(tidyselect::all_of(c(genotype_col, leaf_cols))) %>%
    
    # Convert wide-format leaf measurements to long format
    reshape_longdata(id_cols = genotype_col, names_to = names_to, 
                     values_to = values_to) %>%
    
    # Standardize output columns and optionally transform angles
    dplyr::mutate(Trait = trait_name, 
                  !!names_to := factor(.data[[names_to]], levels = leaf_cols),
                  !!values_to := adjust_angle(.data[[values_to]], 
                                                 transform = transform_angle)) %>%
    
    # Keep only the standardized output columns
    dplyr::select(tidyselect::all_of(c(genotype_col, names_to, "Trait", values_to)))
}