#' ----------------------------------------------------------------------------
#' Export analysis results to an Excel workbook.
#'
#' Writes one or more analysis tables to separate worksheets in an
#' Excel workbook.
#'
#' Inputs:
#'   results     : Named list of data frames to export. e.g.
#'                list(Summary = summary_df, 
#'                   ANOVA = anova_res$anova,
#'                   Shapiro = assumptions$shapiro,
#'                   Levene = assumptions$levene,
#'                   Emmeans = posthoc$emmeans,
#'                   Pairwise = posthoc$pairwise,
#'                   CLD = posthoc$cld)
#'
#'   output_file : Path and name of the Excel workbook, e.g.
#'                 "results/leaf_angle_results.xlsx".
#'
#' Returns:
#'   File path to the saved Excel workbook.
#' ----------------------------------------------------------------------------

export_results <- function(results, output_file = "analysis_results.xlsx") {
  
  # Check that results is a named list
  if (!is.list(results) || is.null(names(results))) {
    stop("results must be a named list of data frames.")
  }
  
  # Create the output directory if it does not exist
  output_dir <- dirname(output_file)
  if (!dir.exists(output_dir)) {dir.create(output_dir, recursive = TRUE)}
  
  # Write each data frame to a separate worksheet
  openxlsx::write.xlsx(results, file = output_file, overwrite = TRUE)
  
  # Get the full path to the saved workbook
  full_path <- normalizePath(output_file, mustWork = FALSE)
  
  message("Results saved to: ", full_path)
  
  # Return the full path
  invisible(output_file)
}
