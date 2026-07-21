#' ----------------------------------------------------------------------------
#' Check model assumptions.
#'
#' Runs diagnostic tests for an ANOVA, linear model, or mixed-effects model:
#' normality of residuals and homogeneity of variance.
#'
#' Inputs:
#'   model      : Fitted model object.
#'   data       : Data frame used to fit the model.
#'   formula    : Fixed-effects formula used for Levene's test. e.g.
#'                  Value ~ Genotype
#'                  Value ~ Genotype * Treatment
#'
#'   output_dir : Directory where the Q-Q plot is saved, e.g.
#'                 "qq_plot.png" or "results/qq_plot.png".
#'
#' Returns:
#'   A list containing:
#'     shapiro : Shapiro-Wilk test results.
#'     levene  : Levene's test results.
#'     qq_plot : File path to the saved Q-Q plot.
#' ----------------------------------------------------------------------------

check_assumptions <- function(model, data, formula, output_file  = "qq_plot.png") {
  
  # Convert data to a base data frame
  data <- as.data.frame(data)
  
  # Convert character formula to formula object
  formula <- stats::as.formula(formula)
  
  # Extract model residuals
  res <- stats::residuals(model)
  
  # Test whether residuals are normally distributed
  shapiro_test <- stats::shapiro.test(res)
  
  shapiro_df <- data.frame(test = "Shapiro-Wilk", 
                           statistic = unname(shapiro_test$statistic),
                           p_value = shapiro_test$p.value)

  # Test whether groups have equal variances
  levene_df <- as.data.frame(car::leveneTest(formula, data = data))
  
  # Create the output directory if it does not exist
  output_dir <- dirname(output_file)
  
  # Create output directory if it does not exist
  if (!dir.exists(output_dir)) {dir.create(output_dir, recursive = TRUE)}
  
  # Save Q-Q plot
  grDevices::png(filename = output_file, width = 1800, height = 1800, res = 300)
  
  # Normal Q-Q plot of residuals
  stats::qqnorm(res, main = "Normal Q-Q Plot")
  stats::qqline(res, col = "red", lwd = 2)
  grDevices::dev.off()
  
  # Get the full path to the saved plot
  full_path <- normalizePath(output_file, mustWork = FALSE)
  
  # Inform the user where the plot was saved
  message("Q-Q plot saved to: ", full_path)
  
  list(shapiro = shapiro_df, levene = levene_df, qq_plot = full_path)
}
