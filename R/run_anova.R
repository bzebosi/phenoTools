#' ----------------------------------------------------------------------------
#' Run ANOVA or a linear mixed-effects model.
#'
#' Fits either a linear model or a linear mixed-effects model
#' supports one-way, two-way, three-way, interaction, or mixed-effect models.
#'
#' Inputs:
#'   data         : input data frame.
#'   formula      : Model formula. e.g.
#'                    Value ~ Genotype
#'                    Value ~ Genotype * Treatment.
#'                    Value ~ Genotype + (1 | Block)
#'                    Value ~ Genotype * Treatment + (1 | Block)
#'                  
#'   mixed        : If TRUE, fits a mixed-effects model using lmerTest::lmer().
#'                  If FALSE, fits a linear model using stats::lm().
#'                  
#'   group_levels : optional named list specifying factor level order. e.g.
#'   list(Genotype = c("WT", "cbg1-1", "cbg2-1")) or 
#'   list(Genotype = c("WT", "cbg1-1", "cbg2-1"), Treatment = c("Control", "BR"))
#'
#' Returns:
#'   A list containing: 
#'      model :  fitted model object.
#'      anova :  ANOVA table.
#'   
#'  ----------------------------------------------------------------------------

run_anova <- function(data, formula, mixed = FALSE, group_levels = NULL) {
  
  # Convert tibbles to base data frames
  data <- as.data.frame(data)
  
  formula <- stats::as.formula(formula)
  
  # set factor level order if provided
  if (!is.null(group_levels)) {
    for (col in names(group_levels)) {
      data[[col]] <- factor(data[[col]], levels = group_levels[[col]])
    }
  }
  
  # fit linear model or mixed-effects model
  if (mixed) {
    model <- lmerTest::lmer(formula, data = data)
  } else {
    model <- stats::lm(formula, data = data)
  }
  
  # ANOVA table
  anova_table <- as.data.frame(stats::anova(model))
  
  # Preserve row names as a column
  anova_table <- cbind(Term = rownames(anova_table), anova_table)
  rownames(anova_table) <- NULL
  
  # Return fitted model and anova table
  list(model = model, anova = anova_table)
}
