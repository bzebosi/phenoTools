#' ----------------------------------------------------------------------------
#' Run post hoc comparisons using estimated marginal means.
#'
#' Calculates estimated marginal means, pairwise comparisons, and compact
#' letter displays from a fitted model.
#'
#' Inputs:
#'    model  : Fitted model object.
#'    specs  : Formula specifying the factor(s) to compare.
#'            Examples:
#'              ~ Genotype               # Compare genotype means
#'              ~ Treatment              # Compare treatment means
#'              ~ Genotype * Treatment   # Compare all genotype × treatment combinations
#'              ~ Genotype | Treatment   # Compare genotypes within each treatment
#'              ~ Treatment | Genotype   # Compare treatments within each genotype
#' 
#'   adjust    : Multiple-comparison adjustment method.
#'            Examples: "sidak" (default), "tukey",  "bonferroni", "holm", "none"
#'
#' Returns:
#'   A list containing:
#'     emmeans  : Estimated marginal means.
#'     pairwise : Pairwise comparisons.
#'     cld      : Compact letter display.
#' ----------------------------------------------------------------------------

posthoc_emmeans <- function(model, specs = ~ Genotype, adjust = "sidak") {
  
  # Compute estimated marginal means for the specified factor(s)
  emm_obj <- emmeans::emmeans(model, specs = specs)
  
  # Convert estimated marginal means to a data frame
  means_df <- as.data.frame(emm_obj)
  
  # Perform pairwise comparisons among estimated marginal means
  pairwise_df <- as.data.frame(
    emmeans::contrast(emm_obj, method = "pairwise", adjust = adjust))
  
  # Generate a compact letter display for group comparisons
  cld_df <- as.data.frame(multcomp::cld(emm_obj, adjust = adjust))
  list(emmeans = means_df, pairwise = pairwise_df, cld = cld_df)
}
