# ============================================================
# phenoTools loader
# Load all phenoTools functions directly from GitHub
# ============================================================

source("https://raw.githubusercontent.com/bzebosi/phenoTools/main/R/install_pkgs.R")
source("https://raw.githubusercontent.com/bzebosi/phenoTools/main/R/reshape_longdata.R")
source("https://raw.githubusercontent.com/bzebosi/phenoTools/main/R/process_leafangle.R")
source("https://raw.githubusercontent.com/bzebosi/phenoTools/main/R/summarize_stats.R")
source("https://raw.githubusercontent.com/bzebosi/phenoTools/main/R/run_anova.R")
source("https://raw.githubusercontent.com/bzebosi/phenoTools/main/R/check_assumptions.R")
source("https://raw.githubusercontent.com/bzebosi/phenoTools/main/R/posthoc_emmeans.R")
source("https://raw.githubusercontent.com/bzebosi/phenoTools/main/R/export_result.R")
source("https://raw.githubusercontent.com/bzebosi/phenoTools/main/R/plot_theme.R")


message("phenoTools loaded successfully!")