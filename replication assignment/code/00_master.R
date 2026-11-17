# =============================================================================
# 00_master.R
# Replication of Table 2 in:
#   Gaikwad, N. and Nellis, G. (2021). "Overcoming the Political Exclusion of
#   Migrants: Theory and Experimental Evidence from India." APSR 115(4).
#
# Running this file runs the full pipeline:
#   01_clean.R     raw T1 experiment data  -> Data/clean/t1_clean.rds
#   02_table2.R    Table 2 regressions     -> results/tables/table2_replication.docx
#   03_checks.R    robustness / validity   -> results/tables/robustness_checks.docx
#                                             results/figures/*.png
#
# Set `root` to the folder that contains code/, Data/, results/.
# =============================================================================

root <- "/Users/nippani/Documents/STAT 5200/replication assignment"

# packages used across the scripts
packages_needed <- c("estimatr", "dplyr", "scales", "flextable", "officer", "ggplot2")

for (p in packages_needed) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p, repos = "https://cloud.r-project.org")
  }
}

source(file.path(root, "code", "01_clean.R"))
source(file.path(root, "code", "02_table2.R"))
source(file.path(root, "code", "03_checks.R"))

# record the software environment for the README
writeLines(capture.output(sessionInfo()),
           file.path(root, "results", "sessionInfo.txt"))

cat("\nPipeline finished.\n")
