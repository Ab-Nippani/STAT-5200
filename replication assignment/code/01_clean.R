# =============================================================================
# 01_clean.R
# Load the authors' T1 (individual-level registration drive) data and build
# the analysis file used for Table 2.
#
# The only data transformation the authors make before Table 2 is to
# winsorize household income at the 99th percentile and rescale it to
# thousands of rupees (supplementary-information.Rmd, lines ~140-150).
# The 99th percentile is computed on the FULL sample of 2,306 (including
# subjects later lost to attrition), exactly as in the original code.
# =============================================================================

if (!exists("root")) root <- "/Users/nippani/Documents/STAT 5200/replication assignment"

library(dplyr)
library(scales)

# ---- 1. load raw data -------------------------------------------------------
raw <- readRDS(file.path(root, "Data", "raw", "data-t1-experiment-df.Rds"))

cat("Raw T1 data:", nrow(raw), "rows,", ncol(raw), "columns\n")

# ---- 2. winsorize income ----------------------------------------------------
income_p99 <- quantile(raw$b_income, 0.99)
income_p00 <- quantile(raw$b_income, 0.00)

cat("Income 99th percentile (INR):", income_p99, "\n")
cat("Number of incomes above the 99th percentile:", sum(raw$b_income > income_p99), "\n")

clean <- raw
clean$b_income_000_winsor <- squish(clean$b_income, c(income_p00, income_p99)) / 1000
clean$b_income_000_raw    <- clean$b_income / 1000

# ---- 3. keep only the variables needed --------------------------------------
vars_keep <- c(
  # identifiers and design
  "i_uniqueid", "i_city", "i_t1", "i_attrition",
  # Table 2 outcomes (endline)
  "e_voter_id", "e_voted_2019", "e_vote_future",
  # baseline measures entered as "lagged" controls, one per outcome
  "b_has_village_voter_id",       # col 1 (hometown voter ID; nobody had a city ID at baseline)
  "b_not_voted_previously",       # col 2
  "b_vote_mc_elecs_how_likely",   # col 3
  # pre-registered covariates
  "b_female", "b_age", "b_muslim", "b_sc_st", "b_primary_edu",
  "b_income_000_winsor", "b_income_000_raw",
  "b_married", "b_length_residence", "b_owns_home"
)

clean <- clean[, vars_keep]

# ---- 4. sanity checks (stop if the data are not what we expect) -------------
stopifnot(nrow(clean) == 2306)
stopifnot(all(clean$i_t1 %in% c(0, 1)))
stopifnot(all(clean$e_voter_id   %in% c(0, 1, NA)))
stopifnot(all(clean$e_voted_2019 %in% c(0, 1, NA)))

# outcomes are missing exactly for the attritors
stopifnot(all(is.na(clean$e_voter_id) == (clean$i_attrition == 1)))

cat("Treated:", sum(clean$i_t1 == 1), " Control:", sum(clean$i_t1 == 0), "\n")
cat("Attritors (no endline):", sum(clean$i_attrition == 1), "\n")
cat("Missing values in covariates:",
    sum(is.na(clean[, c("b_female", "b_age", "b_muslim", "b_sc_st", "b_primary_edu",
                        "b_income_000_winsor", "b_married", "b_length_residence",
                        "b_owns_home")])), "\n")

# ---- 5. save ----------------------------------------------------------------
saveRDS(clean, file.path(root, "Data", "clean", "t1_clean.rds"))
write.csv(clean, file.path(root, "Data", "clean", "t1_clean.csv"), row.names = FALSE)

cat("Saved Data/clean/t1_clean.rds and t1_clean.csv\n")
