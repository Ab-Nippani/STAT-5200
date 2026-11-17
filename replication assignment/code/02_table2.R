# =============================================================================
# 02_table2.R
# Replicate Table 2: "[Pre-Registered] T1 Experimental Results For Primary
# Political Outcomes" (Gaikwad and Nellis 2021, APSR, p. 1140).
#
# Specification (paper eq. 1; Rmd function `t1_estimates`):
#   Y_i = a + b*T1_i + lagged/baseline measure + X_i'g + u_i
#   - OLS, HC2 robust standard errors (estimatr::lm_robust default)
#   - X = female, age, Muslim, SC/ST, primary education, winsorized income
#         (INR 000s), married, years in city, owns home
#   - "p-value (upper)" is a ONE-SIDED p-value for H1: b > 0
#   - control mean = mean of Y in the control group (endline respondents)
#   - attritors are dropped by listwise deletion (outcome is NA)
#
# Output:
#   results/tables/table2_replication.docx
#   results/tables/table2_replication.csv
#   results/tables/table2_published_vs_replicated.csv
# =============================================================================

if (!exists("root")) root <- "/Users/nippani/Documents/STAT 5200/replication assignment"

library(estimatr)
library(flextable)
library(officer)

df <- readRDS(file.path(root, "Data", "clean", "t1_clean.rds"))

covariates <- c("b_female", "b_age", "b_muslim", "b_sc_st", "b_primary_edu",
                "b_income_000_winsor", "b_married", "b_length_residence",
                "b_owns_home")

# ---- 1. the three Table 2 models -------------------------------------------
# one row per column of Table 2: outcome and its baseline control
table2_spec <- data.frame(
  column   = c("(1)", "(2)", "(3)"),
  label    = c("Has city-based voter ID",
               "Voted in city in 2019",
               "Likelihood of voting in city in future"),
  outcome  = c("e_voter_id", "e_voted_2019", "e_vote_future"),
  baseline = c("b_has_village_voter_id", "b_not_voted_previously",
               "b_vote_mc_elecs_how_likely"),
  dv_values = c("{0, 1}", "{0, 1}", "{0, 0.33, 0.67, 1}"),
  stringsAsFactors = FALSE
)

run_t1_model <- function(outcome, baseline, data) {
  rhs <- paste(c("i_t1", baseline, covariates), collapse = " + ")
  frm <- as.formula(paste(outcome, "~", rhs))

  fit <- lm_robust(frm, data = data, se_type = "HC2")

  est   <- coef(summary(fit))["i_t1", "Estimate"]
  se    <- coef(summary(fit))["i_t1", "Std. Error"]
  tstat <- coef(summary(fit))["i_t1", "t value"]
  dfree <- coef(summary(fit))["i_t1", "DF"]

  p_upper   <- pt(tstat, dfree, lower.tail = FALSE)   # what the paper reports
  p_twosided <- 2 * pt(abs(tstat), dfree, lower.tail = FALSE)

  control_mean <- mean(data[[outcome]][data$i_t1 == 0], na.rm = TRUE)

  data.frame(
    outcome      = outcome,
    estimate     = est,
    std_error    = se,
    p_upper      = p_upper,
    p_two_sided  = p_twosided,
    control_mean = control_mean,
    n_obs        = fit$nobs,
    adj_r2       = fit$adj.r.squared,
    ci_low       = fit$conf.low["i_t1"],
    ci_high      = fit$conf.high["i_t1"],
    stringsAsFactors = FALSE
  )
}

results <- data.frame()
for (k in 1:nrow(table2_spec)) {
  one <- run_t1_model(table2_spec$outcome[k], table2_spec$baseline[k], df)
  results <- rbind(results, one)
}
rownames(results) <- NULL
results$label <- table2_spec$label

cat("\n==== Table 2 replication ====\n")
print(results[, c("label", "estimate", "std_error", "p_upper", "p_two_sided",
                  "control_mean", "n_obs", "adj_r2")], digits = 3)

write.csv(results, file.path(root, "results", "tables", "table2_replication.csv"),
          row.names = FALSE)

# ---- 2. compare with the published numbers ----------------------------------
# typed in by hand from the published PDF, Table 2, p. 1140
published <- data.frame(
  outcome      = c("e_voter_id", "e_voted_2019", "e_vote_future"),
  estimate     = c(0.236, 0.203, 0.031),
  std_error    = c(0.019, 0.019, 0.009),
  p_upper      = c(0.000, 0.000, 0.000),
  control_mean = c(0.161, 0.178, 0.856),
  n_obs        = c(2120, 2120, 2120),
  adj_r2       = c(0.084, 0.065, 0.011),
  stringsAsFactors = FALSE
)

stats_to_compare <- c("estimate", "std_error", "p_upper", "control_mean",
                      "n_obs", "adj_r2")

comparison <- data.frame()
for (k in 1:3) {
  for (s in stats_to_compare) {
    pub <- published[k, s]
    rep <- results[k, s]
    digits <- ifelse(s == "n_obs", 0, 3)
    comparison <- rbind(comparison, data.frame(
      column       = table2_spec$column[k],
      outcome      = table2_spec$label[k],
      statistic    = s,
      published    = pub,
      replicated   = round(rep, digits),
      match_at_3dp = (round(rep, digits) == pub),
      stringsAsFactors = FALSE
    ))
  }
}

cat("\n==== Published vs replicated (rounded to 3 dp) ====\n")
print(comparison)
cat("\nAll", nrow(comparison), "numbers match:", all(comparison$match_at_3dp), "\n")

write.csv(comparison,
          file.path(root, "results", "tables", "table2_published_vs_replicated.csv"),
          row.names = FALSE)

# ---- 3. build the Word table in the paper's layout --------------------------
fmt3 <- function(x) formatC(x, format = "f", digits = 3)

word_tab <- data.frame(
  row = c("T1 treatment", "", "p-value (upper)", "Control mean",
          "Observations", "Adjusted R²", "DV values"),
  stringsAsFactors = FALSE
)
for (k in 1:3) {
  word_tab[[paste0("c", k)]] <- c(
    fmt3(results$estimate[k]),
    paste0("(", fmt3(results$std_error[k]), ")"),
    fmt3(results$p_upper[k]),
    fmt3(results$control_mean[k]),
    format(results$n_obs[k], big.mark = ","),
    fmt3(results$adj_r2[k]),
    table2_spec$dv_values[k]
  )
}

ft <- flextable(word_tab)
ft <- set_header_labels(ft,
                        row = "",
                        c1  = "Has city-based voter ID",
                        c2  = "Voted in city in 2019",
                        c3  = "Likelihood of voting in city in future")
ft <- add_header_row(ft, values = c("", "(1)", "(2)", "(3)"), top = FALSE)
ft <- font(ft, fontname = "Arial", part = "all")
ft <- fontsize(ft, size = 10, part = "all")
ft <- align(ft, j = 2:4, align = "center", part = "all")
ft <- border_remove(ft)
ft <- hline_top(ft, border = fp_border(width = 1.5), part = "header")
ft <- hline_bottom(ft, border = fp_border(width = 0.75), part = "header")
ft <- hline(ft, i = 2, border = fp_border(width = 0.75), part = "body")
ft <- hline_bottom(ft, border = fp_border(width = 1.5), part = "body")
ft <- width(ft, j = 1, width = 1.6)
ft <- width(ft, j = 2:4, width = 1.6)

note_text <- paste(
  "Note: Replication of Gaikwad and Nellis (2021), Table 2. Outcomes are whether the",
  "respondent (1) currently has a voter ID card allowing them to vote in city elections,",
  "(2) voted in the city during the 2019 Lok Sabha elections, and (3) intends to vote in",
  "the next state elections held in the city. OLS estimates of intent-to-treat effects.",
  "Models include the baseline measure listed in the README and the nine pre-registered",
  "covariates (gender, age, Muslim, SC/ST, primary education, winsorized income, married,",
  "years in city, owns home). HC2 robust standard errors in parentheses.",
  "'p-value (upper)' is one-sided (H1: effect > 0), as in the original code."
)

# comparison table for the Word doc
cmp_wide <- data.frame(
  Statistic = c("Estimate", "Std. error", "p-value (upper)", "Control mean",
                "Observations", "Adjusted R²"),
  stringsAsFactors = FALSE
)
for (k in 1:3) {
  sub <- comparison[comparison$column == table2_spec$column[k], ]
  pub_txt <- ifelse(sub$statistic == "n_obs", format(sub$published, big.mark = ","),
                    fmt3(sub$published))
  rep_txt <- ifelse(sub$statistic == "n_obs", format(sub$replicated, big.mark = ","),
                    fmt3(sub$replicated))
  cmp_wide[[paste0("pub", k)]] <- pub_txt
  cmp_wide[[paste0("rep", k)]] <- rep_txt
}

ft2 <- flextable(cmp_wide)
ft2 <- set_header_labels(ft2, Statistic = "",
                         pub1 = "Published", rep1 = "Replicated",
                         pub2 = "Published", rep2 = "Replicated",
                         pub3 = "Published", rep3 = "Replicated")
ft2 <- add_header_row(ft2, values = c("", "(1) Voter ID", "(2) Voted 2019",
                                      "(3) Future vote"),
                      colwidths = c(1, 2, 2, 2))
ft2 <- font(ft2, fontname = "Arial", part = "all")
ft2 <- fontsize(ft2, size = 9, part = "all")
ft2 <- align(ft2, j = 2:7, align = "center", part = "all")
ft2 <- border_remove(ft2)
ft2 <- hline_top(ft2, border = fp_border(width = 1.5), part = "header")
ft2 <- hline_bottom(ft2, border = fp_border(width = 0.75), part = "header")
ft2 <- hline_bottom(ft2, border = fp_border(width = 1.5), part = "body")
ft2 <- autofit(ft2)

n_match <- sum(comparison$match_at_3dp)

doc <- read_docx()
doc <- body_add_par(doc, "Replication of Table 2: Gaikwad and Nellis (2021, APSR)",
                    style = "heading 1")
doc <- body_add_par(doc, paste(
  "Table 2. [Pre-Registered] T1 Experimental Results for Primary Political Outcomes",
  "(replicated)"), style = "Normal")
doc <- body_add_flextable(doc, ft)
doc <- body_add_par(doc, note_text, style = "Normal")
doc <- body_add_par(doc, "", style = "Normal")
doc <- body_add_par(doc, "Published versus replicated values", style = "heading 2")
doc <- body_add_flextable(doc, ft2)
doc <- body_add_par(doc, paste0(
  n_match, " of ", nrow(comparison),
  " reported numbers match the published table exactly at three decimal places."),
  style = "Normal")
doc <- body_add_par(doc, paste0(
  "Software: ", R.version.string, "; estimatr ", packageVersion("estimatr"),
  ". Original analysis: R 4.0.5; estimatr 0.30.2."), style = "Normal")

print(doc, target = file.path(root, "results", "tables", "table2_replication.docx"))
cat("Saved results/tables/table2_replication.docx\n")
