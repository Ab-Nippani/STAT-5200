# =============================================================================
# 03_checks.R
# Robustness and credibility checks on the Table 2 results.
# These are NOT in the published Table 2; they support the "how believable
# are the results" part of the presentation.
#
#   A. Specification robustness (no covariates; no baseline control; HC1 SEs;
#      raw (unwinsorized) income; city fixed effect)
#   B. Balance: do baseline covariates jointly predict T1 assignment?
#   C. Attrition: is attrition different across arms? Lee (2009) bounds
#   D. Randomization inference p-values (permute T1 labels)
#   E. Measurement: self-reported voting without a self-reported city ID
#   F. Administrative processing data (treated applications only)
#
# Output:
#   results/tables/robustness_checks.docx / .csv
#   results/figures/fig1_means_by_arm.png
#   results/figures/fig2_coef_robustness.png
# =============================================================================

if (!exists("root")) root <- "/Users/nippani/Documents/STAT 5200/replication assignment"

library(estimatr)
library(flextable)
library(officer)
library(ggplot2)

set.seed(20191007)   # EGAP pre-registration number, for reproducibility

df <- readRDS(file.path(root, "Data", "clean", "t1_clean.rds"))
df_end <- df[df$i_attrition == 0, ]          # endline respondents

covariates <- c("b_female", "b_age", "b_muslim", "b_sc_st", "b_primary_edu",
                "b_income_000_winsor", "b_married", "b_length_residence",
                "b_owns_home")

outcomes  <- c("e_voter_id", "e_voted_2019", "e_vote_future")
baselines <- c("b_has_village_voter_id", "b_not_voted_previously",
               "b_vote_mc_elecs_how_likely")
labels    <- c("(1) Has city voter ID", "(2) Voted in city 2019",
               "(3) Likely to vote in future")

# helper: estimate, SE, two-sided p for i_t1 from a formula
t1_coef <- function(frm, data, se_type = "HC2") {
  fit <- lm_robust(frm, data = data, se_type = se_type)
  s <- coef(summary(fit))["i_t1", ]
  c(estimate = unname(s["Estimate"]), se = unname(s["Std. Error"]),
    p_two = unname(s["Pr(>|t|)"]), ci_low = unname(fit$conf.low["i_t1"]),
    ci_high = unname(fit$conf.high["i_t1"]), n = fit$nobs)
}

# =============================================================================
# A. Specification robustness
# =============================================================================
spec_rows <- data.frame()

for (k in 1:3) {
  y <- outcomes[k]
  b <- baselines[k]

  specs <- list(
    "Published spec (HC2)"        = list(rhs = c("i_t1", b, covariates), se = "HC2"),
    "No covariates"               = list(rhs = c("i_t1"), se = "HC2"),
    "Covariates, no baseline DV"  = list(rhs = c("i_t1", covariates), se = "HC2"),
    "Published spec, HC1 SEs"     = list(rhs = c("i_t1", b, covariates), se = "stata"),
    "Raw (unwinsorized) income"   = list(rhs = c("i_t1", b, sub("b_income_000_winsor",
                                                  "b_income_000_raw", covariates)), se = "HC2"),
    "Add city fixed effect"       = list(rhs = c("i_t1", b, covariates, "i_city"), se = "HC2")
  )

  for (s in names(specs)) {
    frm <- as.formula(paste(y, "~", paste(specs[[s]]$rhs, collapse = " + ")))
    out <- t1_coef(frm, df_end, specs[[s]]$se)
    spec_rows <- rbind(spec_rows, data.frame(
      outcome = labels[k], specification = s,
      estimate = out["estimate"], se = out["se"], p_two_sided = out["p_two"],
      ci_low = out["ci_low"], ci_high = out["ci_high"], n = out["n"],
      stringsAsFactors = FALSE))
  }
}
rownames(spec_rows) <- NULL

cat("\n==== A. Specification robustness ====\n")
print(spec_rows, digits = 3)

# =============================================================================
# B. Balance on baseline covariates (full randomized sample, N = 2,306)
# =============================================================================
balance_frm <- as.formula(paste("i_t1 ~", paste(c(covariates, baselines), collapse = " + ")))
balance_fit <- lm_robust(balance_frm, data = df, se_type = "HC2")
balance_wald <- summary(balance_fit)$fstatistic   # robust joint F-test
balance_F <- unname(balance_wald["value"])
balance_p <- pf(balance_F, balance_wald["numdf"], balance_wald["dendf"], lower.tail = FALSE)

# covariate-by-covariate differences in means (treatment minus control)
balance_rows <- data.frame()
for (v in c(covariates, baselines)) {
  fit_v <- lm_robust(as.formula(paste(v, "~ i_t1")), data = df, se_type = "HC2")
  balance_rows <- rbind(balance_rows, data.frame(
    variable = v,
    control_mean = mean(df[[v]][df$i_t1 == 0]),
    treatment_mean = mean(df[[v]][df$i_t1 == 1]),
    difference = coef(summary(fit_v))["i_t1", "Estimate"],
    p_value = coef(summary(fit_v))["i_t1", "Pr(>|t|)"],
    stringsAsFactors = FALSE))
}
rownames(balance_rows) <- NULL

cat("\n==== B. Balance ====\n")
print(balance_rows, digits = 3)
cat("Covariates imbalanced at p < 0.05:", sum(balance_rows$p_value < 0.05), "of",
    nrow(balance_rows), "(expected by chance:", round(0.05 * nrow(balance_rows), 1), ")\n")
cat("Joint F-test of all baseline covariates predicting T1: F =",
    round(balance_F, 2), " p =", round(balance_p, 3), "\n")

# =============================================================================
# C. Attrition and Lee (2009) bounds
# =============================================================================
attr_fit <- lm_robust(i_attrition ~ i_t1, data = df, se_type = "HC2")
attr_rate_c <- mean(df$i_attrition[df$i_t1 == 0])
attr_rate_t <- mean(df$i_attrition[df$i_t1 == 1])

cat("\n==== C. Attrition ====\n")
cat("Attrition, control:", round(attr_rate_c, 3), " treatment:", round(attr_rate_t, 3),
    " difference p =", round(coef(summary(attr_fit))["i_t1", "Pr(>|t|)"], 3), "\n")

# Lee bounds (no covariates). Trim the arm with the higher response rate
# by the excess share, from the top (lower bound) or bottom (upper bound).
lee_bounds <- function(y, treat, observed) {
  resp_t <- mean(observed[treat == 1])
  resp_c <- mean(observed[treat == 0])

  if (resp_t >= resp_c) {
    trim_arm <- 1
    p_trim <- (resp_t - resp_c) / resp_t
  } else {
    trim_arm <- 0
    p_trim <- (resp_c - resp_t) / resp_c
  }

  y_trim  <- sort(y[treat == trim_arm & observed == 1])
  y_other <- y[treat != trim_arm & observed == 1]
  n_drop  <- round(p_trim * length(y_trim))

  keep_low  <- y_trim[1:(length(y_trim) - n_drop)]          # drop the highest values
  keep_high <- y_trim[(n_drop + 1):length(y_trim)]           # drop the lowest values

  if (trim_arm == 1) {
    lower <- mean(keep_low)  - mean(y_other)
    upper <- mean(keep_high) - mean(y_other)
  } else {
    lower <- mean(y_other) - mean(keep_high)
    upper <- mean(y_other) - mean(keep_low)
  }
  c(lower = lower, upper = upper, share_trimmed = p_trim, trimmed_arm = trim_arm)
}

lee_rows <- data.frame()
for (k in 1:3) {
  y <- df[[outcomes[k]]]
  lb <- lee_bounds(y, df$i_t1, 1 - df$i_attrition)
  # worst case: treated attritors all 0, control attritors all 1 (Manski-style)
  y_worst <- y
  y_worst[df$i_attrition == 1 & df$i_t1 == 1] <- 0
  y_worst[df$i_attrition == 1 & df$i_t1 == 0] <- 1
  worst <- mean(y_worst[df$i_t1 == 1]) - mean(y_worst[df$i_t1 == 0])
  lee_rows <- rbind(lee_rows, data.frame(
    outcome = labels[k],
    diff_in_means = mean(y[df$i_t1 == 1], na.rm = TRUE) - mean(y[df$i_t1 == 0], na.rm = TRUE),
    lee_lower = lb["lower"], lee_upper = lb["upper"],
    share_trimmed = lb["share_trimmed"],
    worst_case_bound = worst, stringsAsFactors = FALSE))
}
rownames(lee_rows) <- NULL
print(lee_rows, digits = 3)

# =============================================================================
# D. Randomization inference (2,000 permutations of T1 among respondents)
# Assignment was simple (Bernoulli 0.5) randomization, so we re-draw T1 as
# independent coin flips; this matches the actual assignment mechanism.
# =============================================================================
n_perm <- 2000
ri_rows <- data.frame()

for (k in 1:3) {
  y <- outcomes[k]
  rhs_vars <- c(baselines[k], covariates)
  d <- df_end[, c(y, "i_t1", rhs_vars)]
  X_other <- cbind(1, as.matrix(d[, rhs_vars]))
  yy <- d[[y]]

  observed <- lm.fit(cbind(d$i_t1, X_other), yy)$coefficients[1]

  perm_est <- numeric(n_perm)
  for (r in 1:n_perm) {
    t_perm <- rbinom(nrow(d), 1, 0.5)
    perm_est[r] <- lm.fit(cbind(t_perm, X_other), yy)$coefficients[1]
  }

  ri_rows <- rbind(ri_rows, data.frame(
    outcome = labels[k],
    observed_estimate = observed,
    largest_permuted = max(abs(perm_est)),
    ri_p_two_sided = mean(abs(perm_est) >= abs(observed)),
    stringsAsFactors = FALSE))
}
rownames(ri_rows) <- NULL

cat("\n==== D. Randomization inference ====\n")
print(ri_rows, digits = 3)

# =============================================================================
# E. Measurement: report voting in the city but no city voter ID
# =============================================================================
voted_no_id <- data.frame(
  arm = c("Control", "Treatment"),
  n_respondents = c(sum(df_end$i_t1 == 0), sum(df_end$i_t1 == 1)),
  voted_without_city_id = c(
    sum(df_end$e_voted_2019 == 1 & df_end$e_voter_id == 0 & df_end$i_t1 == 0),
    sum(df_end$e_voted_2019 == 1 & df_end$e_voter_id == 0 & df_end$i_t1 == 1)),
  has_id_did_not_vote = c(
    sum(df_end$e_voted_2019 == 0 & df_end$e_voter_id == 1 & df_end$i_t1 == 0),
    sum(df_end$e_voted_2019 == 0 & df_end$e_voter_id == 1 & df_end$i_t1 == 1))
)
voted_no_id$share_voted_without_id <- voted_no_id$voted_without_city_id / voted_no_id$n_respondents

cat("\n==== E. Self-report consistency ====\n")
print(voted_no_id, digits = 3)

# =============================================================================
# F. Administrative data on processing of treated applications
# (not linkable to survey respondents; aggregate only)
# =============================================================================
admin <- read.csv(file.path(root, "Data", "raw", "data-processing-times.csv"))
admin_summary <- data.frame(
  applications_tracked = nrow(admin),
  approved = sum(!is.na(admin$time_to_success)),
  rejected = sum(!is.na(admin$time_to_reject)),
  median_days_to_decision = median(admin$time_to_decision, na.rm = TRUE)
)
admin_summary$approval_rate <- admin_summary$approved / admin_summary$applications_tracked
treated_n <- sum(df$i_t1 == 1)
treated_endline_with_id <- mean(df_end$e_voter_id[df_end$i_t1 == 1])

cat("\n==== F. Administrative processing data ====\n")
print(admin_summary)
cat("Treated subjects randomized:", treated_n,
    " approvals / treated =", round(admin_summary$approved / treated_n, 3),
    " self-reported city ID among treated at endline =", round(treated_endline_with_id, 3), "\n")

# =============================================================================
# Save tables (csv + Word)
# =============================================================================
write.csv(spec_rows, file.path(root, "results", "tables", "check_A_specifications.csv"), row.names = FALSE)
write.csv(balance_rows, file.path(root, "results", "tables", "check_B_balance.csv"), row.names = FALSE)
write.csv(lee_rows,  file.path(root, "results", "tables", "check_C_attrition_bounds.csv"), row.names = FALSE)
write.csv(ri_rows,   file.path(root, "results", "tables", "check_D_randomization_inference.csv"), row.names = FALSE)
write.csv(voted_no_id, file.path(root, "results", "tables", "check_E_selfreport.csv"), row.names = FALSE)
write.csv(admin_summary, file.path(root, "results", "tables", "check_F_admin.csv"), row.names = FALSE)

style_ft <- function(ft) {
  ft <- font(ft, fontname = "Arial", part = "all")
  ft <- fontsize(ft, size = 9, part = "all")
  ft <- border_remove(ft)
  ft <- hline_top(ft, border = fp_border(width = 1.5), part = "header")
  ft <- hline_bottom(ft, border = fp_border(width = 0.75), part = "header")
  ft <- hline_bottom(ft, border = fp_border(width = 1.5), part = "body")
  autofit(ft)
}

spec_print <- spec_rows[, c("outcome", "specification", "estimate", "se", "p_two_sided", "n")]
spec_print$estimate <- formatC(spec_print$estimate, format = "f", digits = 3)
spec_print$se <- formatC(spec_print$se, format = "f", digits = 3)
spec_print$p_two_sided <- formatC(spec_print$p_two_sided, format = "f", digits = 4)

lee_print <- lee_rows
for (v in 2:6) lee_print[[v]] <- formatC(lee_print[[v]], format = "f", digits = 3)

ri_print <- ri_rows
for (v in 2:4) ri_print[[v]] <- formatC(ri_print[[v]], format = "f", digits = 3)

doc <- read_docx()
doc <- body_add_par(doc, "Robustness and credibility checks for Table 2", style = "heading 1")

doc <- body_add_par(doc, "A. Specification robustness (two-sided p-values)", style = "heading 2")
doc <- body_add_flextable(doc, style_ft(flextable(spec_print)))

doc <- body_add_par(doc, "B. Covariate balance", style = "heading 2")
doc <- body_add_par(doc, paste0(
  "Regressing T1 assignment on the nine covariates and three baseline measures (N = 2,306): ",
  "robust joint F = ", round(balance_F, 2), ", p = ", round(balance_p, 3), "."), style = "Normal")
balance_print <- balance_rows
for (v in 2:5) balance_print[[v]] <- formatC(balance_print[[v]], format = "f", digits = 3)
doc <- body_add_flextable(doc, style_ft(flextable(balance_print)))

doc <- body_add_par(doc, "C. Attrition", style = "heading 2")
doc <- body_add_par(doc, paste0(
  "Attrition: control ", round(100 * attr_rate_c, 1), "%, treatment ",
  round(100 * attr_rate_t, 1), "% (difference p = ",
  round(coef(summary(attr_fit))["i_t1", "Pr(>|t|)"], 3), "). ",
  "Lee bounds trim the arm with higher response; the worst-case bound sets treated ",
  "attritors to 0 and control attritors to 1."), style = "Normal")
doc <- body_add_flextable(doc, style_ft(flextable(lee_print)))

doc <- body_add_par(doc, "D. Randomization inference (2,000 draws)", style = "heading 2")
doc <- body_add_flextable(doc, style_ft(flextable(ri_print)))

doc <- body_add_par(doc, "E. Consistency of self-reports", style = "heading 2")
doc <- body_add_flextable(doc, style_ft(flextable(voted_no_id)))

doc <- body_add_par(doc, "F. Administrative processing of treated applications", style = "heading 2")
doc <- body_add_flextable(doc, style_ft(flextable(admin_summary)))

print(doc, target = file.path(root, "results", "tables", "robustness_checks.docx"))
cat("\nSaved results/tables/robustness_checks.docx\n")

# =============================================================================
# Figures (sober palette: grey = control, navy = treatment)
# =============================================================================
col_control <- "#7F8C99"
col_treat   <- "#1F3A5F"

# Figure 1: outcome means by arm (endline respondents)
means <- data.frame()
for (k in 1:3) {
  for (a in c(0, 1)) {
    yv <- df_end[[outcomes[k]]][df_end$i_t1 == a]
    means <- rbind(means, data.frame(
      outcome = labels[k], arm = ifelse(a == 1, "Treatment", "Control"),
      mean = mean(yv), se = sd(yv) / sqrt(length(yv))))
  }
}
means$outcome <- factor(means$outcome, levels = labels)
means$arm <- factor(means$arm, levels = c("Control", "Treatment"))

fig1 <- ggplot(means, aes(x = arm, y = mean, fill = arm)) +
  geom_col(width = 0.6) +
  geom_errorbar(aes(ymin = mean - 1.96 * se, ymax = mean + 1.96 * se), width = 0.12,
                colour = "grey20") +
  geom_text(aes(label = formatC(mean, format = "f", digits = 2), y = mean + 1.96 * se + 0.04),
            size = 4, family = "Arial") +
  facet_wrap(~ outcome) +
  scale_fill_manual(values = c(Control = col_control, Treatment = col_treat), guide = "none") +
  scale_y_continuous(limits = c(0, 1.05), breaks = seq(0, 1, 0.25)) +
  labs(x = NULL, y = "Mean at endline") +
  theme_classic(base_family = "Arial", base_size = 13) +
  theme(strip.background = element_blank(), strip.text = element_text(face = "bold"))

ggsave(file.path(root, "results", "figures", "fig1_means_by_arm.png"), fig1,
       width = 9, height = 4.2, dpi = 300)

# Figure 2: T1 coefficient across specifications (outcomes 1 and 2)
plot_specs <- spec_rows[spec_rows$outcome %in% labels[1:2], ]
plot_specs$specification <- factor(plot_specs$specification,
                                   levels = rev(unique(spec_rows$specification)))
plot_specs$outcome <- factor(plot_specs$outcome, levels = labels[1:2])

fig2 <- ggplot(plot_specs, aes(x = estimate, y = specification)) +
  geom_vline(xintercept = 0, colour = "grey60") +
  geom_errorbar(aes(xmin = ci_low, xmax = ci_high), width = 0, colour = col_treat,
                linewidth = 0.8, orientation = "y") +
  geom_point(colour = col_treat, size = 2.6) +
  facet_wrap(~ outcome) +
  scale_x_continuous(limits = c(0, 0.32)) +
  labs(x = "ITT estimate of T1 (95% CI)", y = NULL) +
  theme_classic(base_family = "Arial", base_size = 13) +
  theme(strip.background = element_blank(), strip.text = element_text(face = "bold"))

ggsave(file.path(root, "results", "figures", "fig2_coef_robustness.png"), fig2,
       width = 9, height = 3.6, dpi = 300)

cat("Saved figures to results/figures/\n")
