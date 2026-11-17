# Replication: Gaikwad and Nellis (2021), Table 2

**Paper.** Gaikwad, N. and Nellis, G. (2021). "Overcoming the Political Exclusion of Migrants: Theory and Experimental Evidence from India." *American Political Science Review* 115(4): 1129–1146. doi:10.1017/S0003055421000435

**Replication package.** Harvard Dataverse, doi:10.7910/DVN/G1JCKK

**Target.** Table 2, "[Pre-Registered] T1 Experimental Results for Primary Political Outcomes" (p. 1140).

**Course.** STAT 5200, Applied Econometrics, Fall 2026.

---

## How to run

1. Open `code/00_master.R`.
2. Set `root` (line 15) to the path of this folder.
3. Run the whole file. It installs any missing packages, then runs scripts 01–03 in order. Runtime is under one minute.

## Folder structure

```
STAT 5200/replication assignment/
├── Readme.md
├── code/
│   ├── 00_master.R      runs everything; writes results/sessionInfo.txt
│   ├── 01_clean.R       raw T1 data -> Data/clean/t1_clean.rds (winsorizes income)
│   ├── 02_table2.R      Table 2 regressions -> Word table + published-vs-replicated check
│   └── 03_checks.R      robustness, balance, attrition, randomization inference, figures
├── Data/
│   ├── raw/             untouched copies of the authors' files
│   │   ├── data-t1-experiment-df.Rds     T1 experiment data (N = 2,306)
│   │   ├── data-processing-times.csv     admin data on treated applications
│   │   ├── supplementary-information.Rmd authors' original code
│   │   ├── _readme.txt                   authors' readme
│   │   └── Gaikwad_Nellis_2021_APSR.pdf  the paper
│   └── clean/
│       ├── t1_clean.rds / .csv           analysis file (21 variables)
├── results/
│   ├── tables/
│   │   ├── table2_replication.docx       MAIN OUTPUT: Table 2 in the paper's layout
│   │   ├── table2_replication.csv
│   │   ├── table2_published_vs_replicated.csv
│   │   ├── robustness_checks.docx        checks A–F
│   │   └── check_A ... check_F .csv
│   ├── figures/
│   │   ├── fig1_means_by_arm.png
│   │   └── fig2_coef_robustness.png
│   ├── pipeline_log.txt                  console output of the last full run
│   └── sessionInfo.txt
└── presentation/
    ├── Gaikwad_Nellis_replication.pptx   10-minute presentation (11 slides, speaker notes included)
    └── build_deck.js                     pptxgenjs script that generated the deck
```

## Specification (taken from the authors' Rmd, function `t1_estimates`)

Y = a + b·T1 + (baseline measure) + X'g + u, estimated with OLS and HC2 robust standard errors (`estimatr::lm_robust`). The 2,120 endline respondents are used; the 186 people lost to attrition drop out because their outcomes are missing.

| Column | Outcome | Baseline control the authors include |
|---|---|---|
| (1) | `e_voter_id`: has a city voter ID | `b_has_village_voter_id` (hometown voter ID) |
| (2) | `e_voted_2019`: voted in city in 2019 | `b_not_voted_previously` |
| (3) | `e_vote_future`: likelihood of voting in city in future (0–1) | `b_vote_mc_elecs_how_likely` |

X = female, age, Muslim, SC/ST, primary education, household income (INR '000, winsorized at the 99th percentile of the full N = 2,306), married, years in city, owns home.

"p-value (upper)" in the paper is **one-sided** (H1: b > 0). The paper's text calls these "p" without saying they are one-sided.

## Result

**All 18 numbers in Table 2 replicate exactly to three decimal places** (estimate, SE, p-value, control mean, N, adjusted R² × 3 columns). See `results/tables/table2_published_vs_replicated.csv`.

| | (1) Voter ID | (2) Voted 2019 | (3) Future vote |
|---|---|---|---|
| T1 treatment | 0.236 (0.019) | 0.203 (0.019) | 0.031 (0.009) |
| Control mean | 0.161 | 0.178 | 0.856 |
| N | 2,120 | 2,120 | 2,120 |

## Notes on the replication process

1. **The authors' original code no longer runs as shipped.** It was written for R 4.0.5 (2021). Under R 4.5.1 with current CRAN packages, knitting `supplementary-information.Rmd` stops with two errors before Table 2 is reached:
   - Figure 1: `facet_grid(labeller = labeller_fun)` uses a labeller with `variable, value` arguments. This has been defunct since ggplot2 4.0.0 (installed version: 4.0.3).
   - Table 1: `estimatr::starprep()` was removed in estimatr 2.0 (installed version: 2.0.1).

   After patching those two lines, the authors' own code reproduces Table 2 exactly. The scripts in `code/` do not depend on either function.
2. **The package list is heavy.** The Rmd loads 15 packages (including DeclareDesign, lfe, stargazer, kableExtra) to produce a table that needs only `estimatr`. The `lfe` package was archived on CRAN for a while. `stargazer` is no longer maintained.
3. **The data are clean and well documented.** No recoding was required. Variable names follow a clear prefix scheme (`b_` baseline, `e_` endline, `i_` design).
4. **Things the paper does not state clearly:**
   - the reported p-values are one-sided;
   - each column controls for a different baseline variable, and column 1's control is the *hometown* voter ID;
   - income is winsorized on the full sample, before attritors are dropped.

## Robustness / credibility checks (`03_checks.R`)

| Check | Finding |
|---|---|
| A. Specifications (no covariates, no baseline DV, HC1, raw income, city FE) | Estimates range 0.236–0.243 for col 1 and 0.203–0.208 for col 2. They are very stable. |
| B. Balance (N = 2,306) | Joint F p = 0.06. **3 of 12 baseline variables are imbalanced at p < 0.05**: owns home (+6.3 pp, p = 0.001), years in city (+1.1, p = 0.006), never voted before (+4.3 pp, p = 0.018). About 0.6 would be expected by chance. Covariate adjustment barely moves the estimates. |
| C. Attrition | 8.4% in control vs 7.7% in treatment (p = 0.56). Lee bounds are tight. Under the worst-case bound the effect on voting is still +0.11. |
| D. Randomization inference (2,000 draws) | p < 0.001 for cols 1–2. Col 3 has p = 0.0005. |
| E. Self-report consistency | 5.6% of control and 7.6% of treated respondents say they voted in the city but have no city voter ID. Outcomes are self-reported and not validated against electoral rolls. |
| F. Admin data | 695 approvals among 1,138 treated (61%). Only 40% of treated respondents report holding a city ID at endline. The admin data cannot be linked to individuals, so this gap cannot be resolved. |

## Software

The original analysis used R 4.0.5 on Windows 10 with estimatr 0.30.2. This replication used R 4.5.1 on macOS with estimatr 2.0.1, ggplot2 4.0.3, flextable 0.9.10 and officer 0.7.0. The full listing is in `results/sessionInfo.txt`.
