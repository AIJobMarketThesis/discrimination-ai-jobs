# =============================================================================
# Progression analysis: conditional on receiving a response, does
# the response *advance* the candidate (PRO == 1: interview request or
# other pipeline advancement signal) as opposed to being a non-advancing
# reply (PRO == 0: request for more documents, info etc.)? 
# PRO is a stricter outcome than NNR.
#
# All modelling is done on DATAAR (AR == 1). Assumes main.R has been
# run and DATAAR / DATA exist, both containing a 0/1 PRO column.
# =============================================================================


# --- 1. Setup ----------
library(dplyr)
library(ggplot2)
library(gtsummary)
library(table1)
library(broom)
library(survival)    # clogit for job-level fixed effects

stopifnot(exists("DATAAR"))
stopifnot("PRO" %in% names(DATAAR))

# Make sure PRO is 0/1 and the reference categories set in
# analysis.R carried through to DATAAR.
DATAAR$PRO <- as.integer(DATAAR$PRO)
stopifnot(all(DATAAR$PRO %in% c(0, 1, NA)))

# --- 2. Descriptive statistics ----------
# Raw counts of PRO == 1 per identity cell, with the denominator (number of
# responders in that cell). Reads: "out of N responders in this cell, X
# received a progression signal."

# Counts leading, gtsummary-style
tbl_pro_descriptive <- DATAAR %>%
  select(RG, PRO) %>%
  tbl_summary(
    by = RG,
    missing = "no",
    label = list(PRO ~ "Progression (interview / advancement)"),
    statistic = all_categorical() ~ "{n} / {N} ({p}%)"
  ) %>%
  add_overall() %>%
  add_p() %>%
  modify_caption("**Table. Progression outcomes among responders, by Race + Gender**") %>%
  bold_labels()

tbl_pro_descriptive

# Same thing by race and gender separately, merged
tbl_pro_ri <- DATAAR %>%
  select(RI, PRO) %>%
  tbl_summary(by = RI, missing = "no",
              statistic = all_categorical() ~ "{n} / {N} ({p}%)",
              label = list(PRO ~ "Progression")) %>%
  add_overall() %>% add_p() %>% bold_labels()

tbl_pro_gi <- DATAAR %>%
  select(GI, PRO) %>%
  tbl_summary(by = GI, missing = "no",
              statistic = all_categorical() ~ "{n} / {N} ({p}%)",
              label = list(PRO ~ "Progression")) %>%
  add_overall() %>% add_p() %>% bold_labels()

tbl_merge(
  tbls = list(tbl_pro_ri, tbl_pro_gi),
  tab_spanner = c("**By Race**", "**By Gender**")
)

# Classic table1 for a raw-count view
table1(~ PRO | RG, data = DATAAR)
table1(~ PRO | RI, data = DATAAR)
table1(~ PRO | GI, data = DATAAR)


# --- 3. Main logistic models ----------
# Mirror the NNR analysis, but restricted to responders, with PRO as the outcome.

m_pro_rg   <- glm(PRO ~ RG,              data = DATAAR, family = binomial)
m_pro_ri   <- glm(PRO ~ RI,              data = DATAAR, family = binomial)
m_pro_gi   <- glm(PRO ~ GI,              data = DATAAR, family = binomial)
m_pro_int  <- glm(PRO ~ RI:GI,           data = DATAAR, family = binomial)
m_pro_full <- glm(PRO ~ RI + GI + RI:GI, data = DATAAR, family = binomial)
m_pro_lpm  <- lm(PRO  ~ RG, data = DATA)

tbl_regression(m_pro_rg,   exponentiate = TRUE)
tbl_regression(m_pro_ri,   exponentiate = TRUE)
tbl_regression(m_pro_gi,   exponentiate = TRUE)
tbl_regression(m_pro_int,  exponentiate = TRUE)
tbl_regression(m_pro_full, exponentiate = TRUE)
tbl_regression(m_pro_lpm, vcov = vcovCL(m_pro_lpm,  cluster = ~JobID)) |>
  modify_caption("Table 22 - Linear Regression: Progression Response by Race and Gender. Clustered SE by Job.")

# Combined table
tbl_merge(
  tbls = list(
    tbl_regression(m_pro_ri,   exponentiate = TRUE),
    tbl_regression(m_pro_gi,   exponentiate = TRUE),
    tbl_regression(m_pro_rg,   exponentiate = TRUE),
    tbl_regression(m_pro_full, exponentiate = TRUE)
  ),
  tab_spanner = c("**PRO | Race**", "**PRO | Gender**",
                  "**PRO | RG**",   "**PRO | Interaction**")
)

#Forest Plots

get_or_df(m_pro_ri) |>
  make_forest(ref_label = "RIWhite - 1", title = "Progression Response - Race",
              caption = "Table 8 - Logistic Regression: Progression of Application by Race. Clustered SE by Job.",
              notes   = "OR = Odds Ratio; CI = Confidence Interval. Reference: White.") |>
  draw_forest()

get_or_df(m_pro_gi) |>
  make_forest(ref_label = "GIMale - 1", title = "Progression Response - Gender",
              caption = "Table 9 - Logistic Regression: Progression of Application by Gender. Clustered SE by Job.",
              notes   = "OR = Odds Ratio; CI = Confidence Interval. Reference: Male.") |>
  draw_forest()

get_or_df(m_pro_rg) |>
  make_forest(ref_label = "RG1 - White Male", title = "Progression Response - Intersectional",
              caption = "Table 10 - Logistic Regression: Progression of Application by Race and Gender. Clustered SE by Job.",
              notes   = "OR = Odds Ratio; CI = Confidence Interval. Reference: White Male.") |>
  draw_forest()

# --- 4. Job fixed effects ----------
# Conditional logit on responders with JOB_ID strata, exactly as in the 
# main analysis.
#
# CAVEAT: with ~6 applicants per job but only a subset of jobs producing
# any PRO == 1 case, many strata will contribute nothing to the FE
# likelihood (clogit drops jobs where PRO is constant within-strata).
# If the pooled and FE estimates disagree substantially, that's a signal
# that between-job composition was driving part of the pooled result.

m_clogit_pro     <- clogit(PRO ~ RG      + strata(JOB_ID), data = DATAAR)
m_clogit_pro_sep <- clogit(PRO ~ RI + GI + strata(JOB_ID), data = DATAAR)
m_clogit_pro_int <- clogit(PRO ~ RI * GI + strata(JOB_ID), data = DATAAR)

tbl_regression(m_clogit_pro,     exponentiate = TRUE)
tbl_regression(m_clogit_pro_sep, exponentiate = TRUE)
tbl_regression(m_clogit_pro_int, exponentiate = TRUE)

# Pooled vs FE comparison for PRO ~ RG
tbl_merge(
  tbls = list(tbl_regression(m_pro_rg,     exponentiate = TRUE),
              tbl_regression(m_clogit_pro, exponentiate = TRUE)),
  tab_spanner = c("**Pooled logit**", "**Conditional logit (Job FE)**")
)

summary(m_clogit_pro)


# --- 5. Count bars ----------
# Uses count_bar() from functions.R.
# One figure for AR (already plotted in analysis.R, re-plotted here for
# context) and one for PRO conditional on AR == 1.
#
# For the PRO figure: the denominator is responders only, NOT all applications.

# Individual panels
count_bar(DATAAR, "PRO", "RI", title = "Progression (among responders) by Race",
          ylab = "Number of progressing applications", fill_palette = "Set1")
count_bar(DATAAR, "PRO", "GI", title = "Progression (among responders) by Gender",
          ylab = "Number of progressing applications", fill_palette = "Pastel1")
count_bar(DATAAR, "PRO", "RG", title = "Progression (among responders) by Race + Gender",
          ylab = "Number of progressing applications", fill_palette = "Set2")

# Combined three-panel figure
fig_PRO <- patchwork::wrap_plots(
  count_bar(DATAAR, "PRO", "RI", title = "By Race",
            ylab = "Number of progressing applications", fill_palette = "Set1"),
  count_bar(DATAAR, "PRO", "GI", title = "By Gender",
            ylab = NULL, fill_palette = "Pastel1"),
  count_bar(DATAAR, "PRO", "RG", title = "By Race + Gender",
            ylab = NULL, fill_palette = "Set2"),
  widths = c(1, 0.8, 1.6)
) +
  patchwork::plot_annotation(
    title = "Progression signals among responders, by applicant identity",
    theme = theme(plot.title  = element_text(face = "bold", size = 14),
                  plot.margin = margin(10, 10, 10, 10))
  )

fig_PRO
ggsave("output/fig_PRO.png", fig_PRO, width = 12, height = 5, dpi = 300)

# =============================================================================
# End of progression.R
# =============================================================================
