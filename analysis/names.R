# =============================================================================
# Names-based analysis: does our name selection for each cat. influence results?

# Assumes main.R has already been run (DATA exists in memory with
# AR, NNR, AU, RG, JOB_ID, and NAME_ID columns). NAME_ID is expected to
# take values "N1".."N18" with three names per RG cell.
# =============================================================================

# --- 1. Setup ----------

library(dplyr)
library(tidyr)
library(ggplot2)
library(gtsummary)
library(lmtest)     # lrtest() for the homogeneity test
library(broom)      # tidy() for LOO coefficient extraction

# Guard: make sure DATA and NAME_ID are available
stopifnot(exists("DATA"))
stopifnot("NAME_CODE" %in% names(DATA))

# Treat NAME_ID as a factor with the natural N1..N18 ordering
if (!is.factor(DATA$NAME_CODE)) {
  lvls <- paste0("N", 1:18)
  lvls <- lvls[lvls %in% unique(DATA$NAME_CODE)]
  DATA$NAME_CODE <- factor(DATA$NAME_CODE, levels = lvls)
}

# --- 2. Descriptive: randomization + raw counts per name ----------

# Randomisation check: applications sent per name, grouped by RG cell.
fig_randomization <- DATA %>%
  group_by(RG, NAME_CODE) %>%
  summarise(n_sent = n(), .groups = "drop") %>%
  ggplot(aes(x = NAME_CODE, y = n_sent, fill = RG)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = n_sent), vjust = -0.4, size = 3) +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Name ID", y = "Applications sent",
       fill = "Race + Gender",
       title = "Applications sent per name (randomisation check)",
       subtitle = "Bars within each colour block should be roughly level") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.x = element_blank(),
        legend.position = "bottom",
        plot.title      = element_text(face = "bold"))

fig_randomization
ggsave("output/fig_randomization.png", fig_randomization,
       width = 11, height = 5, dpi = 300)

# One row per name, with the RG cell it belongs to, total applications sent,
# and counts of each outcome. Rates are shown in parentheses next to counts
# for context, but the table leads with raw counts as requested.
#
# How to read this:
# - Compare the three rows within each RG block. If their counts look
#   similar, the cell is well-behaved.
# - If one row has noticeably fewer AR or NNR than its two siblings, that
#   name is pulling the cell mean down (or up, if it's higher).
# - Small n per name (~33) means you need fairly large gaps to worry.

name_counts <- DATA %>%
  group_by(RG, NAME_CODE) %>%
  summarise(
    n_total = n(),
    AR_n    = sum(AR  == 1, na.rm = TRUE),
    NNR_n   = sum(NNR == 1, na.rm = TRUE),
    AU_n    = sum(AU  == 1, na.rm = TRUE),
    AR_pct  = round(100 * AR_n  / n_total, 1),
    NNR_pct = round(100 * NNR_n / n_total, 1),
    AU_pct  = round(100 * AU_n  / n_total, 1),
    .groups = "drop"
  ) %>%
  arrange(RG, NAME_CODE)

print(name_counts, n = Inf)   # forces full print, not truncated

# Same information as a gtsummary table, formatted for the appendix.
# Counts appear as "AR_n (AR_pct%)", which is the standard epidemiology
# convention and reads cleanly.

tbl_names_descriptive <- DATA %>%
  select(RG, NAME_CODE, AR, NNR, AU) %>%
  tbl_strata(
    strata = RG,
    ~ tbl_summary(
      .x,
      by = NAME_CODE,
      missing = "no",
      include = c(AR, NNR, AU),
      label = list(AR  ~ "Any response",
                   NNR ~ "Non-negative response",
                   AU  ~ "Automated response"),
      statistic = all_categorical() ~ "{n} ({p}%)"
    )
  ) %>%
  modify_caption("**Appendix Table. Outcome counts per name, by Race + Gender cell**")

tbl_names_descriptive

# --- 2. Leave-one-out robustness ----------
# For each of the 18 names, re-fits m_nnr_rg on the dataset WITHOUT that
# name and collect the five RG coefficients (all cells except the
# reference, White Male). Then compares the 18 refits to the full-sample
# fit.

fit_rg <- function(df) {
  glm(NNR ~ RG, data = df, family = binomial)
}

# Full-sample reference
full_tidy <- tidy(fit_rg(DATA), conf.int = TRUE, exponentiate = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(dropped = "(full sample)")

# Leave-one-out
loo_list <- lapply(levels(DATA$NAME_CODE), function(nm) {
  fit <- fit_rg(subset(DATA, NAME_CODE != nm))
  tidy(fit, conf.int = TRUE, exponentiate = TRUE) %>%
    filter(term != "(Intercept)") %>%
    mutate(dropped = nm)
})

loo_coefs <- bind_rows(full_tidy, bind_rows(loo_list)) %>%
  mutate(dropped = factor(dropped,
                          levels = c("(full sample)", levels(DATA$NAME_CODE))))

print(loo_coefs, n = Inf)

# Visual: for each RG coefficient, shows the 18 deletion-refit ORs as a
# strip, with the full-sample OR as a horizontal reference line.
# - Horizontal dashed line = full-sample OR
# - Dots = the same OR after deleting one name

full_ref <- full_tidy %>% select(term, full_or = estimate)

loo_plot_df <- loo_coefs %>%
  filter(dropped != "(full sample)") %>%
  left_join(full_ref, by = "term")

fig_loo <- ggplot(loo_plot_df, aes(x = dropped, y = estimate)) +
  geom_hline(aes(yintercept = full_or), linetype = "dashed",
             colour = "grey40") +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0, alpha = 0.3) +
  facet_wrap(~ term, scales = "free_y", ncol = 1) +
  labs(x = "Name dropped", y = "Odds ratio (NNR vs White Male)",
       title = "Figure 7 - Names: Leave-one-out sensitivity of RG coefficients",
       subtitle = "Dashed line = full-sample estimate; dots = estimates after dropping each name") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x     = element_text(angle = 45, hjust = 1, size = 9),
        panel.grid.major.x = element_blank(),
        plot.title      = element_text(face = "bold"))

fig_loo

# =============================================================================
# End of names.R
# =============================================================================
