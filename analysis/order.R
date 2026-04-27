# =============================================================================
# Order analysis: distribution of Race + Gender groups across the
# within-job application ORDER positions (1-6).
#
# Assumes main.R has already been run (so DATA, thm, RG, ORDER are available).
# =============================================================================

## 1. Chi-square test: RG x ORDER ---------------------------------------------

chi_rg_order <- chisq.test(table(DATA$ORDER, DATA$RG))

cat("\nRG x ORDER chi-square test:\n")
print(chi_rg_order)


## 2. Figure 8: stacked proportional bar chart --------------------------------

ggplot(filter(DATA, !is.na(ORDER)),
       aes(x = factor(ORDER), fill = RG)) +
  geom_bar(position = "fill", width = 0.75, colour = "white") +
  geom_hline(yintercept = c(1, 2, 3, 4, 5) / 6,
             linetype = "dashed", colour = "grey30", linewidth = 0.4) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_brewer(palette = "Set2") +
  labs(title    = "Figure X: Distribution of Race and Gender Groups Across Order of Application Positions",
       subtitle = sprintf("Chi-squared test: chi2(df=%d) = %.2f, p = %.3f",
                          chi_rg_order$parameter,
                          chi_rg_order$statistic,
                          chi_rg_order$p.value),
       caption  = "Dashed lines mark equal-share thresholds (1/6 each).",
       x = "ORDER position", y = "% of candidates", fill = "RG") +
  thm + theme(legend.position = "right")

# ggsave("output/figure8_RG_by_ORDER.png", width = 9, height = 5.5, dpi = 300)

# =============================================================================
# End of order.R
# =============================================================================
