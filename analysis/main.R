# =============================================================================
# Our main analysis: race- and gender-based differences in response rates,
# response type, and response speed to job applications.
#
# Outcomes:
#   AR   = Any Response (0/1)
#   NNR  = Non-Negative Response (0/1)
#   AU   = Automated Response (0/1; <=48 calendar hours OR <=10 working hours)
#   GR1  = Gap in calendar hours between application and response
#   GR_WH = Gap in working hours (08-18, Mon-Fri)
#   PRO  = Application Progression (response type conditional on receiving a NNR)
#
# Predictors:
#   RI   = Racial Identity   (White / Asian / Other)
#   GI   = Gender Identity   (Male / Female)
#   RG   = Race + Gender     (six-level interaction factor)
#   TECH = Tech vs Non-tech company (1 for Tech)
#   ST   = Startup vs Established (1 for Startup)
# =============================================================================

# --- 1. Setup ----------

library(tidyverse)
library(readxl)
library(lubridate)
library(labelled)
library(questionr)
library(descr)
library(table1)
library(gtsummary)
library(gt)
library(broom)
library(broom.helpers)
library(lmtest)
library(fixest)
library(survival)
library(emmeans)
library(rstatix)
library(ggplot2)
library(sjPlot)
library(forestmodel)
library(patchwork)
library(sandwich)
library(dplyr)
library(forestploter)
library(grid)            
library(gridExtra)
library(stargazer)
library(modelsummary)


# --- 2. Load Data ----------
DATA <- read_xlsx("D:/Thesis Code and Datasets/Final Dataset.xlsx")
str(DATA)

# --- 3. Construct Variables ----------

## 3.1 Application and Response Timestamps

# Combine date + time into POSIXct. `as.Date() + hms()` produces UTC timestamps.
DATA$DToA <- as.Date(DATA$DoA) + hms(format(DATA$ToA, "%H:%M:%S"))
DATA$DToR <- as.Date(DATA$DoR) + hms(format(DATA$ToR, "%H:%M:%S"))

# Calendar-hour gap - GR1
DATA$GR1 <- as.numeric(
  difftime(DATA$DToR,
           DATA$DToA,
           units = "hours")
)

# Working-hour gap (business clock: 08-18, Mon-Fri, UTC) - GR_WH
source("functions.R")
DATA$GR_WH <- work_hours_between(DATA$DToA, DATA$DToR)


## 3.2 Race + Gender Combined Variable - RG

DATA$RG <- "NA"
DATA$RG[DATA$RI=="White - 1" & DATA$GI== "Male - 1"] <- "1- White Male"
DATA$RG[DATA$RI=="White - 1" & DATA$GI=="Female - 2"] <- "2- White Female"
DATA$RG[DATA$RI=="Asian - 2" & DATA$GI=="Male - 1"] <- "3- Asian Male"
DATA$RG[DATA$RI=="Asian - 2" & DATA$GI=="Female - 2"] <- "4- Asian Female"
DATA$RG[DATA$RI=="Other - 3" & DATA$GI=="Male - 1"] <- "5- Other Male"
DATA$RG[DATA$RI=="Other - 3" & DATA$GI=="Female - 2"] <- "6- Other Female"
DATA$RG<-as.factor(DATA$RG)
var_label(DATA$RG) <- "Race + Gender"

## 3.3 Automated Response - AU

# AU = 1 if reply arrived within EITHER 48 calendar hours OR 10 working hours.
# AU = NA if there was no response at all.

DATA$AU <- ifelse(
  is.na(DATA$GR1) & is.na(DATA$GR_WH), NA_integer_,
  as.integer(DATA$GR1 <= 48 | DATA$GR_WH <= 10)
)
var_label(DATA$AU)    <- "Automated Response (<=48h OR <=10 working h)"
var_label(DATA$GR_WH) <- "Gap between application and response in working hours (08-18, Mon-Fri)"

freq(DATA$AU)

## 3.4 Automated response x NNR cross-factor

# Only defined for AU == 1. Splits automated replies into negative vs
# non-negative, so we can see whether fast replies are mostly rejections.
DATA$AU_NNR <- factor(
  ifelse(DATA$AU == 1 & DATA$NNR == 1, "1 - Automated non-negative",
         ifelse(DATA$AU == 1 & DATA$NNR == 0, "0 - Automated negative", NA)),
  levels = c("0 - Automated negative", "1 - Automated non-negative")
)
var_label(DATA$AU_NNR) <- "NNR | Automated Response"

## 3.5 Single mutually-exclusive outcome category

DATA$OUTCOME <- factor(
  ifelse(is.na(DATA$AR) | DATA$AR == 0,      "1. No response",
         ifelse(DATA$AU == 1   & DATA$NNR == 0,     "2. Automated rejection",
                ifelse(DATA$AU == 1   & DATA$NNR == 1,     "3. Automated non-negative",
                       ifelse(DATA$AU == 0   & DATA$NNR == 0,     "4. Human rejection",
                              "5. Human non-negative")))),
  levels = c("1. No response", "2. Automated rejection",
             "3. Automated non-negative",
             "4. Human rejection", "5. Human non-negative")
)

## 3.6 Reference categories/relevel

DATA$GI <- relevel(factor(DATA$GI), ref = "Male - 1")
DATA$RI <- relevel(factor(DATA$RI), ref = "White - 1")
DATA$RG <- relevel(factor(DATA$RG), ref = "1- White Male")

# Job ID as a factor (column name has a space in the source file)
DATA$JOB_ID <- as.factor(DATA$`JobID`) #Change variable name if different in excel
DATA$JobID <- as.factor(DATA$`JobID`)

## 3.7 Mass-rejections
# A response counts as a mass rejection when every candidate on that job
# was rejected within the span of an hour relative to each other. 
# Flag lives on DATA so any subset inherits it.

DATA <- DATA %>%
  group_by(JobID) %>%
  mutate(
    MASS_REJ = as.integer(
      n() == 6 &
        sum(NNR == 0, na.rm = TRUE) == 6 &
        as.numeric(difftime(max(DToR, na.rm = TRUE),
                            min(DToR, na.rm = TRUE),
                            units = "mins")) <= 60
    )
  ) %>%
  ungroup()

## 3.8 - Building Visualisation Themes

# Global ggplot theme used by helper functions
thm <- theme_minimal(base_size = 12) +
  theme(panel.grid.major.x = element_blank(),
        legend.position    = "none",
        plot.title         = element_text(face = "bold"))

#Broom helpers clearence
tidy_fun = broom.helpers::tidy_parameters

#Build Forest Plot Template for visualising results

get_or_df <- function(model, cluster = ~JobID) {
  vcov_cl <- vcovCL(model, cluster = cluster)
  data.frame(
    term      = names(coef(model)),
    OR        = exp(coef(model)),
    conf.low  = exp(coefci(model, vcov = vcov_cl)[, 1]),
    conf.high = exp(coefci(model, vcov = vcov_cl)[, 2]),
    p.value   = coeftest(model, vcov = vcov_cl)[, 4]
  ) %>% filter(term != "(Intercept)")
}

get_or_df_clogit <- function(model) {
  vc <- vcov(model)       # uses robust variance from cluster() in formula
  se <- sqrt(diag(vc))
  z  <- coef(model) / se
  
  data.frame(
    term      = names(coef(model)),
    OR        = exp(coef(model)),
    conf.low  = exp(coef(model) - 1.96 * se),
    conf.high = exp(coef(model) + 1.96 * se),
    p.value   = 2 * pnorm(-abs(z))
  ) %>% filter(term != "(Intercept)")
}

make_forest <- function(or_df, ref_label, title = "",
                        notes = NULL, caption = NULL) {
  
  ref_row <- data.frame(
    term = ref_label, OR = 1, conf.low = NA, conf.high = NA, p.value = NA
  )
  all_rows <- bind_rows(ref_row, or_df)
  
  dt <- all_rows %>%
    mutate(
      `OR (95% CI)` = ifelse(is.na(conf.low), "Reference",
                             sprintf("%.2f (%.2f, %.2f)", OR, conf.low, conf.high)),
      `p`           = ifelse(is.na(p.value), "—",
                             ifelse(p.value < 0.001, "<0.001",
                                    sprintf("%.3f", p.value))),
      ` `           = paste(rep(" ", 50), collapse = "")
    ) %>%
    rename(Variable = term) %>%
    select(Variable, ` `, `OR (95% CI)`, p)
  
  p <- forest(
    dt,
    est       = c(1, or_df$OR),
    lower     = c(1, or_df$conf.low),
    upper     = c(1, or_df$conf.high),
    ci_column = 2,
    ref_line  = 1,
    xlim      = c(0.2, 2.2),
    ticks_at  = c(0.4, 0.6, 0.8, 1.0, 1.2, 1.6),
    title     = paste0(title, "\n"),
    theme     = forest_theme(
      base_size   = 14,
      base_family = "serif",
      refline_gp  = gpar(lty = "dashed", col = "grey40"),
      ci_pch      = 15,
      ci_col      = "black",
      ci_lwd      = 1.5,
      ci_Theight  = 0.2,
      core = list(
        bg_params = list(fill = c("white", "#f0f0f0")),
        padding   = unit(c(8, 4), "mm")
      )
    )
  )
  
  p <- add_border(p, part = "header", where = "top",    gp = gpar(lwd = 2))
  p <- add_border(p, part = "body",   where = "bottom", gp = gpar(lwd = 2))
  p <- add_border(p, col = 1, part = "body",   where = "left",  gp = gpar(lwd = 2))
  p <- add_border(p, col = 1, part = "header", where = "left",  gp = gpar(lwd = 2))
  p <- add_border(p, col = 4, part = "body",   where = "right", gp = gpar(lwd = 2))
  p <- add_border(p, col = 4, part = "header", where = "right", gp = gpar(lwd = 2))
  p <- add_border(p, part = "header", where = "bottom", gp = gpar(lwd = 2))
  p <- add_border(p, col = 1, part = "body",   where = "right", gp = gpar(lwd = 0.5))
  p <- add_border(p, col = 1, part = "header", where = "right", gp = gpar(lwd = 0.5))
  p <- add_border(p, col = 2, part = "body",   where = "right", gp = gpar(lwd = 0.5))
  p <- add_border(p, col = 2, part = "header", where = "right", gp = gpar(lwd = 0.5))
  p <- add_border(p, col = 3, part = "body",   where = "right", gp = gpar(lwd = 0.5))
  p <- add_border(p, col = 3, part = "header", where = "right", gp = gpar(lwd = 0.5))
  
  list(plot = p, caption = caption, notes = notes)
}

draw_forest <- function(forest_obj) {
  p       <- forest_obj$plot
  caption <- forest_obj$caption
  notes   <- forest_obj$notes
  
  grob_list   <- list(p)
  height_list <- list(unit(1, "null"))
  
  if (!is.null(caption)) {
    grob_list <- c(grob_list, list(
      textGrob(caption, x = 0.5, just = "center",
               gp = gpar(fontsize = 12, fontfamily = "serif", fontface = "bold"))
    ))
    height_list <- c(height_list, list(unit(1.8, "lines")))
  }
  
  if (!is.null(notes)) {
    grob_list <- c(grob_list, list(
      textGrob(paste0("Note: ", notes), x = 0.5, just = "center",
               gp = gpar(fontsize = 9, fontfamily = "serif", fontface = "italic"))
    ))
    height_list <- c(height_list, list(unit(1.5, "lines")))
  }
  
  grid.arrange(
    grobs   = grob_list,
    ncol    = 1,
    heights = do.call(unit.c, height_list)
  )
}

make_comparative_forest <- function(or_df1, or_df2,
                                    ref_label,
                                    label1  = "Non-Tech",
                                    label2  = "Tech",
                                    title   = "",
                                    caption = NULL,
                                    notes   = NULL) {
  
  ref_row <- data.frame(
    term = ref_label, OR = 1, conf.low = NA, conf.high = NA, p.value = NA
  )
  
  rows1 <- bind_rows(ref_row, or_df1)
  rows2 <- bind_rows(ref_row, or_df2)
  
  fmt_or <- function(OR, low, high)
    ifelse(is.na(low), "Reference", sprintf("%.2f (%.2f, %.2f)", OR, low, high))
  fmt_p  <- function(p)
    ifelse(is.na(p), "--", ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)))
  
  dt <- setNames(data.frame(
    rows1$term,
    paste(rep(" ", 25), collapse = ""),
    fmt_or(rows1$OR, rows1$conf.low, rows1$conf.high),
    fmt_p(rows1$p.value),
    paste(rep(" ", 25), collapse = ""),
    fmt_or(rows2$OR, rows2$conf.low, rows2$conf.high),
    fmt_p(rows2$p.value),
    stringsAsFactors = FALSE
  ), c("Variable",
       label1, paste0(label1, " OR (95% CI)"), paste0(label1, " p"),
       label2, paste0(label2, " OR (95% CI)"), paste0(label2, " p")))
  
  p <- forest(
    dt,
    est       = list(c(1, or_df1$OR),       c(1, or_df2$OR)),
    lower     = list(c(1, or_df1$conf.low),  c(1, or_df2$conf.low)),
    upper     = list(c(1, or_df1$conf.high), c(1, or_df2$conf.high)),
    ci_column = c(2, 5),
    ref_line  = 1,
    xlim      = c(0.2, 2.2),
    ticks_at  = c(0.4, 0.8, 1.0, 1.6),
    title     = paste0(title, "\n"),
    theme     = forest_theme(
      base_size    = 12,
      base_family  = "serif",
      refline_gp   = gpar(lty = "dashed", col = "grey40"),
      ci_pch       = c(15, 16),                          # square vs circle
      ci_col       = "steelblue3",     # blue vs orange
      ci_fill      = "steelblue3",  # add this — fills the point
      ci_lwd       = 2.5,                             # was 1.5
      ci_Theight   = 0.5,                            # was 0.2
      ci_alpha     = 1,   
      legend_name  = "Sector",
      legend_value = c(label1, label2),
      core = list(
        bg_params = list(fill = c("white", "#f0f0f0")),
        padding   = unit(c(8, 4), "mm")
      )
    )
  )
  
  p <- edit_plot(p, col = 5,row = 1:nrow(dt), which = "ci",
                 gp = gpar(col = "darkorange2", fill = "darkorange2"))
  p <- add_border(p, part = "header", where = "top",    gp = gpar(lwd = 2))
  p <- add_border(p, part = "body",   where = "bottom", gp = gpar(lwd = 2))
  p <- add_border(p, col = 1, part = "body",   where = "left",  gp = gpar(lwd = 2))
  p <- add_border(p, col = 1, part = "header", where = "left",  gp = gpar(lwd = 2))
  p <- add_border(p, col = 7, part = "body",   where = "right", gp = gpar(lwd = 2))
  p <- add_border(p, col = 7, part = "header", where = "right", gp = gpar(lwd = 2))
  p <- add_border(p, part = "header", where = "bottom", gp = gpar(lwd = 1))
  
  for (col in 1:6) {
    p <- add_border(p, col = col, part = "body",   where = "right", gp = gpar(lwd = 0.5))
    p <- add_border(p, col = col, part = "header", where = "right", gp = gpar(lwd = 0.5))
  }
  
  list(plot = p, caption = caption, notes = notes)
}

# --- 4. Subsets for Analysis ----------

DATAU  <- subset(DATA, AU == 1) # automated responses only
DATAAR <- subset(DATA, AR == 1) # any response at all
DATA01 <- subset(DATA, NNR == 0) # rejections only
DATA01_ind <- subset(DATA, NNR == 0 & MASS_REJ == 0)  # mass rejections filtered out
DATA02 <- subset(DATA, TECH == 0) # non-tech
DATA03 <- subset(DATA, TECH == 1) # tech
DATA04 <- subset(DATA, ST == 0) # excluding startups

# --- 5. Descriptive Statistics ----------

## 5.1 Frequencies TECH / ST
freq(DATA$TECH)
freq(DATA$ST)

## 5.2 Tables
table1(~ AR + NNR | RI, data = DATA) # by race
table1(~ AR + NNR | GI, data = DATA) # by gender
table1(~ AR + NNR + GR1 | RG, data = DATA) # by race + gender

# Automation patterns
table1(~ AU + GR_WH | RG, data = DATA)
table1(~ AU | RI, data = DATA)
table1(~ AU | GI, data = DATA)

# Within automated responses: negative vs non-negative, by identity and speed
table1(~ AU_NNR | RG, data = DATAU)
table1(~ GR1 + GR_WH | AU_NNR, data = DATAU)

# Master table (for backup)
table1(~ AR + NNR + AU_NNR + GR1+ GR_WH  | RG, data = DATA)

## 5.3 Polished tables (gtsummary)

TAB_SAMPLE_RG <- DATA %>%
  select(RG, AR, NNR, AU, TECH, ST, GR1) %>%
  tbl_summary(
    by      = RG,
    missing = "no",
    label   = list(
      AR   ~ "Any response",
      NNR  ~ "Non-negative response",
      AU   ~ "Automated response",
      TECH ~ "Tech sector",
      ST   ~ "Startup",
      GR1  ~ "Response time (hours)"
    ),
    statistic = list(
      all_continuous()  ~ "{median} ({p25}, {p75})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 1
  ) %>%
  add_overall() %>%
  add_p() %>%
  modify_header(label ~ "**Variable**") %>%
  modify_caption("**Table 1. Sample characteristics by Race + Gender**") %>%
  bold_labels()

TAB_SAMPLE_RI <- DATA %>%
  select(RI, AR, NNR, AU) %>%
  tbl_summary(
    by = RI, missing = "no",
    label = list(AR ~ "Any response", NNR ~ "Non-negative", AU ~ "Automated")
  ) %>%
  add_overall() %>% add_p() %>% bold_labels() %>%
  modify_caption("**Table 2a. Outcomes by Race**")

TAB_SAMPLE_GI <- DATA %>%
  select(GI, AR, NNR, AU) %>%
  tbl_summary(
    by = GI, missing = "no",
    label = list(AR ~ "Any response", NNR ~ "Non-negative", AU ~ "Automated")
  ) %>%
  add_overall() %>% add_p() %>% bold_labels() %>%
  modify_caption("**Table 2b. Outcomes by Gender**")

TAB_SAMPLE_RG
tbl_merge(
  tbls = list(TAB_SAMPLE_RI, TAB_SAMPLE_GI),
  tab_spanner = c("**By Race**", "**By Gender**")
)

# Export to Word:
TAB_SAMPLE_RG %>% as_gt() %>% gt::gtsave("table1_sample.docx")

# --- 6. Main Regression Models ----------

## 6.1 Any Response (AR)

m_ar_rg   <- glm(AR ~ RG, data = DATA, family = binomial)
m_ar_ri   <- glm(AR ~ RI, data = DATA, family = binomial)
m_ar_gi   <- glm(AR ~ GI, data = DATA, family = binomial)
m_ar_lpm  <- lm(AR  ~ RG,    data = DATA)   # linear probability


tbl_regression(m_ar_rg, exponentiate = TRUE, vcov = vcovCL(m_ar_rg,   cluster = ~JobID))
tbl_regression(m_ar_ri, exponentiate = TRUE, vcov = vcovCL(m_ar_ri,   cluster = ~JobID))
tbl_regression(m_ar_gi, exponentiate = TRUE, vcov = vcovCL(m_ar_gi,   cluster = ~JobID))
tbl_regression(m_ar_lpm, vcov = vcovCL(m_ar_lpm, cluster = ~JobID)) |>
  modify_caption("Table 20: Linear Regression: Any Response by Race and Gender. Clustered SE by Job.")

plot_model(m_ar_lpm, type = "pred", terms = "RG")

# Forest Model Plots

get_or_df(m_ar_ri) |>
  make_forest(ref_label = "RIWhite - 1", title = "Any Response - Race",
              caption = "Table 2 - Logistic Regression: Any Response by Race. Clustered SE by Job.",
              notes   = "OR = Odds Ratio; CI = Confidence Interval. Reference: White.") |>
  draw_forest()

get_or_df(m_ar_gi) |>
  make_forest(ref_label = "GIMale - 1", title = "Any Response - Gender",
              caption = "Table 3 - Logistic Regression: Any Response by Gender. Clustered SE by Job.",
              notes   = "OR = Odds Ratio; CI = Confidence Interval. Reference: Male.") |>
  draw_forest()

get_or_df(m_ar_rg) |>
  make_forest(ref_label = "RG1- White Male", title = "Any Response - Intersectional",
              caption = "Table 4 - Logistic Regression: Any Response by Race and Gender. Clustered SE by Job.",
              notes   = "OR = Odds Ratio; CI = Confidence Interval. Reference: White Male.") |>
  draw_forest()


## 6.2 Non-Negative Response

m_nnr_rg   <- glm(NNR ~ RG, data = DATA, family = binomial)
m_nnr_ri   <- glm(NNR ~ RI, data = DATA, family = binomial)
m_nnr_gi   <- glm(NNR ~ GI, data = DATA, family = binomial)
m_nnr_lpm  <- lm(NNR  ~ RG, data = DATA)

tbl_regression(m_nnr_rg, exponentiate = TRUE, vcov = vcovCL(m_nnr_rg,   cluster = ~JobID))
tbl_regression(m_nnr_ri, exponentiate = TRUE, vcov = vcovCL(m_nnr_ri,   cluster = ~JobID))
tbl_regression(m_nnr_gi, exponentiate = TRUE, vcov = vcovCL(m_nnr_gi,   cluster = ~JobID))
tbl_regression(m_nnr_lpm, vcov = vcovCL(m_nnr_lpm,  cluster = ~JobID)) |>
  modify_caption("Table 21 - Linear Regression: Non-Negative Response by Race and Gender. Clustered SE by Job.")

plot_model(m_nnr_rg, type = "pred", terms = "RG", transform = "exp")
plot_model(m_nnr_gi, type = "pred", terms = "GI")
plot_model(m_nnr_rg, type = "est",  transform = "exp", 
           show.values = TRUE, show.p = TRUE, value.offset = 0.3) # not clustered errors
plot_model(m_nnr_lpm,  type = "pred", terms = "RG")

# Forest Model Plots

get_or_df(m_nnr_ri) |>
  make_forest(ref_label = "RIWhite - 1", title = "Non-Negative Response - Race",
              caption = "Table 5 - Logistic Regression: Non-Negative Response by Race. Clustered SE by Job.",
              notes   = "OR = Odds Ratio; CI = Confidence Interval. Reference: White.") |>
  draw_forest()

get_or_df(m_nnr_gi) |>
  make_forest(ref_label = "GIMale - 1", title = "Non-Negative Response - Gender",
              caption = "Table 6 - Logistic Regression: Non-Negative Response by Gender. Clustered SE by Job.",
              notes   = "OR = Odds Ratio; CI = Confidence Interval. Reference: Male.") |>
  draw_forest()

get_or_df(m_nnr_rg) |>
  make_forest(ref_label = "RG1 - White Male", title = "Non-Negative Response - Intersectional",
              caption = "Table 7 - Logistic Regression: Non-Negative Response by Race and Gender. Clustered SE by Job.",
              notes   = "OR = Odds Ratio; CI = Confidence Interval. Reference: White Male.") |>
  draw_forest()

## 6.3 Automated Response

m_au_rg <- glm(AU ~ RG, data = DATA, family = binomial)
m_au_ri <- glm(AU ~ RI, data = DATA, family = binomial)
m_au_gi <- glm(AU ~ GI, data = DATA, family = binomial)
m_au_lpm  <- lm(AU  ~ RG, data = DATA)

tbl_regression(m_au_rg, exponentiate = TRUE, vcov = vcovCL(m_au_rg, cluster = ~JobID))
tbl_regression(m_au_ri, exponentiate = TRUE, vcov = vcovCL(m_au_ri, cluster = ~JobID))
tbl_regression(m_au_gi, exponentiate = TRUE, vcov = vcovCL(m_au_gi, cluster = ~JobID))
tbl_regression(m_au_lpm, vcov = vcovCL(m_au_lpm,  cluster = ~JobID)) |>
  modify_caption("Table 23 - Linear Regression: Automated Response by Race and Gender. Clustered SE by Job.")

#Forest Plots
get_or_df(m_au_ri) |>
  make_forest(ref_label = "RIWhite - 1", title = "Automated Response - Race",
              caption = "Table 11 - Logistic Regression: Automated Response by Race. Clustered SE by Job.",
              notes   = "OR = Odds Ratio; CI = Confidence Interval. Reference: White.") |>
  draw_forest()

get_or_df(m_au_gi) |>
  make_forest(ref_label = "GIMale - 1", title = "Automated Response - Gender",
              caption = "Table 12 - Logistic Regression: Automated Response by Gender. Clustered SE by Job.",
              notes   = "OR = Odds Ratio; CI = Confidence Interval. Reference: Male.") |>
  draw_forest()

get_or_df(m_au_rg) |>
  make_forest(ref_label = "RG1- White Male", title = "Automated Response - Intersectional",
              caption = "Table 13 - Logistic Regression: Automated Response by Race and Gender. Clustered SE by Job.",
              notes   = "OR = Odds Ratio; CI = Confidence Interval. Reference: White Male.") |>
  draw_forest()



## 6.4 Combined Tables
tbl_merge(
  tbls = list(
    tbl_regression(m_ar_rg,  exponentiate = TRUE, vcov = vcovCL(m_ar_rg,cluster = ~JobID)),
    tbl_regression(m_nnr_rg, exponentiate = TRUE, vcov = vcovCL(m_nnr_rg,cluster = ~JobID)),
    tbl_regression(m_au_rg, exponentiate = TRUE, vcov = vcovCL(m_au_rg,cluster = ~JobID))
  ),
  tab_spanner = c("**AR Model**", "**NNR Model**", "**AU Model**")
)

get_cl_stats <- function(model) {
  vcov_cl <- vcovCL(model, cluster = ~JobID)
  list(
    coef = exp(coef(model)),
    se   = sqrt(diag(vcov_cl)),
    p    = coeftest(model, vcov_cl)[, 4]
  )
}

s_ar  <- get_cl_stats(m_ar_rg)
s_nnr <- get_cl_stats(m_nnr_rg)
s_au  <- get_cl_stats(m_au_rg)

stargazer(
  m_ar_rg, m_nnr_rg, m_au_rg,
  coef           = list(s_ar$coef,  s_nnr$coef,  s_au$coef),
  se             = list(s_ar$se,    s_nnr$se,    s_au$se),
  p              = list(s_ar$p,     s_nnr$p,     s_au$p),
  column.labels  = c("Any Response", "Non-Neg Response", "Automated Response"),
  dep.var.labels.include = FALSE,
  title          = "Main Results: Logistic Regression (Odds Ratios)",
  notes          = "Clustered standard errors by job posting in parentheses. * p<0.1, ** p<0.05, *** p<0.01",
  notes.append   = FALSE,
  star.cutoffs   = c(0.1, 0.05, 0.01),
  type           = "html",   # change to "latex" for paper output
  out           = "table_main.html"
)


# --- 7. Sub-sample Regressions ----------

## 7.1 Tech vs Non-tech

m_nnr_nontech_rg   <- glm(NNR ~ RG, data = DATA02, family = binomial)
m_nnr_nontech_ri   <- glm(NNR ~ RI, data = DATA02, family = binomial)
m_nnr_nontech_gi   <- glm(NNR ~ GI, data = DATA02, family = binomial)

m_nnr_tech_rg   <- glm(NNR ~ RG, data = DATA03, family = binomial)
m_nnr_tech_ri   <- glm(NNR ~ RI, data = DATA03, family = binomial)
m_nnr_tech_gi   <- glm(NNR ~ GI, data = DATA03, family = binomial)

tbl_merge(
  tbls = list(
    tbl_regression(m_nnr_nontech_ri, exponentiate = TRUE, vcov = vcovCL(m_nnr_nontech_ri, cluster = ~JobID)),
    tbl_regression(m_nnr_tech_ri,    exponentiate = TRUE, vcov = vcovCL(m_nnr_tech_ri,    cluster = ~JobID))
  ),
  tab_spanner = c("**Non-Tech**", "**Tech**")
)

tbl_merge(
  tbls = list(
    tbl_regression(m_nnr_nontech_gi, exponentiate = TRUE, vcov = vcovCL(m_nnr_nontech_gi, cluster = ~JobID)),
    tbl_regression(m_nnr_tech_gi,    exponentiate = TRUE, vcov = vcovCL(m_nnr_tech_gi,    cluster = ~JobID))
  ),
  tab_spanner = c("**Non-Tech**", "**Tech**")
)

tbl_merge(
  tbls = list(
    tbl_regression(m_nnr_nontech_rg, exponentiate = TRUE, vcov = vcovCL(m_nnr_nontech_rg, cluster = ~JobID)),
    tbl_regression(m_nnr_tech_rg,    exponentiate = TRUE, vcov = vcovCL(m_nnr_tech_rg,    cluster = ~JobID))
  ),
  tab_spanner = c("**Non-Tech**", "**Tech**")
)

## Comparative forest plot

make_comparative_forest(
  or_df1    = get_or_df(m_nnr_nontech_ri),
  or_df2    = get_or_df(m_nnr_tech_ri),
  ref_label = "RIWhite-1",
  label1    = "Non-Tech", label2 = "Tech",
  title     = "NNR | Tech vs Non-Tech - Race",
  caption   = "Table 14 - Logistic regression: Non-Negative Response by Race, Tech vs Non-Tech Subsample. Clustered SE by job posting.",
  notes     = "OR = Odds Ratio; CI = Confidence Interval. Reference: White. Blue = Non-Tech, Orange = Tech."
) |> draw_forest()

make_comparative_forest(
  or_df1    = get_or_df(m_nnr_nontech_gi),
  or_df2    = get_or_df(m_nnr_tech_gi),
  ref_label = "GIMale - 1",
  label1    = "Non-Tech", label2 = "Tech",
  title     = "NNR | Tech vs Non-Tech - Gender",
  caption   = "Table 15 - Logistic regression: Non-Negative Response by Gender, Tech vs Non-Tech Subsample. Clustered SE by job posting.",
  notes     = "OR = Odds Ratio; CI = Confidence Interval. Reference: Male. Blue = Non-Tech, Orange = Tech."
) |> draw_forest()

make_comparative_forest(
  or_df1    = get_or_df(m_nnr_nontech_rg),
  or_df2    = get_or_df(m_nnr_tech_rg),
  ref_label = "RG1- White Male",
  label1    = "Non-Tech", label2 = "Tech",
  title     = "NNR | Tech vs Non-Tech - Intersectional",
  caption   = "Table 16 - Logistic regression: Non-Negative Response by Race and Gender, Tech vs Non-Tech Subsample. Clustered SE by job posting",
  notes     = "OR = Odds Ratio; CI = Confidence Interval. Reference: White Male. Blue = Non-Tech, Orange = Tech."
) |> draw_forest()

## 7.2 Excluding startups
m_nnr_nost_rg <- glm(NNR ~ RG, data = DATA04, family = binomial)
m_nnr_nost_ri <- glm(NNR ~ RI, data = DATA04, family = binomial)
m_nnr_nost_gi <- glm(NNR ~ GI, data = DATA04, family = binomial)

tbl_regression(m_nnr_nost_rg, exponentiate = TRUE, vcov = vcovCL(m_nnr_nost_rg, cluster = ~JobID))
tbl_regression(m_nnr_nost_ri, exponentiate = TRUE, vcov = vcovCL(m_nnr_nost_ri, cluster = ~JobID))
tbl_regression(m_nnr_nost_gi, exponentiate = TRUE, vcov = vcovCL(m_nnr_nost_gi, cluster = ~JobID))

get_or_df(m_nnr_nost_ri) |>
  make_forest(ref_label = "RIWhite - 1", title = "NNR | Established Firms - Race",
              caption = "Table 17 - Logistic Regression: Non-Negative Response by Race, Established Firms Subsample. Clustered SE by job posting.",
              notes   = "OR = Odds Ratio; CI = Confidence Interval. Reference: White.") |>
  draw_forest()

get_or_df(m_nnr_nost_gi) |>
  make_forest(ref_label = "GIMale - 1", title = "NNR | Established Firms - Gender",
              caption = "Table 18 - Logistic Regression: Non-Negative Response by Gender, Established Firms Subsample. Clustered SE by job posting.",
              notes   = "OR = Odds Ratio; CI = Confidence Interval. Reference: Male.") |>
  draw_forest()

get_or_df(m_nnr_nost_rg) |>
  make_forest(ref_label = "RG1- White Male", title = "NNR | Established Firms - Intersectional",
              caption = "Table 19 - Logistic Regression: Non-Negative Response by Race and Gender, Established Firms Subsample. Clustered SE by job posting.",
              notes   = "OR = Odds Ratio; CI = Confidence Interval. Reference: White Male.") |>
  draw_forest()

# --- 8. Job-level Fixed Effects -----------------------------------------------


## 8.1 Linear Probability with Job FE (clustering specified in model)

m_fe_ar     <- feols(AR  ~ RG | JobID, data = DATA, cluster = ~JobID)
m_fe_nnr    <- feols(NNR ~ RG | JobID, data = DATA, cluster = ~JobID)
m_fe_nnr_ri <- feols(NNR ~ RI | JobID, data = DATA, cluster = ~JobID)
m_fe_nnr_gi <- feols(NNR ~ GI | JobID, data = DATA, cluster = ~JobID)
m_fe_au     <- feols(AU  ~ RG | JobID, data = DATA, cluster = ~JobID)


etable(m_fe_ar, m_fe_nnr, m_fe_au, headers = c("AR (LPM)", "NNR (LPM)", "AU (LPM)"))

etable(
  m_fe_ar, m_fe_nnr, m_fe_nnr_ri, m_fe_nnr_gi, m_fe_au,
  headers     = c("AR", "NNR", "NNR RI", "NNR GI", "AU"),
  title       = "Linear Probability Models with Job Fixed Effects",
  notes       = "Clustered SE by job posting. * p<0.1, ** p<0.05, *** p<0.01",
  signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
)

modelsummary(
  list("AR" = m_fe_ar, "NNR" = m_fe_nnr,
       "NNR RI" = m_fe_nnr_ri, "NNR GI" = m_fe_nnr_gi,
       "AU" = m_fe_au),
  stars   = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  gof_map = c("nobs", "r.squared"),
  title   = "Table 24: OLS Regressions with Job Fixed Effects for Outcome variables ",
  notes   = list("Standard errors in parentheses, clustered at the Job Level. Reference Race = White, Gender = Male, Race & Gender = White Male")
)

tbl_regression(m_fe_ar)
tbl_regression(m_fe_nnr)
tbl_regression(m_fe_nnr_ri)
tbl_regression(m_fe_nnr_gi)
tbl_regression(m_fe_au)


## 8.2 Conditional Logit with Job FE

m_clogit_ar      <- clogit(AR  ~ RG + strata(JobID)+ cluster(JobID), data = DATA, method = "efron")
m_clogit_ar_ri   <- clogit(AR  ~ RI + strata(JobID)+ cluster(JobID), data = DATA, method = "efron")
m_clogit_ar_gi   <- clogit(AR  ~ GI + strata(JobID)+ cluster(JobID), data = DATA, method = "efron")


m_clogit_nnr     <- clogit(NNR ~ RG + strata(JobID)+ cluster(JobID), data = DATA, method = "efron")
m_clogit_nnr_ri  <- clogit(NNR ~ RI + strata(JobID)+ cluster(JobID), data = DATA, method = "efron")
m_clogit_nnr_gi  <- clogit(NNR ~ GI + strata(JobID)+ cluster(JobID), data = DATA, method = "efron")
m_clogit_nnr_sep <- clogit(NNR ~ RI + GI + strata(JobID)+ cluster(JobID), data = DATA, method = "efron")
m_clogit_nnr_int <- clogit(NNR ~ RI * GI + strata(JobID)+ cluster(JobID), data = DATA, method = "efron")


m_clogit_au      <- clogit(AU  ~ RG + strata(JobID)+ cluster(JobID), data = DATA, method = "efron")
m_clogit_au_ri   <- clogit(AU  ~ RI + strata(JobID)+ cluster(JobID), data = DATA, method = "efron")
m_clogit_au_gi   <- clogit(AU  ~ GI + strata(JobID)+ cluster(JobID), data = DATA, method = "efron")


tbl_merge(
  tbls = list(
    tbl_regression(m_clogit_ar,  exponentiate = TRUE),
    tbl_regression(m_clogit_nnr, exponentiate = TRUE),
    tbl_regression(m_clogit_au,  exponentiate = TRUE)
  ),
  tab_spanner = c("**AR | Job FE**", "**NNR | Job FE**", "**AU | Job FE**")
)



tbl_regression(m_clogit_nnr_sep, exponentiate = TRUE, vcov = vcovCL(m_clogit_nnr_sep, cluster = ~JobID))
tbl_regression(m_clogit_nnr_int, exponentiate = TRUE, vcov = vcovCL(m_clogit_nnr_int, cluster = ~JobID))


# Forest plots — AR with Job FE
get_or_df(m_clogit_ar_ri, cluster = ~JobID) |>
  make_forest(ref_label = "White", title = "AR Cond. Logit - Race",
              caption = "Conditional logit (job FE): any response by race. Clustered SE by job posting.",
              notes   = "OR = Odds Ratio; CI = Confidence Interval. Reference: White.") |>
  draw_forest()


get_or_df_clogit(m_clogit_ar_gi) |>
  make_forest(ref_label = "Male", title = "AR Cond. Logit - Gender",
              caption = "Conditional logit (job FE): any response by gender. Clustered SE by job posting.",
              notes   = "OR = Odds Ratio; CI = Confidence Interval. Reference: Male.") |>
  draw_forest()


get_or_df(m_clogit_ar, cluster = ~JobID) |>
  make_forest(ref_label = "1- White Male", title = "AR Cond. Logit - Intersectional",
              caption = "Conditional logit (job FE): any response by race and gender. Clustered SE by job posting.",
              notes   = "OR = Odds Ratio; CI = Confidence Interval. Reference: White Male.") |>
  draw_forest()


# Forest plots — NNR with Job FE
get_or_df(m_clogit_nnr_ri, cluster = ~JobID) |>
  make_forest(ref_label = "White", title = "NNR Cond. Logit - Race",
              caption = "Conditional logit (job FE): non-negative response by race. Clustered SE by job posting.",
              notes   = "OR = Odds Ratio; CI = Confidence Interval. Reference: White.") |>
  draw_forest()


get_or_df_clogit(m_clogit_nnr_gi) |>
  make_forest(ref_label = "Male", title = "NNR Cond. Logit - Gender",
              caption = "Conditional logit (job FE): non-negative response by gender. Clustered SE by job posting.",
              notes   = "OR = Odds Ratio; CI = Confidence Interval. Reference: Male.") |>
  draw_forest()


get_or_df(m_clogit_nnr, cluster = ~JobID) |>
  make_forest(ref_label = "1- White Male", title = "NNR Cond. Logit - Intersectional",
              caption = "Conditional logit (job FE): non-negative response by race and gender. Clustered SE by job posting.",
              notes   = "OR = Odds Ratio; CI = Confidence Interval. Reference: White Male.") |>
  draw_forest()


# Forest plots — AU with Job FE
get_or_df(m_clogit_au_ri, cluster = ~JobID) |>
  make_forest(ref_label = "White", title = "AU Cond. Logit - Race",
              caption = "Conditional logit (job FE): automated response by race. Clustered SE by job posting.",
              notes   = "OR = Odds Ratio; CI = Confidence Interval. Reference: White.") |>
  draw_forest()


get_or_df_clogit(m_clogit_au_gi) |>
  make_forest(ref_label = "Male", title = "AU Cond. Logit - Gender",
              caption = "Conditional logit (job FE): automated response by gender. Clustered SE by job posting.",
              notes   = "OR = Odds Ratio; CI = Confidence Interval. Reference: Male.") |>
  draw_forest()


get_or_df(m_clogit_au, cluster = ~JobID) |>
  make_forest(ref_label = "1- White Male", title = "AU Cond. Logit - Intersectional",
              caption = "Conditional logit (job FE): automated response by race and gender. Clustered SE by job posting.",
              notes   = "OR = Odds Ratio; CI = Confidence Interval. Reference: White Male.") |>
  draw_forest()


## 8.3 Pooled vs FE Comparison
tbl_merge(
  tbls = list(
    tbl_regression(m_nnr_rg, exponentiate = TRUE, vcov = vcovCL(m_nnr_rg, cluster = ~JobID)),
    tbl_regression(m_clogit_nnr, exponentiate = TRUE, vcov = vcovCL(m_clogit_nnr, cluster = ~JobID))
  ),
  tab_spanner = c("**Pooled Logit**", "**Conditional Logit (Job FE)**")
)
summary(m_clogit_nnr)


## 8.4 Continuous Outcome with Job FE
m_fe_gr1 <- feols(GR1 ~ RG | JobID, data = subset(DATA, NNR == 0), cluster = ~JobID)
etable(m_fe_gr1)
summary(m_fe_gr1)

modelsummary(
  list("Response Gap (hours)" = m_fe_gr1),
  stars     = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  gof_map   = c("nobs", "r.squared", "adj.r.squared"),
  title     = "Response Time Gap by Race-Gender (Job FE, Non-Negative Responses Only)",
  notes     = "OLS with job fixed effects. Clustered SE by job posting. Sample restricted to non-negative responses (NNR = 0). Outcome is calendar hours to response.",
)


# ---- 9. Response-speed analysis (GR1) ----------
# Uses DATA01_ind: rejections only, with mass rejections removed, so as
# not to bias the identity comparison.

## 9.1 Distribution plots
ggplot(DATA, aes(x = GR1)) +
  geom_density() +
  theme_minimal() +
  labs(x = "GR1 (hours)", y = "Density",
       title = "Overall distribution of response time")

# response in days + AU cut-off at 2
ggplot(DATA01_ind, aes(x = GR1 / 24, colour = RG)) +
  geom_density() +
  geom_vline(xintercept = 2, linetype = "dashed", colour = "grey40", linewidth = 0.5) +
  annotate("text", x = 2.5, y = 0.020, label = "AU cutoff (48h)",
           hjust = 0, vjust = 1.5, size = 3.3,
           colour = "grey40", fontface = "italic") +
  theme_minimal() +
  labs(x = "Response time (days)", y = "Density", colour = "RG",
       title = "Response time density by Race + Gender (individual rejections)")

# response in days + AU cut-off at 2 + scaled down to 30 days
ggplot(DATA01_ind, aes(x = GR1 / 24, colour = RG)) +
  geom_density() +
  geom_vline(xintercept = 2, linetype = "dashed", colour = "grey40", linewidth = 0.5) +
  annotate("text", x = 2.35, y = 0.020, label = "AU cutoff (48h)",
           hjust = 0, vjust = 1.5, size = 3.3,
           colour = "grey40", fontface = "italic") +
  coord_cartesian(xlim = c(0, 30)) +
  theme_minimal() +
  labs(x = "Response Time (days)", y = "Density", colour = "RG",
       title = "Figure 5: Response Time Density - Intersectional (individual rejections)")

ggplot(DATA01_ind, aes(x = GR1)) +
  geom_density(fill = "steelblue", alpha = 0.4) +
  facet_wrap(~ RG, scales = "free") +
  theme_minimal() +
  labs(x = "Time gap (hours)", y = "Density",
       title = "Distribution of Time Gap by Race + Gender (individual rejections)")

ggplot(DATA01_ind, aes(x = RG, y = GR1, fill = RG)) +
  geom_boxplot(outlier.alpha = 0.3) +
  scale_y_log10() +
  labs(x = NULL, y = "Hours to rejection (log scale)",
       title = "Time to rejection by Race + Gender (individual rejections)") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x     = element_text(angle = 20, hjust = 1))


## 9.2 Linear model
m_gr1_rg <- lm(GR1 ~ RG, data = DATA01_ind)
plot_model(m_gr1_rg, type = "pred", terms = "RG")


## 9.3 Kruskal-Wallis (non-parametric)
# H0: distribution of GR1 is the same across all RG groups.

kt <-kruskal.test(GR1 ~ RG, data = DATA01_ind)

stars <- ifelse(kt$p.value < 0.01, "***",
                ifelse(kt$p.value < 0.05, "**",
                       ifelse(kt$p.value < 0.10, "*", "")))

cat(sprintf(
  "Kruskal-Wallis test: H(%d) = %.2f, p = %.3f%s",
  kt$parameter, kt$statistic, kt$p.value, stars
))

DATA01_ind %>% kruskal_test(GR1 ~ RG)
DATA01_ind %>% kruskal_effsize(GR1 ~ RG)

DATA01_ind %>%
  group_by(RG) %>%
  summarise(
    n      = n(),
    median = median(GR1,         na.rm = TRUE),
    q25    = quantile(GR1, 0.25, na.rm = TRUE),
    q75    = quantile(GR1, 0.75, na.rm = TRUE),
    mean   = mean(GR1,           na.rm = TRUE)
  )

# Test specifically for early rejections - 1 week
DATA01_ind$early_reject <- as.integer(DATA01_ind$GR1 <= 240)
chisq.test(table(DATA01_ind$RG, DATA01_ind$early_reject))
# Just Other Female vs White Male #yates continuity - Closest to significance
targeted <- DATA01_ind %>%
  filter(RG %in% c("1- White Male", "6- Other Female")) |>
  mutate(RG = droplevels(RG))   

chisq.test(table(targeted$RG, targeted$early_reject))

library(survival)

# Treat rejection time as survival outcome
surv_obj <- Surv(time  = DATA01_ind$GR1,
                 event = rep(1, nrow(DATA01_ind)))

# Test whether rejection timing differs by RG
survdiff(surv_obj ~ RG, data = DATA01_ind)

# Cox model for group-specific hazard ratios
cox_reject <- coxph(surv_obj ~ RG, data = DATA01_ind)
summary(cox_reject)

# Fleming-Harrington: rho=1 weights early differences heavily
survdiff(surv_obj ~ RG, data = DATA01_ind, rho = 1)

# rho=0.5 moderate early weighting
survdiff(surv_obj ~ RG, data = DATA01_ind, rho = 0.5)

#pairwise Wilcoxon Tests
library(rstatix)

# All pairwise comparisons with correction
DATA01_ind %>%
  wilcox_test(GR1 ~ RG, p.adjust.method = "bonferroni") %>%
  filter(group1 == "1- White Male" | group2 == "1- White Male")

# Cox Model Visulaisation
cox_df <- broom::tidy(cox_reject, exponentiate = TRUE, conf.int = TRUE) %>%
  mutate(term = str_remove(term, "^RG"))

ggplot(cox_df, aes(x = estimate, y = term, xmin = conf.low, xmax = conf.high)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50") +
  geom_errorbarh(height = 0.2) +
  geom_point(size = 3, shape = 15) +
  theme_minimal(base_size = 12) +
  labs(x     = "Hazard Ratio (faster rejection = HR > 1)",
       y     = NULL,
       title = "Figure 6: nRejection speed relative to White Male",
       caption = "Reference: White Male. HR > 1 = rejected faster.")


# --- Panel A: Distributional Tests ---
panel_a <- data.frame(
  Test = c(
    "Kruskal-Wallis rank-sum test (all groups)",
    "Pearson chi-squared: early rejection ≤10 days (all groups)",
    "Pearson chi-squared: early rejection ≤10 days (Other Female vs White Male)"
  ),
  Statistic = c("H = 4.37", "χ² = 6.10", "χ² = 3.60"),
  df        = c(5, 5, 1),
  p_value   = c("0.497", "0.297", "0.058†"),
  Note      = c(
    "No significant difference in rejection time distributions across groups",
    "No significant difference in early rejection rates across all six groups",
    "Borderline significant; Other Female applicants rejected earlier than White Male"
  )
)

# --- Panel B: Cox Model ---
panel_b <- data.frame(
  Group     = c("White Female", "Asian Male", "Asian Female",
                "Other Male",  "Other Female"),
  HR        = c(1.071, 1.251, 1.361, 1.114, 1.391),
  CI_low    = c(0.704, 0.836, 0.905, 0.749, 0.928),
  CI_high   = c(1.628, 1.871, 2.047, 1.658, 2.084),
  p_value   = c("0.750", "0.277", "0.139", "0.595", "0.110")
) %>%
  mutate(`95% CI` = sprintf("(%.2f, %.2f)", CI_low, CI_high)) %>%
  select(Group, HR, `95% CI`, p_value)


# --- Build gt table ---
tbl_a <- panel_a %>%
  gt() %>%
  tab_header(
    title    = md("**Table 25 - Tests of Distributional Differences in Rejection Timing**"),
    subtitle = md("*Panel A: Non-Parametric and Chi-Squared tests*")
  ) %>%
  cols_label(
    Test      = md("**Test**"),
    Statistic = md("**Test Statistic**"),
    df        = md("**df**"),
    p_value   = md("**p-value**"),
    Note      = md("**Interpretation**")
  ) %>%
  cols_width(
    Test      ~ px(220),
    Statistic ~ px(100),
    df        ~ px(40),
    p_value   ~ px(80),
    Note      ~ px(250)
  ) %>%
  tab_footnote(
    footnote = "† p < 0.10. Yates' continuity correction applied for 2×2 table.",
    locations = cells_column_labels(columns = p_value)
  ) %>%
  tab_footnote(
    footnote = md("Note: Analysis restricted to applications receiving a rejection (n = 335 after excluding 87 observations). Early rejection defined as response received within 240 hours (10 days)."),
    locations = cells_title(groups = "title")
  ) %>%
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style     = cell_fill(color = "#f7f7f7"),
    locations = cells_body(rows = seq(1, nrow(panel_a), 2))
  ) %>%
  opt_table_font(font = "Times New Roman") %>%
  tab_options(
    table.border.top.width    = px(2),
    table.border.bottom.width = px(2),
    heading.border.bottom.width = px(1),
    column_labels.border.bottom.width = px(1)
  )

tbl_b <- panel_b %>%
  gt() %>%
  tab_header(
    title    = md("**Table 26. Tests of Distributional Differences in Rejection Timing**"),
    subtitle = md("*Panel B: Cox proportional hazards model — hazard ratios for rejection speed*")
  ) %>%
  cols_label(
    Group   = md("**Applicant Group**"),
    HR      = md("**Hazard Ratio**"),
    `95% CI` = md("**95% CI**"),
    p_value = md("**p-value**")
  ) %>%
  fmt_number(columns = HR, decimals = 2) %>%
  tab_footnote(
    footnote = "Reference category: White Male. HR > 1 indicates faster rejection relative to White Male. All models include n = 335 observations.",
    locations = cells_column_labels(columns = Group)
  ) %>%
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style     = cell_fill(color = "#f7f7f7"),
    locations = cells_body(rows = seq(1, nrow(panel_b), 2))
  ) %>%
  opt_table_font(font = "Times New Roman") %>%
  tab_options(
    table.border.top.width    = px(2),
    table.border.bottom.width = px(2),
    column_labels.border.bottom.width = px(1)
  )

# Print both
tbl_a
tbl_b

# Save to Word-compatible HTML
gtsave(tbl_a, "supplementary_table_S1a.html")
gtsave(tbl_b, "supplementary_table_S1b.html")

# ---- 10. Two-stage / hurdle models ----------
# Logic: treat the response process as a two-stage decision.
#   Stage 1: does the company respond at all? (AR)
#   Stage 2a: conditional on responding, what is the outcome? (NNR)
#   Stage 2b: conditional on responding, how fast? (log GR1)
# No selection correction assumed (independent stages).

## 10.1 Setup A: response x response quality

m_hurdle_a_s1 <- glm(AR  ~ RG, data = DATA, family = binomial)
m_hurdle_a_s2 <- glm(NNR ~ RG, data = subset(DATA, AR == 1), family = binomial)

tbl_merge(
  tbls = list(tbl_regression(m_hurdle_a_s1, exponentiate = TRUE),
              tbl_regression(m_hurdle_a_s2, exponentiate = TRUE)),
  tab_spanner = c("**Stage 1: Any Response**",
                  "**Stage 2: Non-Negative | Responded**")
)

# By race and gender separately
m_hurdle_a_s1_r <- glm(AR  ~ RI, data = DATA, family = binomial)
m_hurdle_a_s2_r <- glm(NNR ~ RI, data = subset(DATA, AR == 1), family = binomial)
m_hurdle_a_s1_g <- glm(AR  ~ GI, data = DATA, family = binomial)
m_hurdle_a_s2_g <- glm(NNR ~ GI, data = subset(DATA, AR == 1), family = binomial)

tbl_merge(
  tbls = list(tbl_regression(m_hurdle_a_s1_r, exponentiate = TRUE),
              tbl_regression(m_hurdle_a_s2_r, exponentiate = TRUE)),
  tab_spanner = c("**Stage 1 (Race)**", "**Stage 2 (Race)**")
)

tbl_merge(
  tbls = list(tbl_regression(m_hurdle_a_s1_g, exponentiate = TRUE),
              tbl_regression(m_hurdle_a_s2_g, exponentiate = TRUE)),
  tab_spanner = c("**Stage 1 (Gender)**", "**Stage 2 (Gender)**")
)


## 10.2 Setup B: response x response speed
# Log transform GR1 because it is right-skewed.

DATA_RESPONDED <- subset(DATA, AR == 1 & !is.na(GR1) & GR1 > 0)
DATA_RESPONDED$logGR1 <- log(DATA_RESPONDED$GR1)

m_hurdle_b_s1 <- m_hurdle_a_s1   # same stage-1 specification
m_hurdle_b_s2 <- lm(logGR1 ~ RG, data = DATA_RESPONDED)

tbl_merge(
  tbls = list(tbl_regression(m_hurdle_b_s1, exponentiate = TRUE),
              tbl_regression(m_hurdle_b_s2)),
  tab_spanner = c("**Stage 1: Any Response**",
                  "**Stage 2: log(Hours) | Responded**")
) |>
  modify_caption("**Table 27: Hurdle Model Linear Regressions for Race and Gender**")


# Interpretation: stage-2 coefficients are differences in log-hours.
# exp(coef) - 1 is the approximate % difference in response time vs reference.

# ---- 11. Bar graphs: outcomes by identity ----------

# 11.1 Count bars per outcome x grouping
# Any Response
count_bar(DATA, "AR", "RI", title = "Responses received by Race",
          ylab = "Number of responses", fill_palette = "Set1")
count_bar(DATA, "AR", "GI", title = "Responses received by Gender",
          ylab = "Number of responses", fill_palette = "Pastel1")
count_bar(DATA, "AR", "RG", title = "Responses received by Race + Gender",
          ylab = "Number of responses", fill_palette = "Set2")

# Non-negative Response
count_bar(DATA, "NNR", "RI", title = "Non-negative responses by Race",
          ylab = "Number of non-negative responses", fill_palette = "Set1")
count_bar(DATA, "NNR", "GI", title = "Non-negative responses by Gender",
          ylab = "Number of non-negative responses", fill_palette = "Pastel1")
count_bar(DATA, "NNR", "RG", title = "Non-negative responses by Race + Gender",
          ylab = "Number of non-negative responses", fill_palette = "Set2")


## 11.2 Three-panel combined figures (patchwork)
fig_AR <- (
  count_bar(DATA, "AR", "RI", title = "By Race",
            ylab = "Number of responses", fill_palette = "Set1") +
    count_bar(DATA, "AR", "GI", title = "By Gender",
              ylab = NULL, fill_palette = "Pastel1") +
    count_bar(DATA, "AR", "RG", title = "By Race + Gender",
              ylab = NULL, fill_palette = "Set2")
) +
  plot_layout(widths = c(1, 0.8, 1.6)) +
  plot_annotation(
    title = "Responses received by applicant identity",
    theme = theme(plot.title  = element_text(face = "bold", size = 14),
                  plot.margin = margin(10, 10, 10, 10))
  )

fig_NNR <- (
  count_bar(DATA, "NNR", "RI", title = "By Race",
            ylab = "Number of non-negative responses", fill_palette = "Set1") +
    count_bar(DATA, "NNR", "GI", title = "By Gender",
              ylab = NULL, fill_palette = "Pastel1") +
    count_bar(DATA, "NNR", "RG", title = "By Race + Gender",
              ylab = NULL, fill_palette = "Set2")
) +
  plot_layout(widths = c(1, 0.8, 1.6)) +
  plot_annotation(
    title = "Non-negative responses by applicant identity",
    theme = theme(plot.title  = element_text(face = "bold", size = 14),
                  plot.margin = margin(10, 10, 10, 10))
  )

fig_AR
fig_NNR

ggsave("output/fig_AR.png",  fig_AR,  width = 12, height = 5, dpi = 300)
ggsave("output/fig_NNR.png", fig_NNR, width = 12, height = 5, dpi = 300)


## 11.3 Stacked AU composition
au_outcome_shares <- DATAU %>%
  filter(!is.na(NNR), !is.na(RG)) %>%
  mutate(NNR_lab = factor(ifelse(NNR == 1, "Automated non-negative", "Automated rejection"),
                          levels = c("Automated rejection", "Automated non-negative"))) %>%
  count(RG, NNR_lab) %>%
  group_by(RG) %>%
  mutate(share = n / sum(n)) %>%
  ungroup()

ggplot(au_outcome_shares, aes(x = RG, y = share, fill = NNR_lab)) +
  geom_col(position = "fill") +
  geom_text(
    aes(label = paste0(round(100 * share), "% (", n, ")")),
    position = position_fill(vjust = 0.5),
    size = 3.2, colour = "black", fontface = "italic"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  labs(x = NULL, y = "Share of automated responses",
       fill = "Automated response type",
       title = "Automated response composition by Race + Gender") +
  thm + theme(legend.position = "right")

# ---- 12. Odds-ratio plots ---------------------------------------------------

## 12.1 Forest plots
forest_model(m_nnr_rg)
forest_model(m_nnr_lpm)


## 12.2 OR heatmap by race x gender
newdata <- expand.grid(RI = levels(DATA$RI),
                       GI = levels(DATA$GI))
newdata$pred_logit <- predict(m_nnr_int, newdata, type = "link")
newdata$OR         <- exp(newdata$pred_logit)

ggplot(newdata, aes(x = RI, y = GI, fill = OR)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = round(OR, 2)), size = 5) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                       midpoint = 1, name = "Odds Ratio") +
  theme_minimal() +
  labs(title = "Odds Ratios of Callback by Gender and Race",
       x = "Race", y = "Gender") +
  theme(text = element_text(size = 12),
        panel.grid = element_blank())


## 12.3 Line + ribbon of ORs from RG logit
or_df <- tidy(m_nnr_rg, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(OR        = exp(estimate),
         conf.low  = exp(conf.low),
         conf.high = exp(conf.high))

ggplot(or_df, aes(x = term, y = OR, group = 1)) +
  geom_line() +
  geom_point(size = 3) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  theme_minimal() +
  labs(x = "Race + Gender", y = "Odds Ratio")

# 13. Clustered errors plots

## Helper function
plot_clustered_se <- function(model, title, subtitle = "Reference: White Male  |  95% CIs shown") {
  
  vcov_cl    <- vcovCL(model, cluster = ~JobID)
  coef_cl    <- coeftest(model, vcov = vcov_cl)
  coef_naive <- coeftest(model)
  
  make_df <- function(ct, label) {
    data.frame(
      term      = rownames(ct),
      estimate  = ct[, "Estimate"],
      std_error = ct[, "Std. Error"],
      se_type   = label,
      stringsAsFactors = FALSE
    )
  }
  
  plot_df <- bind_rows(
    make_df(coef_naive, "Naive SE"),
    make_df(coef_cl,    "Clustered SE (job level)")
  ) |>
    filter(term != "(Intercept)") |>
    mutate(
      ci_lo      = estimate - 1.96 * std_error,
      ci_hi      = estimate + 1.96 * std_error,
      term_label = term |>
        str_remove("^RG|^RI|^GI") |>
        str_trim()
    )
  
  ggplot(plot_df, aes(x = estimate, y = term_label,
                      colour = se_type,
                      xmin = ci_lo, xmax = ci_hi)) +
    geom_vline(xintercept = 0, linetype = "dashed",
               colour = "grey60", linewidth = 0.4) +
    geom_errorbarh(aes(height = 0),
                   position = position_dodge(width = 0.5),
                   linewidth = 0.8) +
    geom_point(position = position_dodge(width = 0.5), size = 3) +
    scale_colour_manual(
      values = c("Naive SE"                  = "#378ADD",
                 "Clustered SE (job level)"  = "#D85A30"),
      name   = NULL
    ) +
    labs(
      title    = title,
      subtitle = subtitle,
      x        = "Coefficient (log-odds)",
      y        = NULL,
      caption  = "Clustered standard errors at the job level"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position  = "bottom",
      panel.grid.minor = element_blank()
    )
}


## 13.1 Any Response (AR)
plot_clustered_se(m_ar_rg, "Any Response — Intersectional")
plot_clustered_se(m_ar_ri, "Any Response — Race",
                  subtitle = "Reference: White  |  95% CIs shown")
plot_clustered_se(m_ar_gi, "Any Response — Gender",
                  subtitle = "Reference: Male  |  95% CIs shown")

## 13.2 Non-Negative Response (NNR)
plot_clustered_se(m_nnr_rg, "Non-Negative Response — Intersectional")
plot_clustered_se(m_nnr_ri, "Non-Negative Response — Race",
                  subtitle = "Reference: White  |  95% CIs shown")
plot_clustered_se(m_nnr_gi, "Non-Negative Response — Gender",
                  subtitle = "Reference: Male  |  95% CIs shown")

## 13.3 Automated Response (AU)
plot_clustered_se(m_au_rg, "Automated Response — Intersectional")
plot_clustered_se(m_au_ri, "Automated Response — Race",
                  subtitle = "Reference: White  |  95% CIs shown")
plot_clustered_se(m_au_gi, "Automated Response — Gender",
                  subtitle = "Reference: Male  |  95% CIs shown")

# =============================================================================
# End of main.R
# =============================================================================
