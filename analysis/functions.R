# =============================================================================
# Helper functions for the thesis analysis.
# Sourced by main.R.
# =============================================================================


#' Count working hours between two POSIXct timestamps
#'
#' Working day is defined as [day_start, day_end] on weekdays in `workdays`.
#' Default: 08:00-18:00, Monday-Friday.
#'
#' @param start,end POSIXct vectors of equal length.
#' @param day_start,day_end Integer hours bounding the working day.
#' @param workdays Integer vector, 1 = Mon ... 7 = Sun.
#' @param tz Time zone used for day boundaries. Must match `start`/`end`.
#' @return Numeric vector of working hours (NA where inputs are NA, 0 where
#'         `end <= start`).
work_hours_between <- function(start, end,
                               day_start = 8, day_end = 18,
                               workdays  = 1:5,
                               tz        = "UTC") {
  n   <- length(start)
  out <- rep(NA_real_, n)
  
  for (i in seq_len(n)) {
    s <- start[i]; e <- end[i]
    if (is.na(s) || is.na(e)) next
    if (e <= s) { out[i] <- 0; next }
    
    secs     <- 0
    cur_day  <- as.Date(s, tz = tz)
    last_day <- as.Date(e, tz = tz)
    
    while (cur_day <= last_day) {
      # ISO weekday: Mon = 1 ... Sun = 7
      wd <- as.POSIXlt(cur_day)$wday
      wd <- ifelse(wd == 0L, 7L, wd)
      
      if (wd %in% workdays) {
        ws <- as.POSIXct(paste0(cur_day, sprintf(" %02d:00:00", day_start)), tz = tz)
        we <- as.POSIXct(paste0(cur_day, sprintf(" %02d:00:00", day_end)),   tz = tz)
        seg_s <- max(s, ws)
        seg_e <- min(e, we)
        if (seg_e > seg_s) {
          secs <- secs + as.numeric(difftime(seg_e, seg_s, units = "secs"))
        }
      }
      cur_day <- cur_day + 1
    }
    out[i] <- secs / 3600
  }
  out
}


#' Percentage bar plot of a binary outcome, with 95% CI error bars
#'
#' @param df Data frame.
#' @param outcome Name of the 0/1 outcome column (character).
#' @param group Name of the grouping column (character), default "RG".
#' @param ylab,title Plot labels.
#' @return A ggplot object.
rate_bar <- function(df, outcome, group = "RG",
                     ylab = "Rate", title = NULL,
                     thm = ggplot2::theme_minimal(base_size = 12) +
                       ggplot2::theme(
                         panel.grid.major.x = ggplot2::element_blank(),
                         legend.position    = "none",
                         plot.title         = ggplot2::element_text(face = "bold")
                       )) {
  agg <- df %>%
    dplyr::filter(!is.na(.data[[outcome]]), !is.na(.data[[group]])) %>%
    dplyr::group_by(.data[[group]]) %>%
    dplyr::summarise(
      n  = dplyr::n(),
      p  = mean(.data[[outcome]]),
      se = sqrt(p * (1 - p) / n),
      lo = pmax(0, p - 1.96 * se),
      hi = pmin(1, p + 1.96 * se),
      .groups = "drop"
    )
  
  ggplot2::ggplot(agg, ggplot2::aes(x = .data[[group]], y = p,
                                    fill = .data[[group]])) +
    ggplot2::geom_col() +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = lo, ymax = hi), width = 0.2) +
    ggplot2::geom_text(
      ggplot2::aes(label = paste0(round(100 * p), "%\n(n=", n, ")")),
      vjust = -0.4, size = 3
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::percent_format(accuracy = 1),
      limits = c(0, 1.05)
    ) +
    ggplot2::labs(x = NULL, y = ylab, title = title) +
    thm + ggplot2::theme(legend.position = "none")
}


#' Raw count bar plot of a binary outcome, with "count/n (%)" labels
#'
#' @param df Data frame.
#' @param outcome Name of the 0/1 outcome column (character).
#' @param group Name of the grouping column (character).
#' @param ylab,title Plot labels.
#' @param fill_palette Optional RColorBrewer palette name.
#' @return A ggplot object.
count_bar <- function(df, outcome, group,
                      ylab = "Count", title = NULL, fill_palette = NULL,
                      thm = ggplot2::theme_minimal(base_size = 12) +
                        ggplot2::theme(
                          panel.grid.major.x = ggplot2::element_blank(),
                          legend.position    = "none",
                          plot.title         = ggplot2::element_text(face = "bold")
                        )) {
  agg <- df %>%
    dplyr::filter(!is.na(.data[[outcome]]), !is.na(.data[[group]])) %>%
    dplyr::group_by(.data[[group]]) %>%
    dplyr::summarise(
      n       = dplyr::n(),
      success = sum(.data[[outcome]] == 1),
      p       = success / n,
      .groups = "drop"
    )
  
  ymax <- max(agg$success) * 1.18  # headroom for labels
  
  g <- ggplot2::ggplot(agg, ggplot2::aes(x = .data[[group]], y = success,
                                         fill = .data[[group]])) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::geom_text(
      ggplot2::aes(label = paste0(success, "/", n,
                                  "\n(", round(100 * p), "%)")),
      vjust = -0.3, size = 3.2, lineheight = 0.9
    ) +
    ggplot2::scale_y_continuous(limits = c(0, ymax), expand = c(0, 0)) +
    ggplot2::labs(x = NULL, y = ylab, title = title) +
    thm +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 15, hjust = 1))
  
  if (!is.null(fill_palette)) {
    g <- g + ggplot2::scale_fill_brewer(palette = fill_palette)
  }
  g
}
# =============================================================================
# End of functions.R.
# =============================================================================
