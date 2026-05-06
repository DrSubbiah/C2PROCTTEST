#' Two Independent Samples t-Test (SAS PROC TTEST Style)
#'
#' Performs a two independent samples t-test and prints output tables
#' matching SAS PROC TTEST formatting: Statistics, Confidence Limits,
#' T-Tests (both Pooled and Satterthwaite), and Equality of Variances
#' (Folded F) tables. Also produces SAS-style diagnostic plots
#' (overlapping histograms, side-by-side box plots, Q-Q plots per group).
#'
#' @param x Numeric vector of observations.
#' @param group Vector (character or factor) of group labels, must have exactly 2 levels.
#' @param mu Numeric. Null hypothesis value for mean difference. Default is 0.
#' @param conf.level Numeric. Confidence level for intervals. Default is 0.95.
#' @param var_name Character. Label for the variable shown in output. Default is "x".
#'
#' @return Invisibly returns NULL. Output is printed to the console and
#'   plots are drawn in the active graphics device.
#'
#' @examples
#' sas_two_sample(sleep$extra, sleep$group, var_name = "extra sleep")
#'
#' @export
sas_two_sample <- function(x, group, mu = 0, conf.level = 0.95, var_name = "x") {

  lvls <- levels(factor(group))
  if (length(lvls) != 2) stop("group must have exactly 2 levels")

  g1 <- x[group == lvls[1]];  g1 <- g1[!is.na(g1)]
  g2 <- x[group == lvls[2]];  g2 <- g2[!is.na(g2)]

  # Per-group stats helper
  # Mean, Std Dev, Min, Max -> 1 dp  |  Std Err -> 4 dp
  stats_grp <- function(g, lbl) {
    data.frame(
      Group   = lbl,
      N       = length(g),
      Mean    = round(mean(g),               1),
      Std_Dev = round(sd(g),                1),
      Std_Err = round(sd(g)/sqrt(length(g)), 4),
      Minimum = round(min(g),               1),
      Maximum = round(max(g),               1)
    )
  }

  pool <- t.test(g1, g2, var.equal = TRUE,  mu = mu, conf.level = conf.level)
  satt <- t.test(g1, g2, var.equal = FALSE, mu = mu, conf.level = conf.level)

  diff_mean <- mean(g1) - mean(g2)
  n1 <- length(g1); n2 <- length(g2)
  sp      <- sqrt(((n1-1)*var(g1) + (n2-1)*var(g2)) / (n1+n2-2))
  se_pool <- sp * sqrt(1/n1 + 1/n2)
  se_satt <- satt$stderr

  ci_s1 <- ci_sd(sd(g1), n1,      conf.level)
  ci_s2 <- ci_sd(sd(g2), n2,      conf.level)
  ci_sp <- ci_sd(sp,     n1+n2-2, conf.level)

  # Folded F: larger variance group in numerator to match SAS
  if (var(g1) >= var(g2)) {
    ftest <- var.test(g1, g2, conf.level = conf.level)
  } else {
    ftest <- var.test(g2, g1, conf.level = conf.level)
  }

  cat("=", strrep("=", 55), "\n")
  cat("  The TTEST Procedure  \u2014  Variable:", var_name, "\n")
  cat("=", strrep("=", 55), "\n\n")

  # Statistics table
  cat("--- Statistics ---\n")
  s_tbl <- rbind(
    stats_grp(g1, lvls[1]),
    stats_grp(g2, lvls[2]),
    data.frame(Group   = "Diff (1-2) Pooled",
               N       = NA,
               Mean    = round(diff_mean, 1),
               Std_Dev = round(sp,        1),
               Std_Err = round(se_pool,   4),
               Minimum = NA,
               Maximum = NA),
    data.frame(Group   = "Diff (1-2) Satterthwaite",
               N       = NA,
               Mean    = round(diff_mean, 1),
               Std_Dev = NA,
               Std_Err = round(se_satt,   4),
               Minimum = NA,
               Maximum = NA)
  )
  print(blank_na(s_tbl), row.names = FALSE)

  # Confidence limits table
  # Mean, CL Mean, Std Dev, CL Std Dev -> 1 dp
  cat("\n--- Confidence Limits (", conf.level*100, "%) ---\n", sep = "")
  ci_tbl <- data.frame(
    Group         = c(lvls[1], lvls[2],
                      "Diff (1-2) Pooled", "Diff (1-2) Satterthwaite"),
    Mean          = round(c(mean(g1), mean(g2), diff_mean, diff_mean), 1),
    CL_Mean_Lower = round(c(
      t.test(g1, conf.level = conf.level)$conf.int[1],
      t.test(g2, conf.level = conf.level)$conf.int[1],
      pool$conf.int[1], satt$conf.int[1]), 1),
    CL_Mean_Upper = round(c(
      t.test(g1, conf.level = conf.level)$conf.int[2],
      t.test(g2, conf.level = conf.level)$conf.int[2],
      pool$conf.int[2], satt$conf.int[2]), 1),
    Std_Dev      = c(round(sd(g1), 1), round(sd(g2), 1), round(sp, 1), NA),
    CL_Std_Lower = c(round(ci_s1["lower"], 1), round(ci_s2["lower"], 1),
                     round(ci_sp["lower"], 1), NA),
    CL_Std_Upper = c(round(ci_s1["upper"], 1), round(ci_s2["upper"], 1),
                     round(ci_sp["upper"], 1), NA)
  )
  print(blank_na(ci_tbl), row.names = FALSE)

  # T-test table
  # DF Pooled -> 0 dp  |  DF Satterthwaite -> 2 dp  |  t Value -> 2 dp
  cat("\n--- T-Tests (H0: diff =", mu, ") ---\n")
  t_tbl <- data.frame(
    Method    = c("Pooled", "Satterthwaite"),
    Variances = c("Equal",  "Unequal"),
    DF        = c(round(as.numeric(pool$parameter), 0),
                  round(as.numeric(satt$parameter), 2)),
    t_Value   = round(c(as.numeric(pool$statistic),
                        as.numeric(satt$statistic)), 2),
    p_value   = c(format.pval(pool$p.value, digits = 4),
                  format.pval(satt$p.value, digits = 4))
  )
  print(t_tbl, row.names = FALSE)

  # Equality of Variances — Folded F
  # Num_DF, Den_DF -> 0 dp  |  F Value -> 2 dp  |  p-value -> 4 dp
  cat("\n--- Equality of Variances (Folded F Test) ---\n")
  f_tbl <- data.frame(
    Method  = "Folded F",
    Num_DF  = as.numeric(ftest$parameter[[1]]),
    Den_DF  = as.numeric(ftest$parameter[[2]]),
    F_Value = round(as.numeric(ftest$statistic[[1]]), 2),
    p_value = format.pval(ftest$p.value, digits = 4)
  )
  print(f_tbl, row.names = FALSE)
  cat("\n")

  # SAS default plots
  plot_two_sample(g1, g2, lvls, var_name = var_name)

  invisible(NULL)
}
