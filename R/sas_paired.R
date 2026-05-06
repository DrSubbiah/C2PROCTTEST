#' Paired Samples t-Test (SAS PROC TTEST Style)
#'
#' Performs a paired samples t-test and prints output tables matching
#' SAS PROC TTEST formatting: Statistics (Differences), Confidence Limits,
#' and T-Tests tables. Also produces SAS-style diagnostic plots
#' (histogram of differences, Q-Q plot, profile plot, agreement plot).
#'
#' @param x Numeric vector. First measurement (e.g. "before").
#' @param y Numeric vector. Second measurement (e.g. "after"). Must be same length as x.
#' @param mu Numeric. Null hypothesis value for mean of differences. Default is 0.
#' @param conf.level Numeric. Confidence level for intervals. Default is 0.95.
#' @param var_name Character. Label shown in output header. Default is "before - after".
#'
#' @return Invisibly returns NULL. Output is printed to the console and
#'   plots are drawn in the active graphics device.
#'
#' @examples
#' x <- sleep$extra[sleep$group == 1]
#' y <- sleep$extra[sleep$group == 2]
#' sas_paired(x, y, var_name = "group1 - group2")
#'
#' @export
sas_paired <- function(x, y, mu = 0, conf.level = 0.95,
                        var_name = "before - after") {
  d       <- x - y
  d_clean <- d[!is.na(d)]
  n       <- length(d_clean)
  m       <- mean(d_clean)
  s       <- sd(d_clean)
  se      <- s / sqrt(n)

  ci_mean <- t.test(d_clean, mu = mu, conf.level = conf.level)$conf.int
  ci_s    <- ci_sd(s, n, conf.level)
  tt      <- t.test(x, y, paired = TRUE, mu = mu, conf.level = conf.level)

  cat("=", strrep("=", 55), "\n")
  cat("  The TTEST Procedure  \u2014  Paired:", var_name, "\n")
  cat("=", strrep("=", 55), "\n\n")

  # Statistics table
  # Mean, Std Dev, Min, Max -> 1 dp  |  Std Err -> 4 dp
  cat("--- Statistics (Differences) ---\n")
  print(data.frame(
    N       = n,
    Mean    = round(m,            1),
    Std_Dev = round(s,            1),
    Std_Err = round(se,           4),
    Minimum = round(min(d_clean), 1),
    Maximum = round(max(d_clean), 1)
  ), row.names = FALSE)

  # Confidence limits
  # Mean, CL Mean, Std Dev, CL Std Dev -> 1 dp
  cat("\n--- Confidence Limits (", conf.level*100, "%) ---\n", sep = "")
  print(data.frame(
    Mean          = round(m,             1),
    CL_Mean_Lower = round(ci_mean[1],    1),
    CL_Mean_Upper = round(ci_mean[2],    1),
    Std_Dev       = round(s,             1),
    CL_Std_Lower  = round(ci_s["lower"], 1),
    CL_Std_Upper  = round(ci_s["upper"], 1)
  ), row.names = FALSE)

  # T-test table
  # DF -> 0 dp  |  t Value -> 2 dp  |  p-value -> 4 dp
  cat("\n--- T-Tests (H0: mean diff =", mu, ") ---\n")
  print(data.frame(
    DF      = round(as.numeric(tt$parameter), 0),
    t_Value = round(as.numeric(tt$statistic), 2),
    p_value = format.pval(tt$p.value, digits = 4)
  ), row.names = FALSE)
  cat("\n")

  # SAS default plots
  plot_paired(x, y, var_name = var_name, mu = mu)

  invisible(NULL)
}
