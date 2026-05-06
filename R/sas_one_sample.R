#' One-Sample t-Test (SAS PROC TTEST Style)
#'
#' Performs a one-sample t-test and prints output tables matching
#' SAS PROC TTEST formatting: Statistics, Confidence Limits, and
#' T-Tests tables. Also produces SAS-style diagnostic plots
#' (histogram with density, box plot, Q-Q plot).
#'
#' @param x Numeric vector of observations.
#' @param mu Numeric. Null hypothesis value (H0: mean = mu). Default is 0.
#' @param conf.level Numeric. Confidence level for intervals. Default is 0.95.
#' @param var_name Character. Label for the variable shown in output. Default is "x".
#'
#' @return Invisibly returns NULL. Output is printed to the console and
#'   plots are drawn in the active graphics device.
#'
#' @examples
#' sas_one_sample(rnorm(100, mean = 5), mu = 0, var_name = "score")
#'
#' @export
sas_one_sample <- function(x, mu = 0, conf.level = 0.95, var_name = "x") {

  n  <- sum(!is.na(x))
  m  <- mean(x, na.rm = TRUE)
  s  <- sd(x,   na.rm = TRUE)
  se <- s / sqrt(n)
  mn <- min(x,  na.rm = TRUE)
  mx <- max(x,  na.rm = TRUE)

  ci_mean <- t.test(x, mu = mu, conf.level = conf.level)$conf.int
  ci_s    <- ci_sd(s, n, conf.level)
  tt      <- t.test(x, mu = mu, conf.level = conf.level)

  cat("=", strrep("=", 55), "\n")
  cat("  The TTEST Procedure  \u2014  Variable:", var_name, "\n")
  cat("=", strrep("=", 55), "\n\n")

  # Statistics table
  # Mean, Std Dev, Min, Max -> 1 dp  |  Std Err -> 4 dp
  cat("--- Statistics ---\n")
  print(data.frame(
    N       = n,
    Mean    = round(m,  1),
    Std_Dev = round(s,  1),
    Std_Err = round(se, 4),
    Minimum = round(mn, 1),
    Maximum = round(mx, 1)
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
  cat("\n--- T-Tests (H0: mu =", mu, ") ---\n")
  print(data.frame(
    DF      = round(as.numeric(tt$parameter), 0),
    t_Value = round(as.numeric(tt$statistic), 2),
    p_value = format.pval(tt$p.value, digits = 4)
  ), row.names = FALSE)
  cat("\n")

  # SAS default plots
  plot_one_sample(x, mu = mu, var_name = var_name, conf.level = conf.level)

  invisible(NULL)
}
