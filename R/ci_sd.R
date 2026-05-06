# Internal helper: CI for Standard Deviation (chi-square based).
# SAS default: CI = EQUAL (equal-tailed).
ci_sd <- function(sd, n, conf.level = 0.95) {
  alpha <- 1 - conf.level
  df    <- n - 1
  lower <- sqrt(df * sd^2 / qchisq(1 - alpha/2, df))
  upper <- sqrt(df * sd^2 / qchisq(alpha/2,     df))
  c(lower = lower, upper = upper)
}
