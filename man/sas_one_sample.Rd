\name{sas_one_sample}
\alias{sas_one_sample}
\title{One-Sample t-Test (SAS PROC TTEST Style)}
\usage{
sas_one_sample(x, mu = 0, conf.level = 0.95, var_name = "x")
}
\arguments{
  \item{x}{Numeric vector of observations.}
  \item{mu}{Numeric. Null hypothesis value (H0: mean = mu). Default is 0.}
  \item{conf.level}{Numeric. Confidence level for intervals. Default is 0.95.}
  \item{var_name}{Character. Label for the variable shown in output. Default is "x".}
}
\value{
Invisibly returns NULL. Output is printed to the console and plots
are drawn in the active graphics device.
}
\description{
Performs a one-sample t-test and prints output tables matching SAS PROC TTEST
formatting: Statistics, Confidence Limits, and T-Tests tables. Also produces
SAS-style diagnostic plots (histogram with density, box plot, Q-Q plot).
}
\examples{
sas_one_sample(rnorm(100, mean = 5), mu = 0, var_name = "score")
}
