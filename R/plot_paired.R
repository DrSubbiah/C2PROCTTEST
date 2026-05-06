# Internal helper: SAS-style plots for paired samples t-test.
# Panel: histogram of differences + density, Q-Q plot,
#        profile plot (line per subject), agreement plot.
plot_paired <- function(x, y, var_name = "before - after", mu = 0) {
  d       <- x - y
  d_clean <- d[!is.na(d)]

  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))

  par(mfrow = c(2, 2),
      mar   = c(4, 4, 3, 1),
      oma   = c(0, 0, 3, 0))

  # 1. Histogram of differences + density
  h <- hist(d_clean, plot = FALSE)
  hist(d_clean,
       freq   = FALSE,
       col    = "#BDD7EE",
       border = "white",
       main   = "Histogram of Differences",
       xlab   = paste("Difference:", var_name),
       ylim   = c(0, max(h$density, density(d_clean)$y) * 1.15))
  lines(density(d_clean), col = "#2E75B6",  lwd = 2)
  abline(v = mu,            col = "red",     lwd = 1.5, lty = 2)
  abline(v = mean(d_clean), col = "#70AD47", lwd = 1.5, lty = 1)
  legend("topright",
         legend = c("Density", paste0("H0 = ", mu), "Mean diff"),
         col    = c("#2E75B6", "red", "#70AD47"),
         lwd    = c(2, 1.5, 1.5), lty = c(1, 2, 1),
         cex = 0.75, bty = "n")

  # 2. Q-Q plot of differences
  qqnorm(d_clean,
         main = "Q-Q Plot of Differences",
         xlab = "Theoretical Quantiles",
         ylab = paste("Sample Quantiles:", var_name),
         pch  = 16, col = "#2E75B6", cex = 0.7)
  qqline(d_clean, col = "red", lwd = 1.5)

  # 3. Profile plot (line per subject)
  n_obs <- length(x)
  plot(NULL,
       xlim = c(0.7, 2.3), ylim = range(c(x, y), na.rm = TRUE),
       xaxt = "n",
       main = "Profile Plot (per Subject)",
       xlab = "", ylab = var_name)
  axis(1, at = 1:2, labels = c("Before", "After"))
  for (i in seq_len(n_obs)) {
    lines(c(1, 2), c(x[i], y[i]),
          col = adjustcolor("#2E75B6", alpha.f = 0.4), lwd = 0.8)
  }
  points(rep(1, n_obs), x, pch = 16, col = "#2E75B6", cex = 0.5)
  points(rep(2, n_obs), y, pch = 16, col = "#C00000", cex = 0.5)
  lines(c(1, 2), c(mean(x, na.rm = TRUE), mean(y, na.rm = TRUE)),
        col = "black", lwd = 2.5)

  # 4. Agreement plot: After vs Before
  lims <- range(c(x, y), na.rm = TRUE)
  plot(x, y,
       pch  = 16, col = adjustcolor("#2E75B6", 0.6), cex = 0.7,
       xlim = lims, ylim = lims,
       main = "Agreement Plot (After vs Before)",
       xlab = "Before", ylab = "After")
  abline(0, 1, col = "red", lwd = 1.5, lty = 2)

  mtext(paste("TTEST Procedure \u2014 Paired:", var_name),
        outer = TRUE, cex = 1, font = 2)
}
