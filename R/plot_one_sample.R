# Internal helper: SAS-style plots for one-sample t-test.
# Panel: histogram + density overlay, box plot with CI, Q-Q plot.
plot_one_sample <- function(x, mu = 0, var_name = "x", conf.level = 0.95) {
  x <- x[!is.na(x)]
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))

  par(mfrow = c(1, 3),
      mar   = c(4, 4, 3, 1),
      oma   = c(0, 0, 3, 0))

  # 1. Histogram + density + H0 line
  h <- hist(x, plot = FALSE)
  hist(x,
       freq   = FALSE,
       col    = "#BDD7EE",
       border = "white",
       main   = "Histogram with Density",
       xlab   = var_name,
       ylim   = c(0, max(h$density, density(x)$y) * 1.15))
  lines(density(x), col = "#2E75B6", lwd = 2)
  abline(v = mu,      col = "red",     lwd = 1.5, lty = 2)
  abline(v = mean(x), col = "#70AD47", lwd = 1.5, lty = 1)
  legend("topright",
         legend = c("Density", paste0("H0 = ", mu), "Mean"),
         col    = c("#2E75B6", "red", "#70AD47"),
         lwd    = c(2, 1.5, 1.5), lty = c(1, 2, 1),
         cex = 0.75, bty = "n")

  # 2. Box plot
  ci_m <- t.test(x, mu = mu, conf.level = conf.level)$conf.int
  boxplot(x,
          col     = "#BDD7EE",
          border  = "#2E75B6",
          main    = "Box Plot",
          ylab    = var_name,
          outline = TRUE)
  abline(h = mu,   col = "red",     lwd = 1.5, lty = 2)
  abline(h = ci_m, col = "#ED7D31", lwd = 1,   lty = 3)
  legend("topright",
         legend = c(paste0("H0 = ", mu), paste0(conf.level*100, "% CI")),
         col    = c("red", "#ED7D31"),
         lwd    = c(1.5, 1), lty = c(2, 3),
         cex = 0.75, bty = "n")

  # 3. Q-Q plot
  qqnorm(x,
         main = "Normal Q-Q Plot",
         xlab = "Theoretical Quantiles",
         ylab = paste("Sample Quantiles:", var_name),
         pch  = 16, col = "#2E75B6", cex = 0.7)
  qqline(x, col = "red", lwd = 1.5)

  mtext(paste("TTEST Procedure \u2014 Variable:", var_name),
        outer = TRUE, cex = 1, font = 2)
}
