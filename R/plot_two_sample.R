# Internal helper: SAS-style plots for two independent samples t-test.
# Panel: overlapping histograms, side-by-side box plots, Q-Q plot per group.
plot_two_sample <- function(g1, g2, lvls, var_name = "x") {
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))

  par(mfrow = c(2, 2),
      mar   = c(4, 4, 3, 1),
      oma   = c(0, 0, 3, 0))

  cols <- c("#BDD7EE", "#FFE699")

  # 1. Overlapping histograms
  xrange <- range(c(g1, g2))
  br     <- seq(xrange[1], xrange[2], length.out = 20)
  h1     <- hist(g1, breaks = br, plot = FALSE)
  h2     <- hist(g2, breaks = br, plot = FALSE)
  ylim   <- c(0, max(h1$density, h2$density) * 1.2)
  hist(g1, breaks = br, freq = FALSE,
       col = adjustcolor(cols[1], 0.6), border = "white",
       ylim = ylim, main = "Histograms by Group", xlab = var_name)
  hist(g2, breaks = br, freq = FALSE,
       col = adjustcolor(cols[2], 0.6), border = "white", add = TRUE)
  lines(density(g1), col = "#2E75B6", lwd = 2)
  lines(density(g2), col = "#C00000", lwd = 2)
  legend("topright", legend = lvls,
         fill = adjustcolor(cols, 0.6), bty = "n", cex = 0.75)

  # 2. Side-by-side box plots
  boxplot(list(g1, g2),
          names   = lvls,
          col     = cols,
          border  = c("#2E75B6", "#C00000"),
          main    = "Box Plots by Group",
          ylab    = var_name,
          outline = TRUE)

  # 3. Q-Q plot group 1
  qqnorm(g1, main = paste("Q-Q Plot:", lvls[1]),
         xlab = "Theoretical Quantiles",
         ylab = paste("Sample Quantiles:", var_name),
         pch = 16, col = "#2E75B6", cex = 0.7)
  qqline(g1, col = "red", lwd = 1.5)

  # 4. Q-Q plot group 2
  qqnorm(g2, main = paste("Q-Q Plot:", lvls[2]),
         xlab = "Theoretical Quantiles",
         ylab = paste("Sample Quantiles:", var_name),
         pch = 16, col = "#C00000", cex = 0.7)
  qqline(g2, col = "red", lwd = 1.5)

  mtext(paste("TTEST Procedure \u2014 Variable:", var_name, "by Group"),
        outer = TRUE, cex = 1, font = 2)
}
