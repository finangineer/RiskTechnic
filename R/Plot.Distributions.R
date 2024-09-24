#' Density Plots on Histogram
#'
#' Data is fitted into the Standard Normal, Standard-t, Skewed Normal and Skewed t and then they are plotted on the histogram with different colors.
#' @param data Return Data
#'
#' @return Plots the density functions on Histogram
#' @export
#'
#' @examples Plot.Distributions(rnorm(100,1,2))

Plot.Distributions <- function(data) {

  # Check if required packages are installed, if not, install them
  if (!requireNamespace("fGarch", quietly = TRUE)) {
    install.packages("fGarch")
  }

  # Load the fGarch package
  library(fGarch)

  hist(data, breaks = 30, probability = TRUE, col = "lightblue",
       main = "Histogram with Skewed t-, t-, and Normal Distribution Densities",
       xlab = "Value", ylab = "Density")

  # 1. Fit skewed t-distribution to the data
  fit_sstd <- sstdFit(data)
  mu_sstd <- fit_sstd$estimate[["mean"]]# Location parameter for skewed t-distribution
  sd_sstd <- fit_sstd$estimate[["sd"]] # Scale parameter for skewed t-distribution
  df_sstd <- fit_sstd$estimate[["nu"]] # Degrees of freedom for skewed t
  skew_sstd <- fit_sstd$estimate[["xi"]] # Skewness parameter for skewed t

  # 2. Fit a t-distribution to the data
  fit_t <- stdFit(data)
  mu_t <- fit_t$par[["mean"]]  # Location parameter for t-distribution
  sd_t <- fit_t$par[["sd"]]  # Scale parameter for t-distribution
  df_t <- fit_t$par[["nu"]] # Degrees of freedom for t-distribution

  # 3. Fit a Skewed Normal to the data
  fit_snorm <- snormFit(returns)
  mu_snorm <- fit_snorm$par[["mean"]]  # Location parameter for Skewed Normal
  sd_snorm <- fit_snorm$par[["sd"]]  # Scale parameter for Skewed Normal
  skew_snorm <- fit_snorm$par[["xi"]] # Skewness for Skewed Normal

  # 3. Fit a normal distribution to the data
  mu_norm <- mean(data)  # Mean for normal distribution
  sd_norm <- sd(data)  # Standard deviation for normal distribution

  # Define a sequence of x values over the range of your data
  x_vals <- seq(min(data), max(data), length = 100)

  # Compute the skewed t-distribution density
  density_skt <- dsstd(x_vals, mean = mu_sstd, sd = sd_sstd, nu = df_sstd,xi=skew_sstd)

  # Add the skewed t-distribution density curve to the histogram
  lines(x_vals, density_skt, col = "red", lwd = 2,lty=2)

  # Compute the t-distribution density
  density_t <- dt((x_vals - mu_t) / sd_t, df = df_t) / sd_t

  # Add the t-distribution density curve to the histogram
  lines(x_vals, density_t, col = "blue", lwd = 2, lty = 1)

  # Compute the Skewed normal distribution density
  density_snorm <- dsnorm(x_vals, mean = mu_snorm, sd = sd_norm,xi=skew_snorm)

  # Add the Skewed normal distribution density curve to the histogram
  lines(x_vals, density_snorm, col = "purple", lwd = 2, lty = 2)

  # Compute the normal distribution density
  density_norm <- dnorm(x_vals, mean = mu_norm, sd = sd_norm)

  # Add the normal distribution density curve to the histogram
  lines(x_vals, density_norm, col = "black", lwd = 2, lty = 1)

  legend("topleft", legend = c("Skewed t-Distribution", "t-Distribution", "Skewed Normal","Normal Distribution"),
         col = c("red", "blue","purple","black"), lwd = 2, lty = c(2, 1, 2 ,1) ,cex=0.6)
}
