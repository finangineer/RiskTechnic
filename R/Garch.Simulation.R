#'GARCH Data Generation Process
#'
#'When the input is the log-return series, it simulates new time series by using the input specification of ARMA(p,q)-GARCH(p,q) process for the pre-specified time horizon
#'
#' @param data Log-return series
#' @param spec Specification of GARCH model  for the log-return series with ARMA(p,q) and GARCH(m,n) orders
#' @param n.sim Number of Simulations
#' @param horizon Daily Horizon, 1 for 1-day, 10 for 10-day
#'
#' @return Simulated Forecasts
#' @export
#'
#' @examples garchspec <- ugarchspec(variance.model = list(model="sGARCH",garchOrder=c(1,1)),
#' mean.model = list(armaOrder=c(0,0),include.mean=TRUE))
#' Garch.Simulation(rnorm(100,1,2),garchspec,n.sim=1000,horizon=1)

Garch.Simulation <- function(data,spec,n.sim = 1000,horizon=1){


  # Combined check for 'rugarch' and 'dplyr' packages
  if (!requireNamespace("rugarch", quietly = TRUE) || !requireNamespace("dplyr", quietly = TRUE)) {
    stop("The 'rugarch' and 'dplyr' packages are required but not installed.
         Please install them using install.packages('rugarch') and install.packages('dplyr').")
  }

  # Load the required libraries
  library(rugarch)
  library(dplyr)

  #Step 1: Fit the GARCH model
  garchfit <- ugarchfit(spec,data)
  coef <- coef(garchfit)
  #Garch Coefficients
  omega <- coalesce(coef["omega"], 0);alpha <- coalesce(coef["alpha1"], 0);beta <- coalesce(coef["beta1"], 0)
  #Auto-Regressive Coefficients
  ar1 <- coalesce(coef["ar1"], 0);ar2 <- coalesce(coef["ar2"], 0);ar3 <- coalesce(coef["ar3"], 0)
  #Moving-Average Coefficients
  ma1 <- coalesce(coef["ma1"], 0);ma2 <- coalesce(coef["ma2"], 0);ma3 <- coalesce(coef["ma3"], 0)
  mu <- coalesce(coef["mu"], 0)


  # Initialize matrices to store forecasts and volatilities
  yForecasts <- epsilon.t <- h.t <- arma.effect <- matrix(0, nrow = n.sim, ncol = horizon)
  z.t <- matrix(rnorm(n.sim * horizon), nrow = n.sim, ncol = horizon)

  # Generate the initial volatility for all simulations
  # This one should be just h.t[1,1]
  h.t[1,1] <- omega / (1 - alpha - beta)

  for (j in 1:horizon) {

    for (i in 1:n.sim) {

      epsilon.t[i, j] <- z.t[i, j] * sqrt(h.t[i, j])

      arma.effect[i,j]<- mu + ar1 * ifelse(i > 1, yForecasts[i-1,j], 0) +
        ar2 * ifelse(i > 2, yForecasts[i-2,j], 0) +
        ar3 * ifelse(i > 3, yForecasts[i-3,j], 0) +
        ma1 * ifelse(i > 1, epsilon.t[i-1,j], 0) +
        ma2 * ifelse(i > 2, epsilon.t[i-2,j], 0) +
        ma3 * ifelse(i > 3, epsilon.t[i-3,j], 0)


      yForecasts[i, j] <- arma.effect[i,j] + epsilon.t[i, j]

      if (i < n.sim) {
        # Update next volatility recursively for the same horizon
        h.t[i+1, j] <- omega + alpha * epsilon.t[i, j]^2 + beta * h.t[i, j]
      }
    }

    # For j > 1, use h.t from the previous horizon (i.e., recursive over horizon)
    if (j > 1) {

      for (i in 1:n.sim) {

        h.t[i,j] <- omega + alpha * epsilon.t[i, j-1]^2 + beta * h.t[i,j-1]
        epsilon.t[i, j] <- z.t[i, j] * sqrt(h.t[i, j])
        arma.effect[i,j]<- mu + ar1 * ifelse(i > 1, yForecasts[i-1,j-1], 0) +
          ar2 * ifelse(i > 2, yForecasts[i-2,j-1], 0) +
          ar3 * ifelse(i > 3, yForecasts[i-3,j-1], 0) +
          ma1 * ifelse(i > 1, epsilon.t[i-1,j-1], 0) +
          ma2 * ifelse(i > 2, epsilon.t[i-2,j-1], 0) +
          ma3 * ifelse(i > 3, epsilon.t[i-3,j-1], 0)
        yForecasts[i, j] <- arma.effect[i,j] + epsilon.t[i, j]
      }
    }
  }

  return(yForecasts)
}
