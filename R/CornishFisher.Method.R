#' Univariate Cornish Fisher Method
#'
#'It performs Cornish Fisher method for univariate daily risk calculation.
#' @param data Daily Log-Return Series
#' @param confidence.level Confidence Level for Risk calculation,e.g. 95%
#'
#' @return Value-at-Risk and Expected Shortfall Estimates
#' @export
#'
#' @examples CornishFisher.Method(rnorm(100,2,1),confidence.level=0.95)
#'
CornishFisher.Method <- function(data,confidence.level=0.95){

  #Performs Cornish Fisher Method
  #data             : Univariate Return Data
  #confidence.level : Confidence Level for Risk Calculation
  #Output           : VaR and ES estimates

  # Check if the "moments" package is installed, if not, install it
  if (!requireNamespace("moments", quietly = TRUE)) {
    install.packages("moments")
  }

  alpha <- 1-confidence.level
  skewness <- skewness(data)
  excess.kurtosis <- kurtosis(data) - 3
  mu <- mean(data)
  sigma <- sd(data)

  z.score <- qnorm(alpha)

  adjusted.zscore.var <- z.score + (1/6) * (z.score^2 - 1) * skewness + (1 / 24)*(z.score^3 - 3 * z.score)*(excess.kurtosis) - (1 / 36) * (2 * z.score^3 - 5 * z.score) * skewness^2
  var.forecast <- - (mu + sigma * adjusted.zscore.var)
  adjusted.zscore.es <- - dnorm(qnorm(alpha))/alpha * (1+(1/6) * z.score * skewness + (1/36) * (1 - 2 * z.score^2) * skewness^2 + (1/24) * (-1 + z.score^2) * (excess.kurtosis))
  es.forecast = - (mu + sigma * adjusted.zscore.es)


  output <- data.frame(VaR = var.forecast, ES = es.forecast, row.names = "Cornish-Fisher")
  return(output)

}
