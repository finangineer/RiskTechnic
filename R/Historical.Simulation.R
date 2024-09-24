#' Historical Simulation Method
#'
#' @param data Vector of Asset Returns
#' @param confidence.level confidence level for the risk calculation, e.g. 95%
#'
#' @return VaR and ES estimates based on a risk level
#' @export
#'
#' @examples Historical.Simulation(rnorm(100,-0.5,1),confidence.level=0.95)
Historical.Simulation <- function(data,confidence.level=0.95){

  # Calculate VaR as the quantile of historical returns
  var.forecast <- -quantile(data, probs = (1 - confidence.level))

  # Calculate ES as the mean of returns worse than VaR
  exceedances <- data[data <= -var.forecast]
  # If there are no returns worse than VaR, set ES to NA
  es.forecast <- ifelse(length(exceedances) > 0, -mean(exceedances), NA)

  output <- cbind(var.forecast,es.forecast)
  colnames(output) <- c("VaR","ES")

  return(output)
}
