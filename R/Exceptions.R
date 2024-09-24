#' Exceptions for Back-Testing
#'
#'Generates a vector with 0's in the days in which exceptions do not occur
#'and the daily return in the ones where the daily return exceeds VaR estimates
#'
#' @param VaR Value-at-Risk estimates for n days
#' @param data Daily Log Returns Data of the n days
#'
#' @return A vector with 0's for the non-exceptions days and daily returns in exceptions days
#' @export
#'
#' @examples Exceptions(VaR.data,actual.data)
#'
Exceptions <- function(VaR,data){

  #Generates a vector with 0's in the days in which exceptions do not occur
  #and the daily return in the ones where they do
  #VaR = Value-at-Risk estimates for n days
  #returns = daily returns data of the n days

  l <- length(VaR)
  output <- vector(mode="numeric",length=l)

  for(i in seq(l))
    if(-data[i] > VaR[i]){
      output[i] <- data[i]
    }
  return(output)
}
