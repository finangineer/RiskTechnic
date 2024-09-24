#' Univariate Monte-Carlo Simulation Method
#'
#' It performs Monte-Carlo method for univariate daily risk calculation based on the selected distribution.
#' @param data Daily Log-Return Series
#' @param dist "normal","std","skewn","skewt"
#' @param n.sim Number of Simulations
#' @param confidence.level Confidence Level for Risk calculation,e.g. 95%
#'
#' @return Value-at-Risk and Expected Shortfall Estimates
#' @export
#'
#' @examples MonteCarlo.Method(rnorm(100,1,2),dist="normal",n.sim=1000,confidence.level=0.95)
#'
MonteCarlo.Method <- function(data,dist=c("normal","std","skewn","skewt"),n.sim=1000,confidence.level=0.95){


  if (!exists("Fit.Parameters") || !is.function(Fit.Parameters)) {
    stop("The Fit.Parameters function is not loaded or defined.
         Please load or define it before running Parametric.Method.")
  }

  alpha <- (1-confidence.level)

  parameters <- Fit.Parameters(data,dist=dist)
  mu = tryCatch(parameters[["mu"]], error = function(e) NA)
  sd = tryCatch(parameters[["sigma"]], error = function(e) NA)
  df = tryCatch(parameters[["shape"]], error = function(e) NA)
  skew = tryCatch(parameters[["skew"]], error = function(e) NA)

  switch(dist,
         normal = {
           simulated.returns <- rnorm(n.sim,mean=mu,sd=sd)
           var.forecast <- - quantile(simulated.returns,alpha)
           es.forecast <- - mean(simulated.returns[simulated.returns <= -var.forecast])
         },
         std = {

           simulated.returns <- mu + sd * rdist("std",n.sim,mu = 0, sigma = 1,shape = df)
           #sigma <- sd * sqrt((df - 2) / df); simulated.returns <- rt(n.sim,df=df) * sigma + mu
           var.forecast <- - quantile(simulated.returns,alpha)
           es.forecast <- - mean(simulated.returns[simulated.returns <= -var.forecast])
         },
         skewn = {

           simulated.returns <- mu + sd * rdist("snorm",n.sim,mu = 0, sigma = 1, skew = skew)
           var.forecast <- - quantile(simulated.returns,alpha)
           es.forecast <- - mean(simulated.returns[simulated.returns <= -var.forecast])

         },
         skewt = {

           simulated.returns <- mu + sd * rdist("sstd",n.sim,mu = 0, sigma = 1, skew = skew,shape=df)
           var.forecast <- - quantile(simulated.returns,alpha)
           es.forecast <- - mean(simulated.returns[simulated.returns <= -var.forecast])

         })

  output <- data.frame(VaR = var.forecast, ES = es.forecast, row.names = dist)
  return(output)

}
