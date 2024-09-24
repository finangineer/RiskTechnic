#' Univariate Parametric Risk Method
#'
#' It performs Parametric method for univariate daily risk calculation based on the selected distribution.
#' @param data Daily Log-Return Series
#' @param dist "normal","std","skewn","skewt"
#' @param confidence.level Confidence Level for Risk calculation,e.g. 95%
#'
#' @return Value-at-Risk and Expected Shortfall Estimates
#' @export
#'
#' @examples Parametric.Method(rnorm(100,1,2),dist="normal",confidence.level=0.95)
#'
Parametric.Method <- function(data,dist=c("normal","std","skewn","skewt"),confidence.level=0.95){

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
           var.forecast <- -(mu + sd * qnorm(alpha))
           # mean(-returns) + sd(-returns) * qnorm(0.95) #If losses are evaluated
           es.forecast <- -(mu - sd * dnorm(qnorm(1 - alpha)) / alpha)
         },
         std = {
           sigma <- sd * sqrt((df - 2) / df)
           var.forecast <- -(mu + sigma * qt(alpha, df))
           #var.forecast= - qst(alpha,mu=parameter[1],sd=parameter[2],df=parameter[3],scale=TRUE) (QRM PACKAGE)
           #var.forecast = - (mu + sd * qdist("std",0.05,0,1,shape=ke[4])) (RUGARCH PACKAGING)
           b <- beta(df / 2, 0.5)
           es.forecast <- - (mu - 2*sigma/(alpha*2*sqrt(df)*b)*1/((1+((qt(alpha,df))^2/df))^((df+1)/2))*(df + (qt(alpha,df))^2)/(df-1))
         },
         skewn = {
           var.forecast <- -(mu + sd * qdist("snorm", alpha, mu = 0, sigma = 1, skew = skew))
           es.forecast <- -(mu + sd * integrate(function(x) qdist("snorm", x, mu = 0, sigma = 1, skew = skew), 0, alpha)$value / alpha)
         },
         skewt = {
           var.forecast <- -(mu + sd * qdist("sstd", alpha, mu = 0, sigma = 1, skew = skew, shape = df))
           es.forecast <- -(mu + sd * integrate(function(x) qdist("sstd", x, mu = 0, sigma = 1, skew = skew, shape = df), 0, alpha)$value / alpha)
         }
  )

  output <- data.frame(VaR = var.forecast, ES = es.forecast, row.names = dist)
  return(output)

}
