#' Acerbi and Szeky Conditional and Unconditional Expected Shortfall Backtesting
#'
#' Performs Acerbi and Szeky (2013) Conditional and Unconditional Expected Shortfall Backtest
#'
#' @param data Return data together with in-sample and out-of-sample
#' @param risk.var Value-at-Risk Estimates for the out-of-sample
#' @param risk.es Expected Shortfall Estimates for the out-of-sample
#' @param n.sim Number of simulation replicates
#' @param confidence.level Confidence Level for Back-Testing,e.g. 95%
#'
#' @return Test Statistics,"Critical Values for 1%, 5% and 10% levels and p-values
#' @export
#'
#' @examples getSymbols("^HSI",from="2017-06-07",to="2018-09-02",src="yahoo")
#' returns <- as.vector(na.omit(diff(log(Ad(`HSI`)))))
#' hsforecasts <- Rolling.Window(data=returns,window.size = 250,func = Historical.Simulation,confidence.level = 0.95)
#' hsforecasts <- do.call(rbind,hsforecasts)
#' varforecasts <- hsforecasts$Value_at_Risk
#' esforecasts <- hsforecasts$Expected_Shortfall
#' AcerSzeky.Backtesting(returns,varforecasts,esforecasts,n.sim = 10000)

AcerSzeky.Backtesting <- function(data,risk.var,risk.es,n.sim=1000,confidence.level=0.95){

  # Check if Exceptions function is installed
  if (!exists("Exceptions")) {
    stop("Required functions 'Exceptions' is not available. Please load or define it")
  }

  #Inputs
  alpha <- 1-confidence.level
  oos.length <- length(risk.var)
  oos.data <- tail(data,oos.length)
  excepts.var <- Exceptions(risk.var,oos.data)

  #Statictics calculation of Conditional and Unconditional Test
  statistic <- function(excepts, risk.es, divisor) sum(excepts/ risk.es) / divisor + 1
  conditional.statistic <- statistic(excepts.var,risk.es,sum(excepts.var!= 0))
  unconditional.statistic <- statistic(excepts.var,risk.es,alpha*oos.length)

  z1.statistic <- z1.breaches <- vector(length = n.sim) #conditional test
  z2.statistic <- z2.breaches <- vector(length = n.sim) #unconditional test

  #Simulations for p-value calculation
  for (i in 1:n.sim) {
    returns.sim <- rnorm(n = oos.length, mean = mean(oos.data), sd = sd(oos.data))
    excepts.sim <- Exceptions(risk.var,returns.sim)
    z1.statistic[i] <- statistic(excepts.sim,risk.es,sum(excepts.sim!= 0))
    z2.statistic[i] <- statistic(excepts.sim,risk.es,alpha*oos.length)
    z1.breaches[i] <- z1.statistic[i] < conditional.statistic
    z2.breaches[i] <- z2.statistic[i] < unconditional.statistic
  }

  combined.statistics <- cbind(z1.statistic,z2.statistic)
  critical.value <- apply(combined.statistics,2,quantile,c(0.01,0.05,0.1),na.rm=TRUE)
  p.values <- c(mean(z1.breaches,na.rm=TRUE),mean(z2.breaches,na.rm=TRUE))

  #Output
  output <- rbind(c(conditional.statistic,critical.value[,1],p.values[1]),
                  c(unconditional.statistic,critical.value[,2],p.values[2]))
  rownames(output) = c("Conditional","Unconditional")
  colnames(output) = c("Statistic","Critic 1%","Critic 5%","Critic 10%","p-value")

  return(round(output,5))
}
