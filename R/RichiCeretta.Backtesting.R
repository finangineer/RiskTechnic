#' Righi and Ceretta Expected Shortfall Backtesting
#'
#' Performs Righi and Ceretta (2013) Expected Shortfall Backtesting
#'
#' @param risk.var Value-at-Risk Estimates for the out-of-sample
#' @param data Return data together with in-sample and out-of-sample
#' @param confidence.level Confidence Level for Back-Testing,e.g. 95%
#' @param n.boots Size of the Bootstrapping Samples
#' @param n.sim Number of Simulations (Bootstrap Replicates)
#' @param garch.spec Specification of GARCH process that is required for GARCH simulation
#'
#' @return The test statistics and p-values for the days where the exceptions occur
#' @export
#'
#' @examples garchspec <- ugarchspec(variance.model = list(model="sGARCH",garchOrder=c(1,1)),
#' mean.model = list(armaOrder=c(0,0),include.mean=TRUE))
#'
#' getSymbols("^HSI",from="2017-06-07",to="2018-09-02",src="yahoo")
#' returns <- as.vector(na.omit(diff(log(Ad(`HSI`)))))
#' hsforecasts <- Rolling.Window(data=returns,window.size = 250,func = Historical.Simulation,confidence.level = 0.95)
#' hsforecasts <- do.call(rbind,hsforecasts)
#' varforecasts <- hsforecasts$Value_at_Risk
#' esforecasts <- hsforecasts$Expected_Shortfall
#' RichiCeretta.Backtesting(risk=varforecasts,data=returns,garch.spec=garchspec,n.boots=10000,n.sim=1000)
#'
RichiCeretta.Backtesting <- function(risk.var,data,confidence.level=0.95,n.boots,n.sim=10000,garch.spec){

  # Check if Garch.Simulation and Exceptions function is installed
  if (!exists("Garch.Simulation") || !exists("Exceptions")) {
    stop("Required functions 'Garch.Simulation' and 'Exceptions' are not available. Please load or define them.")
  }

  alpha <- 1-confidence.level
  l.train <- length(data) - length(risk.var)
  excepts.var <- Exceptions(risk.var,tail(data,length(risk.var)))
  n.rows <- sum(excepts.var!=0)
  output <- matrix(nrow = n.rows, ncol = 6)
  colnames(output) = c("Trading Day", "Statistic", "Critic 1%", "Critic 5%", "Critic 10%", "p-value")

  row=1
  for(i in seq_along(excepts.var)){

    if(excepts.var[i] != 0){

      ret <- excepts.var[i]
      empirical.dist = sort(Garch.Simulation(data[i:(i-1+l.train)],n.boots,garch.spec))
      l = length(empirical.dist)
      ES = mean(empirical.dist[1:(alpha * l)])
      SD = sd(empirical.dist[1:(alpha * l)])
      test.stat = (ret - ES) / SD

      #Initialize matrices
      p.values <- numeric(n.sim)
      t.crit <- matrix(NA, nrow = n.sim, ncol = 3)

      # Vectorized sampling and sorting
      replicate.dist <- replicate(n.sim, sort(sample(empirical.dist, n.boots, replace = TRUE)))

      # Calculate ESr and SDr for each replicate
      ESr <- colMeans(replicate.dist[1:(alpha * n.boots), ])
      SDr <- apply(replicate.dist[1:(alpha * n.boots), ], 2, sd)

      #BTr of the replicated distributions
      replicate.dist <- (replicate.dist - rep(ESr, each = n.boots)) / rep(SDr, each = n.boots)

      # Calculate m and p.values
      m <- colSums(replicate.dist[1:(alpha * n.boots), ] < test.stat)
      p.values <- m / (alpha * n.boots)

      # Extract t.crit
      t.crit <- t(replicate.dist[c(0.01, 0.05, 0.1) * alpha * n.boots, ])

      # Store the output
      output[row, ] <- c(i, test.stat, apply(t.crit, 2, median), median(p.values))

      row <- row + 1

    }
  }
  return(output)
}
