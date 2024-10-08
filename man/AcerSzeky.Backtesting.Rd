% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AcerSzeky.Backtesting.R
\name{AcerSzeky.Backtesting}
\alias{AcerSzeky.Backtesting}
\title{Acerbi and Szeky Conditional and Unconditional Expected Shortfall Backtesting}
\usage{
AcerSzeky.Backtesting(
  data,
  risk.var,
  risk.es,
  n.sim = 1000,
  confidence.level = 0.95
)
}
\arguments{
\item{data}{Return data together with in-sample and out-of-sample}

\item{risk.var}{Value-at-Risk Estimates for the out-of-sample}

\item{risk.es}{Expected Shortfall Estimates for the out-of-sample}

\item{n.sim}{Number of simulation replicates}

\item{confidence.level}{Confidence Level for Back-Testing,e.g. 95\%}
}
\value{
Test Statistics,"Critical Values for 1\%, 5\% and 10\% levels and p-values
}
\description{
Performs Acerbi and Szeky (2013) Conditional and Unconditional Expected Shortfall Backtest
}
\examples{
getSymbols("^HSI",from="2017-06-07",to="2018-09-02",src="yahoo")
returns <- as.vector(na.omit(diff(log(Ad(`HSI`)))))
hsforecasts <- Rolling.Window(data=returns,window.size = 250,func = Historical.Simulation,confidence.level = 0.95)
hsforecasts <- do.call(rbind,hsforecasts)
varforecasts <- hsforecasts$Value_at_Risk
esforecasts <- hsforecasts$Expected_Shortfall
AcerSzeky.Backtesting(returns,varforecasts,esforecasts,n.sim = 10000)
}
