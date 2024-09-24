#' Fit Parameters
#'
#' For the input data, it finds the parameters for "Standard Normal", "Standard-t", "Skewed-Normal" and "Skewed-t" distributions
#' @param data Daily Log-Return Series
#' @param dist "normal","std","skewn","skewt"
#'
#' @return Parameters of the fitted distributions based on the choice
#' @export
#'
#' @examples Fit.Parameters(rnorm(100,1,2),dist="normal")
#'
Fit.Parameters <- function(data,dist=c("normal","std","skewn","skewt")){

  # Check if the "rugarch" package is installed, if not, install it
  if (!requireNamespace("rugarch", quietly = TRUE)) {
    install.packages("rugarch")
  }
  # Load the package
  library("rugarch", character.only = TRUE)

  parameters <- list()

  if(dist=="normal"){

    parameters <- fitdist(data,distribution="norm")$pars
    #parameters[1] <- mu , parameters[2] <- sigma
  }

  if(dist=="std"){

    parameters <- fitdist(data,distribution="std")$pars
    #parameters[1] <- mu , parameters[2] <- sigma, parameters[3] <- shape
  }

  if(dist=="skewn"){

    parameters <- fitdist(data,distribution="snorm")$pars
    #parameters[1] <- mu , parameters[2] <- sigma, parameters[3] <- skew
  }

  if(dist=="skewt"){

    parameters <- fitdist(data,distribution="sstd")$pars
    #parameters[1] <- mu , parameters[2] <- sigma, parameters[3] <- skew, parameters[4] <- shape
  }

  return(parameters)
}
