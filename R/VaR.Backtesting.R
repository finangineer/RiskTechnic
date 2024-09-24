#' Value-at-Risk Backtesting Conditional Coverage Test, Independence Test and Kupiec Test
#'
#' It performs Kupiec's Exceedances Test, Independence Test and Conditional Coverage (Joint)
#' Test for Value-at-Risk Backtesting
#' @param excepts Exceptions data calculated by Exceptions Function
#' @param confidence.level Confidence Level for Back-Testing,e.g. 95%
#'
#' @return Test Statistics, Critical Values, p-values, Zero Hypothesis and the Test Decisions
#' @export
#'
#' @examples VaR.Backtesting(exceptions.data,confidence.level=0.95)
#'
VaR.Backtesting <- function(excepts,confidence.level=0.95){

  n1 <- sum(excepts!=0)
  n <- length(excepts)
  n0 <-  n - n1

  #Kupiec Exceedances Test
  pi.observed <- n1/n
  pi.expected <- (1-confidence.level)

  LRKupiec.stat <- -2 * log(pi.expected^n1 * (1-pi.expected)^n0) +
    2 * log(pi.observed^n1 * (1-pi.observed)^n0)

  #Independence Test
  n00 <- sum(excepts[1:(n-1)] == 0 & excepts[2:n] == 0)
  n01 <- sum(excepts[1:(n-1)] == 0 & excepts[2:n] != 0)
  n10 <- sum(excepts[1:(n-1)] != 0 & excepts[2:n] == 0)
  n11 <- sum(excepts[1:(n-1)] != 0 & excepts[2:n] != 0)

  pi01 <- n01/(n00+n01)
  pi11 <- n11/(n10+n11)
  pi <- (n01 + n11) / (n00+n01 + n10 +n11)
  LRIndepe.stat <-  -2 * log(pi^(n01+n11) * (1-pi)^(n00+n10)) +
    2 * log(pi01^n01 * (1-pi01)^n00 * pi11^n11 * (1-pi11)^n10)

  #Conditional Coverage Test
  LRCondit.stat <- LRKupiec.stat + LRIndepe.stat


  LRKupiec.critical.value <- qchisq(confidence.level,df=1)
  LRIndepe.critical.value <- qchisq(confidence.level,df=1)
  LRCondit.critical.value <- qchisq(confidence.level,df=2)

  LRKupiec.p.value <- 1 - pchisq(q = LRKupiec.stat, df = 1)
  LRIndepe.p.value <- 1 - pchisq(q = LRIndepe.stat, df = 1)
  LRCondit.p.value <- 1 - pchisq(q = LRCondit.stat, df = 2)

  result.kupiec <- matrix(c(LRKupiec.stat,LRKupiec.critical.value,LRKupiec.p.value),
                          nrow = 3, byrow = TRUE)
  result.indepe <- matrix(c(LRIndepe.stat,LRIndepe.critical.value,LRIndepe.p.value),
                          nrow = 3, byrow = TRUE)
  result.condit <- matrix(c(LRCondit.stat,LRCondit.critical.value,LRCondit.p.value),
                          nrow = 3, byrow = TRUE)

  result <- round(cbind(result.kupiec,result.indepe,result.condit),4)

  hypothesis <- c("Correct","Independent","Correct & Independent Exceedances")

  kupiec.decision <- ifelse(LRKupiec.p.value > (1-confidence.level),"Fail to Reject","Reject H0")
  indepe.decision <- ifelse(LRIndepe.p.value > (1-confidence.level),"Fail to Reject","Reject H0")
  condit.decision <- ifelse(LRCondit.p.value > (1-confidence.level),"Fail to Reject","Reject H0")

  result <- rbind(result,hypothesis,c(kupiec.decision,indepe.decision,condit.decision))

  colnames(result) <- c("Kupiec","Independence","Conditional Coverage")
  rownames(result) <- c("Test Statistics","Critical Value","p-value","Zero Hypothesis","Decision")
  return(result)

}
