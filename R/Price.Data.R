#' Daily Price Data Loading
#'
#' @param start First date of the data
#' @param end Last date of the data
#' @param tickers Asset Tickers
#' @param type Open,High,Low,Close,Volume,Adjusted
#'
#' @return Daily Prices
#' @export
#'
#' @examples Price.Data(start="2023-01-01",end="2024-01-01",tickers="AAPL",type="Close")
#'
Price.Data <- function(start="2023-01-01",end="2024-01-01",tickers = c("AAPL","META"),
                       type=c("Open","High","Low","Close","Volume","Adjusted")){

  n <- length(tickers)
  p <- getSymbols(Symbols = tickers[1], src = "yahoo",
                  from = start, to = end,
                  auto.assign = F)
  while(n > 1){

    for (i in 2:n){

      p <- merge(p, getSymbols(Symbols = tickers[i], src = "yahoo",
                               from = start, to = end,
                               auto.assign=F))
    }

    break
  }

  p <- p[,grep(type,names(p),value = TRUE)]
  names(p) <- gsub("\\..*","",names(p))

  return(p)

}
