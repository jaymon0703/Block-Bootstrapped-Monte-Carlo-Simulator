#' Monte Carlo simulate strategy results
#'
#' Return bands of returns based on Monte Carlo simulations of back-test results
#' @param Portfolio string identifier of portfolio name
#' @param R number of simulations, default = 1000
#' @param l block length, default = 20
#' @param use determines whether to use daily or txn PL, default = "equity"
#' @return a plot.xts object of simulated return streams
#' @note 
#' Requires boot package
#' @importFrom boot tsboot
#' @export
#' @author Jasen Mackie, Brian G. Peterson
#' @seealso \code{\link{boot}}

####################################### Get Daily EqPL function
dailyEqPL <- function(Portfolio, Account, Symbols, drop.time=TRUE)
{
  ret <- NULL
  #for (Portfolio in Portfolios){
  pname <- Portfolio
  Portfolio <- getPortfolio(pname)        
  
  ## FIXME: need a way to define symbols for each portfolio    
  if(missing(Symbols)) symbols <- ls(Portfolio$symbols)
  else symbols <- Symbols
  
  ## Trade Statistics
  for (symbol in symbols){
    posPL <- Portfolio$symbols[[symbol]]$posPL
    posPL <- posPL[-1,] # remove initialization row
    
    Equity <- cumsum(posPL$Net.Trading.PL)
    if(!nrow(Equity)){
      warning('No P&L rows for',symbol)
      next()
    }             
    
    #DailyPL <- apply.daily(Equity,last)
    DailyPL           <- apply.daily(posPL$Net.Trading.PL,colSums)
    colnames(DailyPL) <- paste(symbol,'DailyEndEq',sep='.')
    if(is.null(ret)) ret=DailyPL else ret<-cbind(ret,DailyPL)
    
  } # end symbol loop
  #} # end portfolio loop
  ret <- apply.daily(ret,colSums,na.rm=TRUE)  
  if(drop.time) index(ret) <- as.Date(index(ret))
  return(ret)
}


####################################### Get Daily TXN PL function
dailyTxnPL <- function(Portfolios, Symbols, drop.time=TRUE)
{
  ret <- NULL
  #for (Portfolio in Portfolios){
  pname <- Portfolio
  Portfolio <- getPortfolio(pname)        
  
  
  ## FIXME: need a way to define symbols for each portfolio    
  
  PL.ne0 <- txn$Net.Txn.Realized.PL[txn$Net.Txn.Realized.PL != 0]
  if(!nrow(PL.ne0)){
    warning('No P&L rows for',symbol)
    next()
  }             
  DailyPL           <- apply.daily(PL.ne0,sum)
  colnames(DailyPL) <- paste(symbol,'DailyTxnPL',sep='.')
  if(is.null(ret)) ret=DailyPL else ret<-cbind(ret,DailyPL)
  
  #} # end symbol loop
  #} # end portfolio loop
  ret <- apply.daily(ret,colSums,na.rm=TRUE)  
  if(drop.time) index(ret) <- as.Date(index(ret))
  return(ret)
}

####################################### mcsim function
mcsim <- function(Portfolio, Account, n = 1000, l = 20, use=c('equity','txns')){
  
  use=use[1] #take the first value if the user didn't specify
  switch (use,
          Eq =, eq =, Equity =, equity =, cumPL = {
            dailyPL <- dailyEqPL(Portfolio)
          },
          Txns =, txns =, Trades =, trades = {
            dailyPL <- dailyTxnPL(Portfolio)
          }
  )

  p <- getPortfolio(Portfolio)
  a <- getAccount(Account)
  initEq <- attributes(a)$initEq
  t1 <- Sys.time()
  n <- 1000
  l <- 20
  use=c('equity','txns')
  tsb <- tsboot(ROC(initEq + cumsum(dailyPL)), maxDrawdown, n, l, sim = "fixed")
  inds <- t(boot.array(tsb))
  k <- NULL
  tsbootARR <- NULL
  tsbootxts <- NULL
  tmp <- NULL
  EndEqdf <- data.frame(dailyPL)
  EndEqdf[is.na(EndEqdf)] <- 0
  for(k in 1:ncol(inds)){
    tmp <- cbind(tmp, EndEqdf[inds[,k],])
}
    tsbootARR <- apply(tmp, 2, function(x) cumsum(x))
    which(is.na(tsbootARR))
    tsbootxts <- xts(tsbootARR, index(dailyPL))
    plot.zoo(tsbootxts, plot.type = "single", col = "lightgray")
    lines(index(dailyPL), cumsum(dailyPL), col = "red")
    xname <- paste(n, "replicates with block length", l)
    hist(tsb$t, main = paste("Drawdown distribution of" , xname), breaks=50) # need to determine optimal no. of breaks, perhaps based on size of range?
    abline(v=maxDrawdown(ROC(initEq + cumsum(dailyPL))), col = "red")
    text(maxDrawdown(ROC(initEq + cumsum(dailyPL))), -1, "backtest", col = "red")
    abline(v=median(tsb$t), col = "blue")
    text(median(tsb$t), -1, "sample median", col = "blue")
    t2 <- Sys.time()
    difftime(t2,t1)
}