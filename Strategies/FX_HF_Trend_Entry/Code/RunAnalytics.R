#RunAnalytics
#Analytics--------------------------------------

RunAnalytics <- function(BlackBoardTimeseries)
{
#Performance Analytics
thePortfolio = getPortfolio('default')
thePortfolio$summary <- cbind(thePortfolio$summary,BlackBoardTimeseries)
tail(thePortfolio$summary,5)

Transactions <- getTxns(Portfolio="default", Symbol=symbol)
Transactions <- Transactions[-1]
Transactions$Net.Txn.Realized.PL.USD <- 0
ifelse(Transactions$Txn.Price == 0,Transactions$Net.Txn.Realized.PL.USD <- 0,(Transactions$Net.Txn.Realized.PL.USD <- Transactions$Net.Txn.Realized.PL / Transactions$Txn.Price))

#Clean up posPL xts
#Find NA rows
tmpErrorRows <- which(is.na(thePortfolio$symbols$GBPUSD$posPL[,'Pos.Value']))
thePortfolio$symbols$GBPUSD$posPL <- thePortfolio$symbols$GBPUSD$posPL[-1,]
#Removes duplicated rows
thePortfolio$symbols$GBPUSD$posPL <- thePortfolio$symbols$GBPUSD$posPL[!duplicated(index(thePortfolio$symbols$GBPUSD$posPL),fromLast=TRUE),]

# Build Analytics
Analytics <- list(Trades=0
                  ,Buys=0
                  ,Sells=0
                  ,Position=0
                  ,Positionfill=0
                  ,CumPL=0
                  )

# Build Trades, PositionFill and Equity
{  
  Analytics$Trades <- thePortfolio$symbols[[symbol]]$txn$Txn.Qty
  Analytics$Buys = thePortfolio$symbols[[symbol]]$txn$Txn.Price[which(Analytics$Trades>0)]
  Analytics$Sells = thePortfolio$symbols[[symbol]]$txn$Txn.Price[which(Analytics$Trades<0)]
  
  Analytics$Position = thePortfolio$symbols[[symbol]]$txn$Pos.Qty
  Analytics$Positionfill = na.locf(merge(Analytics$Position,index(GBPUSD)))
}

CumPL = cumsum(thePortfolio$symbols[[symbol]]$posPL$Net.Trading.PL[-1])
if(length(CumPL)>1){
  Analytics$CumPL = na.locf(merge(CumPL,index(GBPUSD)))
}else{Analytics$CumPL = NULL}

return(Analytics)
}