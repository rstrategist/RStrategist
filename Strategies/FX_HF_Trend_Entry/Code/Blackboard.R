#BlackBoard

CreateBlackBoard <- function(ts)
{
# Create BlackBoard----------------------------
# BlackBoard
BlackBoard <- list(
  PeriodUnrealisedPL = 0
  ,CumUnrealisedPnL = 0
  ,TradeMaxCumPL = 0
  ,Position= 0
  ,Equity=initEq
  ,MaxPosition = 0
  ,StopLoss = -5000
  ,LimitProfit = 55000
  ,RecentTrade = FALSE
  ,Status = "Started - Looking for Entry"
  #Initialise Dates
  ,start.data = start(ts)
  ,end.data = end(ts)
  ,initDate = start(ts)-86400 #Seconds in a day
  ,CurrentDate = start(ts)
  )
  return(BlackBoard)
}

# BlackBoard Timeseries----

CreateBlackBoardTimeseries <- function(thePortfolioRealtime)
{
  thePortfolioRealtime <- thePortfolioRealtime
  BlackBoardTimeseries <- thePortfolioRealtime$summary[,c("Close","Pip.Move","Position.Multiple","CumUnrealisedPnL")]
  return(BlackBoardTimeseries)
}

# Update BlackBoard Time Series
UpdateBlackBoardTimeseries <- function(CurrentDate,OpenPrice,ClosePrice,BlackBoardTimeseries,symbol,i)
{
  #Create XTS row with trade details
  tmp <- xts(as.matrix(matrix(c(0,0,0,0),nrow=1)),as.POSIXct(CurrentDate),tz="GMT",format='%Y-%m-%d %H:%M')
  colnames(tmp) <- c("Close","Pip.Move","Position.Multiple","CumUnrealisedPnL")
  #Append XTS row to BlackBoard Timeseries
  (BlackBoardTimeseries <- c(BlackBoardTimeseries, tmp))
  
  #Update Bloackboard Timeseries------
  BlackBoardTimeseries$Close[i+1] <- ClosePrice
  BlackBoardTimeseries$Pip.Move[i+1] <- as.numeric(ClosePrice - OpenPrice)*100    
  BlackBoardTimeseries$Position.Multiple[i+1] <- as.numeric(abs(round((getPortfolio('default')$summary$Gross.Value[i]/3330000),0)))
  #(PositionMultiple <- as.numeric(BlackBoardTimeseries$Position.Multiple[i+1]))
  BlackBoardTimeseries$CumUnrealisedPnL[i+1] <- as.numeric(tail(cumsum(thePortfolio$symbols[[symbol]]$posPL$Period.Unrealized.PL[1:i]),1))
  
  return(BlackBoardTimeseries)
}
  
UpdateBlackBoard <- function(CurrentDate,ClosePrice,OpenPrice,Posn,BlackBoard,thePortfolio,ParameterConfig)
{  
    # Update BlackBoard----INCLUDE ALL TRADE AND OPTIONS RELATED INFORMATION ON THE BLACKBOARD
    # HAVE A BLACKBOARD STATUS which is printed every hour...e.g.
    # Position = 350,000
    # Positon Status >= Max posiiton
    #, P&L = $3,547
    #, Stop Status = Not Hit
    #, Stop Loss = -$10,000
    #, Stop Level = 99.56
    #, Limit Profit = $75,000
    #, Current Signal = Enter short
    #,Status = Looking for exit
    
    #Update BlackBoard
    BlackBoard$CurrentDate = CurrentDate
    EquityRisked <- ParameterConfig$Trading$EquityRisked
    TickSize <- ParameterConfig$Trading$TickSize
    LimitMove <- ParameterConfig$Trading$LimitMove
    #Update Unrealised PL and Equity
    #Clean $posPL table
    thePortfolio$symbols[[symbol]]$posPL <- thePortfolio$symbols[[symbol]]$posPL[!duplicated(index(thePortfolio$symbols$GBPUSD$posPL),fromLast=TRUE),]
    (tmpErrorRows <- which(is.na(thePortfolio$symbols[[symbol]]$posPL[,'Pos.Value'])))
    is.na(thePortfolio$symbols[[symbol]]$posPL[i,'Period.Unrealized.PL'])
    if(is.na(thePortfolio$symbols[[symbol]]$posPL[i,'Period.Unrealized.PL']))
    {thePortfolio$symbols[[symbol]]$posPL$Period.Unrealized.PL[i] <- 0}else{BlackBoard$PeriodUnrealisedPL = 0}
    
    (BlackBoard$PeriodUnrealisedPL = as.numeric(thePortfolio$symbols[[symbol]]$posPL$Period.Unrealized.PL[i]))
    (BlackBoard$CumUnrealisedPnL <- as.numeric(tail(cumsum(thePortfolio$symbols[[symbol]]$posPL$Period.Unrealized.PL[1:i]),1)))
    #(BlackBoard$CumUnrealisedPnL <- as.numeric(tail(cumsum(thePortfolio$summary$Unrealized.PL[1:i]),1)))
    (BlackBoard$Position = Posn)
    equity = getEndEq(Account='default', BlackBoard$CurrentDate)
    #print(equity)
    (BlackBoard$Equity = equity)
    #Update Max Position
    (BlackBoard$MaxPosition = (EquityRisked*equity*(1/TickSize)*10)/LimitMove)
    #print(BlackBoard$MaxPosition)
    #Update Recent Trade Activity
    #BlackBoard$RecentTrade = TRUE
    (BlackBoard$RecentTrade = as.vector((pricedata_1hr[(max(1,i-3)),"NetSumSignal"]>4)))
    (BlackBoard$RecentTrade = ifelse(BlackBoard$RecentTrade,BlackBoard$RecentTrade,as.vector((pricedata_1hr[(max(1,i-2)),"NetSumSignal"]>4))))
    (BlackBoard$RecentTrade = ifelse(BlackBoard$RecentTrade,BlackBoard$RecentTrade,as.vector((pricedata_1hr[(max(1,i-1)),"NetSumSignal"]>4))))
    
    #Update BB Status
    #BlackBoard$Status <- "Started - Looking for Entry"
    #print(BlackBoard)
    #print(names(BlackBoard))
  
    # Position Entry (assume fill at close)
    #(RecentTrade <- cumsum(thePortfolio$summary$Position.Multiple[max(1,i-2):i]))
  
    #(BlackBoard$RecentTrade <- as.integer(thePortfolio$summary$Position.Multiple[max(1,i-2)]))
    #(BlackBoard$RecentTrade <- BlackBoard$RecentTrade + as.numeric(thePortfolio$summary$Position.Multiple[max(1,i-1)]))
    #(BlackBoard$RecentTrade <- RecentTrade + as.numeric(thePortfolio$summary$Position.Multiple[max(1,i)]))
  
  
    #thePortfolioRealtime <- thePortfolioRealtime
    #BlackBoardTimeseries <- thePortfolioRealtime$summary[,c("Close","Pip.Move","Position.Multiple","Trade.Commentary")]
    return(BlackBoard)
}

  
UpdateBlackBoardStopLoss <- function(BlackBoard,ParameterConfig)
{ 
  StopLossReset <- ParameterConfig$Trading$StopLossReset
  if(BlackBoard$Position == 0) {BlackBoard$StopLoss = StopLossReset}
  
  if(BlackBoard$Position != 0)
    {
    TrailingStopLoss <- PnLBasedStopLoss(BlackBoard$CumUnrealisedPnL,BlackBoard$StopLoss)
    # Does not allow Trailing Stop Loss to adjust downwards but only rachet upwards
    BlackBoard$StopLoss = max(BlackBoard$StopLoss,TrailingStopLoss)
    }
  
  return(as.integer(BlackBoard$StopLoss))
}

PnLBasedStopLoss <- function(CumUnrealisedPnL,StopLoss)
{
  #if(CumUnrealisedPnL > 5000) {StopLoss = 0}
  if(CumUnrealisedPnL > 10000) {StopLoss = 2000}    
  if(CumUnrealisedPnL > 15000) {StopLoss = 5000}
  if(CumUnrealisedPnL > 20000) {StopLoss = 15000}
  if(CumUnrealisedPnL > 30000) {StopLoss = 25000}
  if(CumUnrealisedPnL > 40000) {StopLoss = 35000}
  if(CumUnrealisedPnL > 50000) {StopLoss = 45000}
  if(CumUnrealisedPnL > 60000) {StopLoss = 55000}
  if(CumUnrealisedPnL > 70000) {StopLoss = as.numeric(CumUnrealisedPnL-1)}
  if(CumUnrealisedPnL > 80000) {StopLoss = as.numeric(CumUnrealisedPnL-1)}
  #print(paste("StopLoss = ", StopLoss,sep=""))
  return(as.integer(StopLoss))
}


KSStopLoss <- function(BlackBoard,BlackBoardTimeseries,TradeCommentary,ParameterConfig,symbol,i)
{
  ClosePrice <- BlackBoardTimeseries$Close[i+1]
  #BlackBoardTimeseries$Position.Multiple[i+1]
  ScaleInFactor <- ParameterConfig$Trading$ScaleInFactor
  StopLossReset <- ParameterConfig$Trading$StopLossReset
  
  if(BlackBoard$CumUnrealisedPnL < BlackBoard$StopLoss && BlackBoard$Position != 0)
  {
    UnitSize = -BlackBoard$Position
    # Store trade with blotter
    addTxn('default', Symbol=symbol, TxnDate=BlackBoard$CurrentDate,
           TxnPrice=ClosePrice, TxnQty=UnitSize, TxnFees=0, verbose=verbose)
    tmpTradeCommentary <- c("Exit Trade - Stop Loss",i)
    TradeCommentary <- rbind(TradeCommentary,tmpTradeCommentary)
    print(tail(TradeCommentary,1))
    tmpList <- list(StopLossReset,TradeCommentary)
    return(tmpList)
  }
}


KSEntry <- function(BlackBoard,BlackBoardTimeseries,TradeCommentary,ParameterConfig,symbol,i)
{
  ClosePrice <- BlackBoardTimeseries$Close[i+1]
  #BlackBoardTimeseries$Position.Multiple[i+1]
  ScaleInFactor <- ParameterConfig$Trading$ScaleInFactor
  StopLossReset <- ParameterConfig$Trading$StopLossReset
  
  if( BlackBoard$Position == 0 && BlackBoard$RecentTrade == FALSE)
  {
    #No position, so test to initiate Short position  
    if( pricedata_1hr[i,"NetSumSignal"] > SumSignalThreshold )
      {
      UnitSize = as.numeric(trunc((BlackBoard$MaxPosition*ScaleInFactor)))
      # Store trade with blotter
      addTxn('default', Symbol=symbol, TxnDate=BlackBoard$CurrentDate,
           TxnPrice=ClosePrice, TxnQty=-UnitSize, TxnFees=0, verbose=verbose)
      (tmpTradeCommentary <- list("New Short Trade",i))
      TradeCommentary <- rbind(TradeCommentary,tmpTradeCommentary)
      print(tail(TradeCommentary,1))
      tmpList <- list(StopLossReset,TradeCommentary)
      return(tmpList)
      }
  }
}


KSEntryAdd <- function(BlackBoard,BlackBoardTimeseries,TradeCommentary,ParameterConfig,symbol,i)
{#Entry - Add to position if signal triggered   
  
  ClosePrice <- BlackBoardTimeseries$Close[i+1]
  #BlackBoardTimeseries$Position.Multiple[i+1]
  ScaleInFactor <- ParameterConfig$Trading$ScaleInFactor
  StopLossReset <- ParameterConfig$Trading$StopLossReset
  
  if( pricedata_1hr[i,"NetSumSignal"] > ParameterConfig$Trading$SumSignalThreshold )
    {
      #Size to enter
      (UnitSize = as.numeric(trunc((BlackBoard$MaxPosition*ScaleInFactor))))
      #Store trade with blotter
      addTxn('default', Symbol=symbol, TxnDate=BlackBoard$CurrentDate,
             TxnPrice=ClosePrice, TxnQty=-UnitSize, TxnFees=0, verbose=verbose)
      tmpTradeCommentary <- list("New Transaction - Increase Exposure",i)
      TradeCommentary <- rbind(TradeCommentary,tmpTradeCommentary)
      print(tail(TradeCommentary,1))
      #BlackBoard$Status<-TradeCommentary
      #BlackBoard$StopLoss = StopLossReset #Reset StopLoss
      tmpList <- list(BlackBoard$StopLoss,TradeCommentary)
      return(tmpList)
    }
}


KSExitLimit <- function(BlackBoard,BlackBoardTimeseries,TradeCommentary,ParameterConfig,symbol,i)
{
  ClosePrice <- BlackBoardTimeseries$Close[i+1]
  #BlackBoardTimeseries$Position.Multiple[i+1]
  ScaleInFactor <- ParameterConfig$Trading$ScaleInFactor
  ScaleOutFactor <- ParameterConfig$Trading$ScaleOutFactor
  StopLossReset <- ParameterConfig$Trading$StopLossReset
  
  #EXIT - LIMIT PROFIT
  if(BlackBoard$CumUnrealisedPnL >= BlackBoard$LimitProfit && BlackBoard$Position != 0)
  {
    
    #If position is less than ScaleOutFactor then exit the whole position
    if(abs(BlackBoard$Position) < (BlackBoard$MaxPosition*ScaleOutFactor))
    {
      UnitSize = -BlackBoard$Position
      # Store trade with blotter
      addTxn('default', Symbol=symbol, TxnDate=BlackBoard$CurrentDate,
             TxnPrice=ClosePrice, TxnQty=UnitSize, TxnFees=0, verbose=verbose)
      tmpTradeCommentary <- list("Exit Trade - Limit Profit",i)
      TradeCommentary <- rbind(TradeCommentary,tmpTradeCommentary)
      print(tail(TradeCommentary,1))
      #BlackBoard$Status<-TradeCommentary
      BlackBoard$StopLoss = StopLossReset #Reset StopLoss
      tmpList <- list(BlackBoard$StopLoss,TradeCommentary)
      return(tmpList)
    }
      
    #If position is greater than ScaleOutFactor then exit according to scale out factor
    if(abs(BlackBoard$Position) > (BlackBoard$MaxPosition*ScaleOutFactor))
    {
         UnitSize = (BlackBoard$MaxPosition*ScaleOutFactor)
         # Store trade with blotter
         addTxn('default', Symbol=symbol, TxnDate=BlackBoard$CurrentDate,
                TxnPrice=ClosePrice, TxnQty=UnitSize, TxnFees=0, verbose=verbose)
         tmpTradeCommentary <- list("Exit Transaction - Limit Profit - Scale Out",i)
         TradeCommentary <- rbind(TradeCommentary,tmpTradeCommentary)
         print(tail(TradeCommentary,1))
         #BlackBoard$Status<-TradeCommentary
         #BlackBoard$StopLoss = StopLossReset #Reset StopLoss
         tmpList <- list(BlackBoard$StopLoss,TradeCommentary)
         return(tmpList)
    }
  } #End Limit Profit when position < Max Position
}
