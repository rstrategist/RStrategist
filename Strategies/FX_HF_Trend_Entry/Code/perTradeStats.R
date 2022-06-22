b <- perTradeStats(Portfolio='default', Symbol='GBPUSD')

perTradeStats <- function(Portfolio, Symbol, includeOpenTrades=FALSE, ...) {
  portf <- getPortfolio(Portfolio)
  
  if(missing(Symbol)) Symbol <- names(portf$symbols)[[1]]
  
  posPL <- portf$symbols[[Symbol]]$posPL
  
  instr <- getInstrument(Symbol)
  tick_value <- instr$multiplier*instr$tick_size
  
  trades <- list()
  
  # identify start and end for each trade, where end means flat position
  trades$Start <- index(posPL[which(posPL$Pos.Value!=0 & lag(posPL$Pos.Value)==0),])
  trades$End <- index(posPL[which(posPL$Pos.Value==0 & lag(posPL$Pos.Value)!=0),])
  
  # if the last trade is still open, adjust depending on whether wants open trades or not
  if(length(trades$Start)>length(trades$End))
  {
    if(includeOpenTrades)
      trades$End <- c(trades$End,last(index(posPL)))
    else
      trades$Start <- head(trades$Start, -1)
  }
  
  # calculate information about each trade
  for(i in 1:length(trades$End))
  {
    timespan <- paste(format(trades$Start[[i]], "%Y-%m-%d %H:%M:%OS6"),
                      format(trades$End[[i]], "%Y-%m-%d %H:%M:%OS6"), sep="::")
    
    trade <- posPL[timespan]        
    
    # close and open may occur in at same index timestamp, must be corrected
    if(first(trade)$Pos.Qty==0) trade <- tail(trade, -1)
    if(last(trade)$Pos.Qty!=0) trade <- head(trade, -1)
    
    # add cost basis column
    trade$Pos.Cost.Basis <- cumsum(trade$Txn.Value)
    #add running posPL column
    trade$PosPL <- trade$Pos.Value-trade$Pos.Cost.Basis
    
    #count number of transactions
    trades$Num.Txns[i]<-length(which(trade$Txn.Value!=0))
    
    #position sizes
    trades$Init.Pos[i] <- first(trade$Pos.Qty)
    trades$Max.Pos[i] <- first(trade[which(abs(trade$Pos.Qty)==max(abs(trade$Pos.Qty))),]$Pos.Qty)
    # investment
    trades$Max.Notional.Cost[i] <- first(trade[which(abs(trade$Pos.Qty)==max(abs(trade$Pos.Qty))),]$Pos.Cost.Basis)
    
    # percentage P&L
    trade$Pct.PL <- trade$PosPL/abs(trade$Pos.Cost.Basis) #broken for last timestamp
    trade$Pct.PL[length(trade$Pct.PL)]<-last(trade)$PosPL/abs(trades$Max.Notional.Cost[i])
    
    trades$Pct.Net.Trading.PL[i] <- last(trade$Pct.PL)
    trades$Pct.MAE[i] <- min(0,trade$Pct.PL)
    trades$Pct.MFE[i] <- max(0,trade$Pct.PL)
    
    # cash P&L
    trades$Net.Trading.PL[i] <- last(trade)$PosPL
    trades$MAE[i] <- min(0,trade$PosPL)
    trades$MFE[i] <- max(0,trade$PosPL)
    
    # tick P&L
    #Net.Trading.PL/position/tick value=ticks
    trade$tick.PL <- trade$PosPL/abs(trade$Pos.Qty)/tick_value #broken for last observation
    trade$tick.PL[length(trade$tick.PL)] <- last(trade$PosPL)/abs(trades$Max.Pos[i])/tick_value
    
    trades$tick.Net.Trading.PL[i] <- last(trade$tick.PL)
    trades$tick.MAE[i] <- min(0,trade$tick.PL)
    trades$tick.MFE[i] <- max(0,trade$tick.PL)
  }
  #print(trades)
  
  return(as.data.frame(trades))
}