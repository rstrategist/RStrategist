# High Frequency Trend Entry Strategy---------------------------------------
# Strategy Outline - See readme file.
# GBPUSD 1min data import and 1hr strategy

#***********************************************************************
# Set system timezone and initial source directory for loading libraries
#***********************************************************************

Sys.setenv(TZ='GMT')
Sys.timezone()

originalDirectory <-getwd()
source("C:\\R\\RStrategist\\FX_HF_Trend_Entry\\Code\\LoadLibraries.R")
setwd("C:/R/Data/Forex")
LoadLibraries() # mac, pc, linux

#***********************************************************************
# Connect to Interactive Brokers TWS API====
# See TWS_FX_Downloader.R for standalone implementation and pull for all ccys
#***********************************************************************
tws <- twsConnect()
reqCurrentTime(tws)

# Create contracts and request contract details====
pairs <- c("GBPUSD")
define_FX(pairs)
ls_twsInstruments() # all instruments containing IB slot with twsContract object

# Import 1 minute FX Spot Data from file
Filename = paste("Data_",pairs[1],"_minute.csv", sep="")
ColClasses = c("character", "numeric", "numeric", "numeric", "numeric")
tmpPricedata <- read.table(Filename, sep=",", header=TRUE, colClasses=ColClasses)
(tmpTime<-tmpPricedata[,1])

#Create XTS of FX with POSIXct index
tmpPricedata <- xts(as.matrix(tmpPricedata[,-(1)]),as.POSIXct(tmpTime,tz="GMT",format='%d/%m/%Y %H:%M')) 

# Request historical data - 1 minute data for 3 days bind to the XTS
# and also save to file ====
for( i in 1:length(pairs) ) {
  contract <- getContract(pairs[i])
  tmpPricedata_new <- reqHistoricalData(tws,contract,barSize = "1 min",duration = "3 D", whatToShow = "BID") # request historical data
  tmpPricedata_new <- tmpPricedata_new[,-(5:9)] # remove unnecessary columns
  tmpPricedata_new <- rbind(tmpPricedata,tmpPricedata_new)
  write.zoo(tmpPricedata_new, file = paste("Data_",pairs[i],"_minute.csv",sep=""),sep=",")
}

pricedata_1hr <- to.hourly(tmpPricedata_new)
colnames(pricedata_1hr)<-c("Open","High","Low","Close")

# Remove tmp objects
rm(list=ls(pattern="tmp"))

#***********************************************************************
# Parameter Config - Import strategy parameters
#***********************************************************************
(ParameterConfig <- ImportParameterConfig())
SumSignalThreshold <- ParameterConfig$Trading$SumSignalThreshold
ScaleInFactor <- ParameterConfig$Trading$ScaleInFactor

# Add Indicators - Technical indicators
pricedata_1hr <- AddIndicators(pricedata_1hr,ParameterConfig$Indicators)

# Add Signals - Generated trading signals
pricedata_1hr <- AddSignals(pricedata_1hr)

# Commentary - Keeping a track of the strategy behaviour/status
tmpCommentary<-"Start"
tmpTimeSeriesIndex<-0
TradeCommentary <- data.frame(tmpCommentary,tmpTimeSeriesIndex,stringsAsFactors=FALSE)
colnames(TradeCommentary)<-c("TradeCommentary","TimeSeriesIndex")
TradeCommentary
# Remove tmp objects
rm(list=ls(pattern="tmp"))

# Create Black Board
(BlackBoard <- CreateBlackBoard(pricedata_1hr))

#***********************************************************************
# Initialise portfolio, instruments and account
#***********************************************************************
symbols = c("GBPUSD")
currency("GBP")
currency("USD")
exchange_rate("GBPUSD",currency = "USD",tick_size=0.0001)
getInstrument("GBPUSD")

initEq=100000
args(initPortf)
initPortf(name='default', symbols=symbols, initDate=BlackBoard$initDate)
initAcct(name='default', portfolios='default', initDate=BlackBoard$initDate, initEq=initEq)

# Realtime Portfolio
thePortfolioRealtime <- getPortfolio('default')
(thePortfolioRealtime <- CreatethePortfolioRealtime(thePortfolioRealtime))

# Black Board Timeseries-----
(BlackBoardTimeseries <- CreateBlackBoardTimeseries(thePortfolioRealtime))

#******************************************************************
# Trading
#******************************************************************
# Create trades----------------
# R Blotter requires you go through the ts
# Also implemented in quantstrat and bespoke libraries

GBPUSD <- pricedata_1hr[,c("Open","High","Low","Close")] # Only require OHLC

for( i in 1:NROW(GBPUSD) ) {
  #Set Current Date and Get Equity
  (CurrentDate=time(GBPUSD)[i])
  (equity = getEndEq(Account='default', CurrentDate))
  
  for( symbol in symbols ){
    # Prepare Portfolio and Black Board Timeseries
    sym = get(symbol)
    (ClosePrice = as.numeric(Cl(sym[i,])))
    (OpenPrice = as.numeric(Op(sym[i,])))
    (Posn = getPosQty(Portfolio='default', Symbol=symbol, Date=CurrentDate))
    updatePortf(Portfolio='default', Dates=CurrentDate)
    (thePortfolio = getPortfolio('default'))
    # Calculate perTradeStats
    
    # UpdateBlackBoardTimeseries
    (BlackBoardTimeseries = UpdateBlackBoardTimeseries(CurrentDate,OpenPrice,ClosePrice,BlackBoardTimeseries,symbol,i))
    (PositionMultiple = as.numeric(BlackBoardTimeseries$Position.Multiple[i+1]))
    (CumUnrealisedPnL <- cumsum(thePortfolio$symbols[[symbol]]$posPL$Period.Unrealized.PL[1:i]))
    # UpdateBlackBoard
    BlackBoard = UpdateBlackBoard(CurrentDate,ClosePrice,OpenPrice,Posn,BlackBoard,thePortfolio,ParameterConfig)
    (BlackBoard$StopLoss <- as.numeric(UpdateBlackBoardStopLoss(BlackBoard,ParameterConfig)))
    
    # EXIT - STOP LOSS - Check Cumulative unrealised PnL and exit if greater than Trade Stop Loss
    if(BlackBoard$CumUnrealisedPnL < BlackBoard$StopLoss && Posn != 0)
    {
      rm(list=ls(pattern="tmp"))
      tmpList <- KSStopLoss(BlackBoard,BlackBoardTimeseries,TradeCommentary,ParameterConfig,symbol,i)
      if(!is.null(tmpList)){BlackBoard$StopLoss = as.numeric(tmpList[1])}
      if(!is.null(tmpList)){TradeCommentary <- NULL
                            TradeCommentary <- data.frame(tmpList[2])}
      rm(list=ls(pattern="tmp"))
    }
    
    else
    {
      if(Posn == 0)
      {# ENTRY - SIGNAL + NO POSITION + NO RECENT TRADE
      rm(list=ls(pattern="tmp"))
      tmpList <- KSEntry(BlackBoard,BlackBoardTimeseries,TradeCommentary,ParameterConfig,symbol,i)
      if(!is.null(tmpList)){BlackBoard$StopLoss = as.numeric(tmpList[1])}
      if(!is.null(tmpList)){TradeCommentary <- NULL
                            TradeCommentary <- data.frame(tmpList[2])}
      rm(list=ls(pattern="tmp"))
      }
      
    # ENTRY - STOP LOSS NOT HIT + HAVE POSITION  
    if(Posn != 0)
    {
    # Update Trailing StopLoss------
    (BlackBoard$StopLoss = as.numeric(UpdateBlackBoardStopLoss(BlackBoard,ParameterConfig)))
    # CumUnrealisedPnL[i]
    # BlackBoard$CumUnrealisedPnL
    
      # EXIT - STOP LOSS - EXTRA LAYER OF POSITION AND STOP LOSS CHECK
      # Check Cumulative unrealised PnL and exit if less than Trade Stop Loss
      if(BlackBoard$CumUnrealisedPnL < BlackBoard$StopLoss && Posn != 0)
      {rm(list=ls(pattern="tmp"))
       tmpList <- KSStopLoss(BlackBoard,BlackBoardTimeseries,TradeCommentary,ParameterConfig,symbol,i)
       if(!is.null(tmpList)){BlackBoard$StopLoss = tmpList[1]}
       if(!is.null(tmpList)){TradeCommentary <- NULL
                             TradeCommentary <- data.frame(tmpList[2])}
       rm(list=ls(pattern="tmp"))
      }
      
      # ENTRY/EXIT - Option 1 - Max Position not reached, look for entry/exit
      if( abs(Posn) <  abs(BlackBoard$MaxPosition) && PositionMultiple < 3 && Posn != 0)
      {
      # EXIT - LIMIT PROFIT
      rm(list=ls(pattern="tmp"))
      tmpList <- KSExitLimit(BlackBoard,BlackBoardTimeseries,TradeCommentary,ParameterConfig,symbol,i)
      # if(!is.null(tmpList)){BlackBoard$StopLoss = tmpList[1]}
      if(!is.null(tmpList)){TradeCommentary <- NULL
                            TradeCommentary <- data.frame(tmpList[2])}
      rm(list=ls(pattern="tmp"))
      
      # ENTRY - FURTHER ENTRY
      rm(list=ls(pattern="tmp"))
      tmpList <- KSEntryAdd(BlackBoard,BlackBoardTimeseries,TradeCommentary,ParameterConfig,symbol,i)
      # if(!is.null(tmpList)){BlackBoard$StopLoss = tmpList[1]}
      if(!is.null(tmpList)){TradeCommentary <- NULL
                            TradeCommentary <- data.frame(tmpList[2])}
      rm(list=ls(pattern="tmp"))
      } # END ENTRY/EXIt - Option 1 - Max Position not reached

    
      # EXIT - Option 2 - Max Position reached, look for exit
      if( abs(Posn) >=  abs(BlackBoard$MaxPosition) && PositionMultiple >= 3 && Posn != 0)
      {
        # EXIT - LIMIT PROFIT
        rm(list=ls(pattern="tmp"))
        tmpList <- KSExitLimit(BlackBoard,BlackBoardTimeseries,TradeCommentary,ParameterConfig,symbol,i)
        # if(!is.null(tmpList)){BlackBoard$StopLoss = tmpList[1]}
        if(!is.null(tmpList)){TradeCommentary <- NULL
                              TradeCommentary <- data.frame(tmpList[2])}
        rm(list=ls(pattern="tmp"))
      } # END EXIT - Option 2 - Max Position reached
    } # END - POSITION != 0
    } # END - NOT STOPPED - Stop reached is the first check after updating Black Board
   } # END symbols loop
  
  # Update Blotter - Calculate P&L and resulting equity with blotter
  {updatePortf(Portfolio='default', Dates=CurrentDate)
  updateAcct(name='default', Dates=CurrentDate)
  updateEndEq(Account='default', Dates=CurrentDate)
  thePortfolio = getPortfolio('default')
  }
} # End dates loop

#******************************************************************
# Trade Analytics
#******************************************************************
Analytics <- RunAnalytics(BlackBoardTimeseries)

# Plot Chart------------
chartSeries(pricedata_1hr[,1:4],legend="",subset='2012-05-01/2012-05-11',theme="white",TA=NULL,up.col="blue",dn.col="red")
addMACD(fast = ParameterConfig$Indicators$fastMA
        , slow = ParameterConfig$Indicators$slowMA
        , signal = ParameterConfig$Indicators$signalMA
        , type = ParameterConfig$Indicators$maType
        , histogram = T)
addTA(pricedata_1hr$EMA.18,legend="EMA 18",on=1,col=1,lwd=1)
addTA(pricedata_1hr$EMA.36,legend="EMA 36",on=1,col=2,lwd=2)
addTA(pricedata_1hr$EMA.102,legend="EMA 102",on=1,col=3,lwd=3)
addBBands()
addTA(pricedata_1hr$Stoch.fastK,type="l",col=8)
addTA(pricedata_1hr$Stoch.slowD,type="l",col=5,on=3)
addTA(pricedata_1hr$RSI,type="l",col="purple",on=3)
addTA(pricedata_1hr$NetSumSignal,type="h",col="red")
addLines(,4,col="red",on=4)
addLines(,5,col="red",on=4)

if(!is.null(nrow(Analytics$Buys)) && nrow(Analytics$Buys) >=1 ) (addTA(Analytics$Buys,pch=2,type='p',col='blue', on=1));
if(!is.null(nrow(Analytics$Sells)) && nrow(Analytics$Sells) >= 1) (addTA(Analytics$Sells,pch=6,type='p',col='red', on=1));

(addTA(Analytics$Positionfill,type='l',col='blue', lwd=2))
(addTA(Analytics$Position,type='p',col='blue', lwd=2,on=5))

if(!is.null(Analytics$CumPL)) (addTA(Analytics$CumPL, col='darkgreen', lwd=2))

plot(xyplot(thePortfolio$summary,xlab="",type="h",col=4))

# Plot Cumulative Return and Drawdown
rets <- PortfReturns(Account='default')
tail(rets,12)
charts.PerformanceSummary(rets,colorset = bluefocus)

# Chart Maximum Adverse Excursion Charts
chart.ME('default', symbol, type='MAE', scale='percent')
chart.ME('default', symbol, type='MAE', scale='cash')

chart.ME('default', symbol, type='MFE', scale='percent')
chart.ME('default', symbol, type='MFE', scale='cash')

# Obtain tradeStats sourcecode
tradeStatsRR(Portfolios='default', Symbols=symbol)
dailyStats(Portfolios='default', use = c("Equity", "Txns"))

# The Account-------
{theAccount = getAccount('default')
names(theAccount)
names(theAccount$portfolios)
names(theAccount$portfolios$default)
names(theAccount$summary)
}

ExportFiles(pricedata_1hr)