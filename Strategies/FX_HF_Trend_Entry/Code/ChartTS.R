ChartTS <- function(ts,ParameterConfig)
{
  ParameterConfig<-ParameterConfig
  
  # Indicators-Parameters--------------------------------------------------------------------
  #MACD
  fastMA <- ParameterConfig$fastMA
  slowMA <- ParameterConfig$slowMA
  signalMA <- ParameterConfig$signalMA
  maType<-ParameterConfig$maType
  #EMA
  fastEMA <- ParameterConfig$fastEMA
  slowEMA <- ParameterConfig$slowEMA
  superslowEMA <- ParameterConfig$superslowEMA
  #Stoch
  nFastK <- ParameterConfig$nFastK #no. past periods to use
  nFastD <- ParameterConfig$nFastD #no. smoothing periods to apply to Fast%K
  nSlowD <- ParameterConfig$nSlowD #no. smoothing periods to apply to Fast%D
  smooth <- ParameterConfig$smooth #no. internal smoothing periods to Fast%K
  #RSI
  RSIperiod <- ParameterConfig$RSIperiod
  #TRIX
  TRIXPeriod <- ParameterConfig$TRIXPeriod
  TRIXSigPeriod <- ParameterConfig$TRIXSigPeriod
  
  # Technical Indicators & Charting----------------------------------------------------
  
  #Plot Hourly
  chartSeries(ts[,1:4],legend="",subset='2012-01/2012-01',theme="white",TA=NULL,up.col="blue",dn.col="red")
  ts$EMA.18<-ifelse(is.na(ts$EMA.18),0,ts$EMA.18)
  head(ts$EMA.18,30)
  addTA(ts$EMA.18,legend="EMA 18",on=1,col=1,lwd=1)
  addTA(ts$EMA.36,legend="EMA 36",on=1,col=2,lwd=2)
  addTA(ts$EMA.102,legend="EMA 102",on=1,col=3,lwd=3)
  MACDHist <- ts$MACD - ts$MACD.sig
  addTA(MACDHist,type='h')
  addBBands()
  
  #Stoch
  addTA(ts$Stoch.fastK,type="l",col=8)
  addTA(ts$Stoch.slowD,type="l",col=5,on=4)
  addTA(ts$RSI,type="l",col="purple",on=4)
  addTA(ts$SumSignal,type="h",col="purple")
  addLines(,4,col="purple",on=5)
  addTA(ts$NetSumSignal,type="h",col="red")
  addLines(,4,col="red",on=6)
  
  }