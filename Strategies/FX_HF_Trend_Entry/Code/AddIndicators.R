#Add Indicators and Signals

AddIndicators <- function(XTS,ParameterConfig)
{
  
#Indicators-Parameters====
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

#Add Indicators to Timeseries-----------------

#XTS
tmpIndicator <- NULL
head(tmpIndicator)
tail(tmpIndicator)
tmpIndicator[1,1:6]
ncol(tmpIndicator)
head(tmpIndicator,51)
tmpIndicator[49:51,6]

tmpIndicator<-MACD(XTS[,c("Close")],nFast=fastMA,nSlow=slowEMA,nSig=signalMA,maType="EMA")
tmpIndicator<- cbind(tmpIndicator,tmpIndicator[,1]-tmpIndicator[,2])
tmpIndicator<-cbind(tmpIndicator,EMA(Cl(XTS),n=fastEMA))
tmpIndicator<-cbind(tmpIndicator,EMA(Cl(XTS),n=slowEMA))
tmpIndicator<-cbind(tmpIndicator,EMA(Cl(XTS),n=superslowEMA))
tmpIndicator<-cbind(tmpIndicator,stoch(XTS[,c("High","Low","Close")]))
tmpIndicator<-cbind(tmpIndicator,RSI(XTS[,"Close"],n=RSIperiod,maType="EMA"))
tmpIndicator<-cbind(tmpIndicator,TRIX(XTS[,"Close"], n=TRIXPeriod, nSig=TRIXSigPeriod))
#head(tmpIndicator)
#tail(tmpIndicator)
tmpIndicator<-cbind(tmpIndicator,ADX(XTS[,c("High","Low","Close")]))
colnames(tmpIndicator)<-c("MACD","MACD.sig","MACD.Hist",paste("EMA.",fastEMA,sep=""),paste("EMA.",slowEMA,sep=""),paste("EMA.",superslowEMA,sep=""),"Stoch.fastK","Stoch.fastD","Stoch.slowD","RSI","TRIX","TRIX.Sig","DI+","DI-","DX","ADX")
tail(tmpIndicator)
#head(tmpIndicator)
XTS<-cbind(XTS,tmpIndicator)
XTS<-na.omit(XTS)
#Scale RSI
XTS$RSI<-XTS$RSI/100
rm(list=ls(pattern="tmp"))
tail(XTS)

return(XTS)  
}
  
AddSignals_Short <- function(XTS)
{

#SumSignal-------------------------------------------------
# < THIS CODE HAS BEEN REMOVED - SORRY SECRET SAUCE :) > 

#SumSignal Penalties-----------------------------------------

#Bind SumSignals-------------------------------------------------
#SumSignal<-tmpSignal1+tmpSignal2+tmpSignal3+tmpSignal4+tmpSignal5+tmpSignal6+tmpSignal7
colnames(SumSignal)<-c("SumSignal")
colnames(NetSumSignal)<-c("NetSumSignal")
XTS<-cbind(XTS,SumSignal,NetSumSignal)
rm(list=ls(pattern="tmp"))
tail(XTS)

#% move
tmpPctMove <- ((XTS$Close/XTS$Open)-1)*100
colnames(tmpPctMove)<-c("PctMove")
tail(tmpPctMove)

#Pips move
tmpPipMove <- ((XTS$Close - XTS$Open))*10000
colnames(tmpPipMove)<-c("PipMove")
tail(tmpPipMove)

#Cumulative 5hr pip move
tmpCumPipMove <- cumsum(tmpPipMove)
colnames(tmpCumPipMove)<-c("CumPipMove")
tail(tmpCumPipMove)

tmpCum5PPipMove <- diff(tmpCumPipMove,5,na.pad=FALSE)
colnames(tmpCum5PPipMove)<-c("Cum5PPipMove")
tail(tmpCum5PPipMove)
head(tmpCum5PPipMove)

#Cumulative 10hr pip move
tmpCum10PPipMove <- diff(tmpCumPipMove,10,na.pad=FALSE)
colnames(tmpCum10PPipMove)<-c("Cum10PPipMove")
tail(tmpCum10PPipMove)
head(tmpCum10PPipMove)

#Bind all Move and Cumulative Move Data
XTS<-cbind(XTS,tmpPctMove, tmpPipMove, tmpCum5PPipMove, tmpCum10PPipMove)
rm(list=ls(pattern="tmp"))
#XTS<-na.omit(XTS)
head(XTS)
tail(XTS)

return(XTS)
}


AddSignals_Long <- function(XTS)
{
  # < THIS CODE HAS BEEN REMOVED - SORRY SECRET SAUCE :) > 
  
  #Bind SumSignals-------------------------------------------------
  #SumSignal<-tmpSignal1+tmpSignal2+tmpSignal3+tmpSignal4+tmpSignal5+tmpSignal6+tmpSignal7
  colnames(SumSignal)<-c("SumSignal")
  colnames(NetSumSignal)<-c("NetSumSignal")
  XTS<-cbind(XTS,SumSignal,NetSumSignal)
  rm(list=ls(pattern="tmp"))
  tail(XTS)
  
  #% move
  tmpPctMove <- ((XTS$Close/XTS$Open)-1)*100
  colnames(tmpPctMove)<-c("PctMove")
  tail(tmpPctMove)
  
  #Pips move
  tmpPipMove <- ((XTS$Close - XTS$Open))*10000
  colnames(tmpPipMove)<-c("PipMove")
  tail(tmpPipMove)
  
  #Cumulative 5hr pip move
  tmpCumPipMove <- cumsum(tmpPipMove)
  colnames(tmpCumPipMove)<-c("CumPipMove")
  tail(tmpCumPipMove)
  
  tmpCum5PPipMove <- diff(tmpCumPipMove,5,na.pad=FALSE)
  colnames(tmpCum5PPipMove)<-c("Cum5PPipMove")
  tail(tmpCum5PPipMove)
  head(tmpCum5PPipMove)
  
  #Cumulative 10hr pip move
  tmpCum10PPipMove <- diff(tmpCumPipMove,10,na.pad=FALSE)
  colnames(tmpCum10PPipMove)<-c("Cum10PPipMove")
  tail(tmpCum10PPipMove)
  head(tmpCum10PPipMove)
  
  #Bind all Move and Cumulative Move Data
  XTS<-cbind(XTS,tmpPctMove, tmpPipMove, tmpCum5PPipMove, tmpCum10PPipMove)
  rm(list=ls(pattern="tmp"))
  #XTS<-na.omit(XTS)
  head(XTS)
  tail(XTS)
  
  return(XTS)
}