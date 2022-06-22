# Cross Momentum - Global Sector Rotation - Blotter - Indices
#Strategy Outline - See readme file.

#System Timezone Setup----
Sys.setenv(environment=".blotter", tz="Europe/London")
Sys.setenv(TZ="UTC")
Sys.setenv(tz="Europe/London")
Sys.timezone()

# Directories Setup----
originalDirectory <-getwd()
RFilesDirectory <- "C:\\R\\RStrategist\\XMom_GSR\\Code\\" # R Files
DataDirectory <- "C:\\R\\RStrategist\\XMom_GSR\\Data\\" # Data Files
OutputDirectory <- "C:\\R\\RStrategist\\XMom_GSR\\Output\\" # Export Output Files

#Load Libraries----
source(paste(RFilesDirectory,"LoadLibraries.R",sep=""))
LoadLibraries()
getwd()
search()

# Instruments and Data----
setwd(DataDirectory)
Indices <- read.table("Indices.csv",header=TRUE,sep=",",as.is = TRUE)

symbols <- Indices[,2] # Developed market indices
symbols.EM <- symbols[11:20] # EM indices
head(Indices)

# Clear out old portfolios and orders
try(rm(list=ls(pos=.blotter),pos=.blotter),silent=TRUE)
try(rm(list=ls(pos=.strategy),pos=.strategy),silent=TRUE)

# Set initial values
initDate='2000-01-01'
endDate='2013-11-15'

# Set currency
currency("USD")

# ETF or Index Data---

# Importing Data----
# Load data from csv's
setwd(DataDirectory)

IndicesXTS <- ImportData_Indices()
head(IndicesXTS)
tail(IndicesXTS)

# Load Risk Aversion Index (for advanced strategy)
setwd(DataDirectory)
RAI <- ImportData_RAI()
head(RAI)
tail(RAI)

#Initialise Accounts----
a.strategy <- "XMomentum_GSR"
initEq=1e6
#args(initPortf)
#args(initAcct)
initPortf(a.strategy,symbols.EM, initDate='2000-01-01', currency="USD")
initAcct(a.strategy,portfolios=a.strategy, initDate='2000-01-01', initEq=initEq, currency="USD")

#ls() #Global environment
#ls(envir=.blotter) #portfolio and account objects stored in .blotter environment
#ls(envir=FinancialInstrument:::.blotter)
#ls(envir=FinancialInstrument:::.instrument) #ETF and trading instrument objects stored in the .instrument environment
if(!exists(".blotter")) .blotter <- new.env()


# Strategy----

# Parameters
# No. long, neutral and short sectors
NLong <- 3
NNeutral <- 4
NShort <- 3

LookBackPeriod <- 12 # Look-back period in months
InvestmentHorizon <- 12 # Investment period in months

# Calculate Returns from Prices----
# Create ReturnXTS object and clean
head(IndicesXTS)
IndicesXTS <- IndicesXTS[-1,] # Exclude first row; Dec 1998 entry

# Split into DM/EM Indices
IndicesXTS.DM <- IndicesXTS[,1:10]
IndicesXTS.EM <- IndicesXTS[-1:-12,11:20]

head(IndicesXTS.DM)
head(IndicesXTS.EM)

# Create investable instruments from IndicesXTS----
stock(symbols.EM, "USD")

for(symbol in symbols.EM){
  sym <- IndicesXTS.EM[,symbol]
  colnames(sym) <- "Close"
  assign(symbol, sym, envir = .GlobalEnv)
}

# Cycle through ReturnXTS, find best return momentum and trade----
i=1

for( i in 1:NROW(IndicesXTS.EM) ) {
  #Set Current Date
  (CurrentDate=time(IndicesXTS.EM)[i])
  (equity = getEndEq(a.strategy, CurrentDate))
  
  # Sector return momentum: Highest, lowest
  ReturnXTS.DM <- CalcReturn(IndicesXTS.DM, CurrentDate, LookBackPeriod)
  
  # Return vectors of long top 3, neutral 4, and short bottom 3
  (Long <- colnames(ReturnXTS.DM[,1:NLong]))
  (Neutral <- colnames(ReturnXTS.DM[,(NLong+1):(NLong+NNeutral)]))
  (Short <- colnames(ReturnXTS.DM[,-(1:7)]))

  # Identify EM equivalent sectors
  Long.EM <- 0
  for (j in 1:length(Long)) {
    Long.EM[j] <- paste("MSFU",str_sub(Long[j],5,str_length(Long[j])),sep="")
  }
  
  Neutral.EM <- 0
  for (j in 1:length(Neutral)) {
    Neutral.EM[j] <- paste("MSFU",str_sub(Neutral[j],5,str_length(Neutral[j])),sep="")
  }

  Short.EM <- 0
  for (j in 1:length(Short)) {
    Short.EM[j] <- paste("MSFU",str_sub(Short[j],5,str_length(Short[j])),sep="")
  }
  
  # Get transaction sizes to move to new positions for EM sectors----
  ThePortfolio = getPortfolio(a.strategy)
  TheAccount = getAccount(a.strategy)
  LongShort.EM <- c(Long.EM,Short.EM)
  (tmpPosnTrade.LongShort <- TransactionsGrid(LongShort.EM, Long.EM, Short.EM, symbols.EM, CurrentDate, Strategy=a.strategy,IndicesXTS.EM[CurrentDate],BuySell=1))
  
  # Exclude neutral position on first run as only long and short side entered
  if(i!=1){ 
  (tmpPosnTrade.Neutral <- TransactionsGrid(Neutral.EM, symbols.EM, CurrentDate, Strategy=a.strategy,IndicesXTS.EM[CurrentDate],BuySell=0))
  }
  
  # Execution of trades - Cycle through each required transaction----
  #symbol <- names(tmpPosnTrade.Long[1])
  #tmpPosn <- list()
  
  # All Trades
  for(symbol in names(tmpPosnTrade.LongShort)){
  sym = get(symbol)
  head(sym)
  (ClosePrice = as.numeric(sym[CurrentDate]))
  (TradeQty <- as.numeric(tmpPosnTrade.LongShort[symbol]))
  
  addTxn(a.strategy, Symbol=symbol, TxnDate=CurrentDate,
         TxnPrice=ClosePrice, TxnQty = TradeQty , TxnFees=0)
  } # End all transactions loop

  # Calculate P&L and resulting equity with blotter
  updatePortf(a.strategy, Symbols = symbols.EM, Dates = CurrentDate)
  updateAcct(a.strategy, Dates = CurrentDate)
  updateEndEq(a.strategy, Dates = CurrentDate)
  
  rm(list=ls(pattern="tmp"))
  #PreviousDate <- CurrentDate
  #(tmpPreviousDate <- as.POSIXct(paste(PreviousDate),tz="GMT",format='%Y-%m-%d'))
} # End dates loop

 # Export Files---- See function for details of reports produced
 CurrentDate = time(CarryXTS)[i]
 ExportFiles(a.strategy, OutputDirectory, symbols.EM, IndicesXTS.EM)

# Performance Analytics----
 ThePortfolio = getPortfolio(a.strategy)
 TheAccount = getAccount(a.strategy)
  
# Charts and trade analytics----
dev.list()
dev.cur()

# Export PDF summary of position for each instrument
AllInstr = symbols.EM
for (Instr in AllInstr){
  chart.Posn(a.strategy, Symbol=Instr, Dates='2000::')
  plot(add_SMA(n=10,col=4,on=1,lwd=2))
  fname <- paste("GSR - ",Instr,".pdf",sep="")
  dev.copy(pdf,width = 11.69, height = 8.27,fname)
  dev.off()
}

# Plot 
TradingPL <- getPortfolio(a.strategy)$summary$Net.Trading.PL
sum(TradingPL)
rets <- TradingPL/initEq
charts.PerformanceSummary(rets,
                          colorset = bluefocus,
                          xlab="",
                          main = "Basic Cross Momentum Global Sector Rotation Strategy Performance")
dev.copy(pdf,"Basic Cross Momentum Global Sector Rotation Strategy Performance.pdf")
dev.off()

plot(xyplot(ThePortfolio$summary,xlab="",type="h",col=4))
dev.copy(pdf,"GSR - Portfolio Summary.pdf",width = 11.69, height = 8.27)
dev.off()

