pairs <- c("USDJPY")
duration <- "10 D"
History <- "All"

TWS_FX_Downloader(pairs, duration, History)

TWS_FX_Downloader <- function(pairs, duration, History)
{
#Connect to TWS, Download FX data and update FX data files  

setwd("C:/R/Data/Forex")
pairs <- pairs
duration <- duration
History <- History

#Install relevant packages====
#install.packages("IBrokers")
#install.packages("twsInstrument", repos="http://R-Forge.R-project.org")
#install.packages("qmao", repos="http://R-Forge.R-project.org")


#Load Standard Libraries====
library(zoo)
library(quantmod)
library(blotter)
library(FinancialInstrument)
library(quantstrat)
library(PerformanceAnalytics)
library(quantstrat)
library(lattice)
library(stringr)
library(TTR)
library(xts)
library(IBrokers)
library(twsInstrument)

#Connect to TWS====
tws <- twsConnect()
reqCurrentTime(tws)

#Create contracts and request contract details====
#pairs <- c("EURUSD","USDCAD","USDJPY","USDCHF","AUDUSD","NZDUSD","USDSEK","USDNOK",
#           "EURGBP","EURCHF","EURJPY", "EURNOK","EURSEK",
#           "AUDJPY","NZDJPY")
pairs <- c("EURUSD")
#,"USDJPY","USDCHF","AUDUSD","NZDUSD","USDSEK","USDNOK",
           #           "EURGBP","EURCHF","EURJPY", "EURNOK","EURSEK",
           #           "AUDJPY","NZDJPY")

define_FX(pairs)
ls_twsInstruments() #all instruments containing IB slot with twsContract object
#get_quote(ls_twsInstruments()) #Not managed to get this to work yet. Hangs, may return something.

#Import data from files, download new data, update files
setwd("C:/R/Data/Forex")

for( i in 1:length(pairs) ) {
  Filename = paste("Data_",pairs[1],"_minute.csv", sep="")
  ColClasses = c("character", "numeric", "numeric", "numeric", "numeric")
  Data <- read.table(Filename, sep=",", header=TRUE, colClasses=ColClasses)
  tmpTime <- Data[,1]
  tmpTime <- as.POSIXct(tmpTime,tz="GMT",format='%Y-%m-%d %H:%M')
  head(tmpTime)
  tail(tmpTime)
  
  #Create XTS of FX with POSIXct index
  tmpData <- xts(as.matrix(Data[,-(1)]),tmpTime) #Leave original Data intact
  head(tmpData)
  head(Data)
  tail(Data)
  
  #Request historical data - 1 min for specified duration====
  (contract <- getContract(pairs[i]))
  if(History=="All"){
    print("1 year of history will be downloaded")
    tmp_newdata <- reqHistory(tws, contract,whatToShow = "BID")
  }else{
    comment <- paste(duration,"of history will be downloaded")
    print(comment)
    tmp_newdata <- reqHistoricalData(tws,contract,barSize = "1 min",duration = duration, whatToShow = "BID") # request historical data
  }
  
  #Requesting data backwards from a specific end date
  #tmp_newdata <- reqHistoricalData(tws,contract,barSize = "1 min",duration = "10 D", whatToShow = "BID", endDateTime = "20150620 11:22:00 GMT") # request historical data
  #endDateTime = "20150626 01:20:00 GMT"
  #'S' (seconds), 'D' (days), 'W' (weeks), 'M' (months), and 'Y' (year)
  
  #Remove unwated volume related columns
  tmp_newdata <- tmp_newdata[,-(5:9)]
  tail(tmp_newdata)
  head(tmp_newdata)
  #Identify non-overlapping timeseries
  tmp_newdata <- tmp_newdata[!time(tmp_newdata) %in% time(tmpData)]
  tail(tmpData,1)
  head(tmp_newdata,1)
  
  #Check a specific day in the new timeseries
  #head(tmp_newdata["2015-06-10/2015-06-10"])
  #head(tmp_newdata["2015-06-08 22:00:00/2015-06-10 11:24:00"],20)
  #head(tmpData["2015-06-08 22:00:00/2015-06-10 11:24:00"],20)
  
  #Bind the timeseries
  tmpData <- rbind(tmpData,tmp_newdata) 
  head(tmpData)
  tail(tmpData)
  #Remove duplicate time - should not need to run this
  #tmpData<-tmpData[!duplicated(index(tmpData))] 
  write.zoo(tmpData, file = paste("Data_",pairs[i],"_minute.csv",sep=""),sep=",")
  Sys.sleep(10)
  rm(list=ls(pattern="tmp")) #Clear tmp items
  Data <- NULL
  Sys.sleep(10)
}

twsDisconnect(tws) # disconnect from the TWS

# Removing instruments====
rm_instruments() #remove all but currencies
rm_currencies() #remove currencies only
}

#Request historical data - 1 minute data for prior year====
# for( i in 14:length(pairs) ) {
#   contract <- getContract(pairs[i])
#   #pricedata <- reqHistoricalData(tws,contract,barSize = "1 min",duration = "3 D", whatToShow = "BID") # request historical data
#   pricedata <- reqHistory(tws, contract,whatToShow = "BID")
#   head(pricedata)
#   tail(pricedata)
#   Sys.sleep(5)
#   pricedata <- pricedata[,-(5:8)]
#   Sys.sleep(5)
#   write.zoo(pricedata, file = paste("Data_",pairs[i],"_minute.csv",sep=""),sep=",")
#   pricedata <- NULL
# }
