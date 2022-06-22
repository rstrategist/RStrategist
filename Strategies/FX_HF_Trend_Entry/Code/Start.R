Start <- function()
{
# Clear out old portfolios and orders----------------------------------------
#try(rm(list=ls(pos=.blotter),pos=.blotter),silent=TRUE)
#try(rm(list=ls(pos=.strategy),pos=.strategy),silent=TRUE)
# Load Data--------------------------------------------------------------------
#originalDirectory <-getwd()
#setwd("C:/R/Data/Forex")
#setwd("M:/Personal/R/Data/Forex")

GBPUSD<-read.table("GBPUSD.csv",header=TRUE,sep=",")
#setwd=originalDirectory
GBPUSD$Volume <- NULL
head(GBPUSD)
tail(GBPUSD)

# Time=0201 is read as 201 so must force the string to be padded with zeros
tmpTime<-sprintf("%04d", GBPUSD[,2])
tmpTime<-paste(substr(tmpTime,1,2),":",substr(tmpTime,3,4), sep="")

# Convert d-m-y char to date
tmpDate<-zoo::as.Date(GBPUSD[,1],format="%m/%d/%Y")

# Create XTS of FX with POSIXct index
GBPUSD <- xts(as.matrix(GBPUSD[,-(1:2)]),as.POSIXct(paste(tmpDate,tmpTime),tz="GMT",format='%Y-%m-%d %H:%M')) 
rm(list=ls(pattern="tmp"))

#Name Columns
colnames(GBPUSD) <- c('Open','High','Low','Close')
head(GBPUSD)
# Convert TS to various timescales-----------------------------------------
GBPUSD5min=to.minutes5(GBPUSD, indexAt='endof')
colnames(GBPUSD5min) <- c('Open','High','Low','Close')

GBPUSD1hr=to.hourly(GBPUSD5min, indexAt='endof')
colnames(GBPUSD1hr) <- c('Open','High','Low','Close')
GBPUSD1hr$IndexNo <- 1:nrow(GBPUSD1hr)

return(GBPUSD1hr)
}