#Load Libraries

LoadLibraries <- function(LoadEnvironment)
{
  
  #install.packages("IBrokers")
  #install.packages("twsInstrument", repos="http://R-Forge.R-project.org")
  #install.packages("qmao", repos="http://R-Forge.R-project.org")
  library(zoo)
  library(quantmod)
  library(blotter)
  library(FinancialInstrument)
  library(quantstrat)
  library(PerformanceAnalytics)
  library(IBrokers)
  library(twsInstrument)
  
  setwd("M:/Personal/R/Data/Forex")
  source("M:\\Personal\\R\\R Script\\Chart_Posn.R")
  source("M:\\Personal\\R\\R Script\\Chart.ME.R")
  source("M:\\Personal\\R\\R Script\\FX Short Strategy\\Start.R")
  source("M:\\Personal\\R\\R Script\\FX Short Strategy\\ChartTS.R")
  source("M:\\Personal\\R\\R Script\\FX Short Strategy\\AddIndicators.R")
  source("M:\\Personal\\R\\R Script\\FX Short Strategy\\ImportParameterConfig.R")
  source("M:\\Personal\\R\\R Script\\FX Short Strategy\\BlackBoard.R")
  source("M:\\Personal\\R\\R Script\\FX Short Strategy\\PortfolioFunctions.R")
  
}