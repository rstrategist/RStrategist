#Load Libraries

LoadLibraries <- function()
{
  # Install and update packages if required
  #install.packages("FinancialInstrument", repos="http://R-Forge.R-project.org")
  #install.packages("quantstrat", repos="http://R-Forge.R-project.org")
  #install.packages("blotter", repos="http://R-Forge.R-project.org")
  
  #Load Standard Libraries
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
  library(ggplot2)
  
  setwd("C:/R/RStrategist/XMom_GSR/Code")
  source("C:\\R\\RStrategist\\XMom_GSR\\Code\\ImportParameterConfig.R")
  source("C:\\R\\RStrategist\\XMom_GSR\\Code\\ImportData_Indices.R")
  source("C:\\R\\RStrategist\\XMom_GSR\\Code\\ImportData_RAI.R")
  source("C:\\R\\RStrategist\\XMom_GSR\\Code\\TransactionsGrid.R")
  source("C:\\R\\RStrategist\\XMom_GSR\\Code\\CalcReturn.R")
  source("C:\\R\\RStrategist\\XMom_GSR\\Code\\ExportFiles.R")

}