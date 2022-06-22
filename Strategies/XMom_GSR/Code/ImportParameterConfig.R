ImportParameterConfig <- function()
{
  ParameterConfig <- list('Indicators','Trading')
  
  ParameterConfig$Indicators <-
    list(# Indicators Parameters----
    fastMA=12
    ,slowMA=26
    ,fastMA=12
    ,slowMA=26
    ,signalMA=9
    ,maType="EMA"
    ,fastEMA=18
    ,slowEMA=36
    ,superslowEMA=40
    ,nFastK=14 #no. past periods to use
    ,nFastD=3 #no. smoothing periods to apply to Fast%K
    ,nSlowD=3 #no. smoothing periods to apply to Fast%D
    ,smooth=1 #no. internal smoothing periods to Fast%K
    ,RSIperiod=12
    ,TRIXPeriod=20
    ,TRIXSigPeriod=9
    )
  
  ParameterConfig$Trading <-
      list(#Trading Parameters----
      #Scaling in/out
      ScaleIn = TRUE
      ,ScaleOut = TRUE
      ,ScaleInFactor = 1/3
      ,ScaleOutFactor = 1/2
      #Equity Risk per trade/instrument
      ,EquityRisked = 0.05 #5% equity in single trade/instrument
      ,StopMove = 29 #pips
      ,LimitMove = 50 #pips
      ,TickSize = 0.01 #EURJPY tick size
      ,MaxLoss = -5000 #BlackBoard$StopLoss
      ,StopLossReset = -5000 #BlackBoard$StopLoss
      #SumSignals
      ,SumSignalThreshold = 4
      ,SumSignalExitThreshold = 1 
      )

  return(ParameterConfig)
}