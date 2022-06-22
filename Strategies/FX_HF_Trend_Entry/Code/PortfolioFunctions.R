#Portfolio Functions

#RealtimRe Portfolio
CreatethePortfolioRealtime <- function (thePortfolioRealtime)
{
thePortfolioRealtime$summary$Close <- 0
thePortfolioRealtime$summary$Pip.Move <- 0
thePortfolioRealtime$summary$Position.Multiple <- 0
thePortfolioRealtime$summary$CumUnrealisedPnL <- 0

return(thePortfolioRealtime)
}
