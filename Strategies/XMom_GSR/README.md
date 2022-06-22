### Strategy - Cross-Momentum Global Sector Rotation

#### Strategy Outline

The Cross-Momentum Global Sector Rotation (XMom-GSR) strategy invests across EM sectors according to past global sector performance. The cross-momentum nature of the strategy captures both the market inefficiency exploited by momentum strategies and the slow information diffusion across segmented markets.

#### Basic Implementation

Monthly, the strategy monitors the past 12-month performance of the 10 MSCI global sector indices. It then ranks the sectors from highest to lowest performing. The strategy goes long the top 3, neutral the middle 4, and short the bottom 3 performing sector indices. Rebalancing occurs on this monthly observation date.

The drawback with this basic implementation is that we are always in the market, there are no stop-losses and the market is observed only monthly. This strategy has proven to be lucrative, however, the result of these drawbacks is that there are occasionally very large drawdowns from a risk-off environment in equities. This is observed from the performance deterioration in 2008-2009, although the strategy did recover and make new highs in subsequent years.

#### Enhanced Implementation

The enhanced implementation makes use of a Risk Appetite Index (RAI) as a risk-aversion index to exit the market in a risk-off environment. A further enhancement could be to maintain short-only positions in negative momentum sector indices.

#### Results

-   GSR - Portfolio Summary

-   GSR - Basic Cross Momentum Global Sector Rotation Strategy Performance

-   More detailed results can be obtained by running the ExportFiles.R function in the code repository.

#### Work In Progress

-   Import Risk Appetite Index (RAI).
-   Change code to move to cash in a risk-off environment or maintain short-only positions at monthly observation dates.
-   Add leverage.
-   Add stop-loss.
-   Potentially test on sector ETF data as opposed to index data with a spread and include transaction costs. These have currently been ignored as the sector ETFs track the indices very closely and ETF transaction costs are very small.
