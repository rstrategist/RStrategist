### Strategy - High Frequency Trend Entry

#### Strategy Outline

The FX high frequency series of strategies utilise tick and minute level data to trade fx market microstructure. The trend entry strategy observes the behaviour of the fx market in a trending mode and, at the micro-level, builds and risk-manages a position to take advantage of the continuing trend while minimising losses.

The code utilises a Blackboard to keep notes on the strategies performance and monitors many other metrics that can be used to make transaction decisions.

This light version shared here is an early and basic implementation using R's Blotter. More advanced versions were then implemented using R's quantstrat and bespoke libraries.

#### Basic Implementation

The basic implementation of the strategy observes multiple technical indicators for specific trade setups and selects opportune windows of time in which to build a position to take advantage of a trend continuation. The strategy risk-manages the possibility of trend reversal at this critical point as well as entry and exit position size management.

#### Enhanced Implementation

The enhanced implementation optimises strategy parameters, from technical and economic indicator input parameters, for position sizing and risk-management parameters.

#### Results

㊙️

#### Work In Progress

-   Update code to latest hierarchy.

    ✅ Test and fix bugs related to position sizing.

    ✅ Test over longer period.

    ✅ Test out-of-sample.

    ✅ Optimise parameters.
