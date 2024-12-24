library(PerformanceAnalytics) # for daily returns
library(PortfolioAnalytics)
library(quantmod)
library(ROI)
library(ROI.plugin.glpk)
library(ROI.plugin.quadprog)
library(ROI.plugin.symphony)
library(jsonlite)  # for reading JSON
library(dplyr)     # for data manipulation
library(lubridate) # for date manipulation
library(xts)
library(reshape2)
library(moments)

all_dates = seq.Date(from = as.Date("2019-10-17"), to = as.Date("2024-11-27"), by = "day") # creating date sequence 

# setting benchmark index date sequence
from_date = "2019-10-17"
to_date = "2024-11-28"

# EMC
emc.historical = read.csv('historical_prices.csv')

emc_complete = merge(
  expand.grid(Date = all_dates, Security = unique(emc.historical$Security)),
  emc.historical,
  by = c("Date", "Security"),
  all.x = TRUE
) # formatting data frame by date range and security

emc_wide = dcast(emc_complete, Date ~ Security, value.var = "Price") # reshaping data frame

emc_xts = xts(emc_wide[,-1], order.by = emc_wide$Date) # creating time series object indexed by date

emc.portfolioPrices = emc_xts[,-c(2:3,6,12,15,17,29:33)] # creating subset xts object 

emc.portfolioReturns = na.omit(ROC(emc.portfolioPrices)) # calculating daily change in price

emc.num_assets = ncol(emc.portfolioReturns) # number of assets
emc.weights = rep(1 / emc.num_assets, emc.num_assets) # equal weighting

dim(emc.portfolioPrices)
emc.portfolioReturn = Return.portfolio(emc.portfolioReturns, weights = emc.weights) # emc total portfolio daily return 

benchmarkPrices = getSymbols.yahoo("^GSPTSE" , # setting benchmark index as S&P/TSX
                                   from=from_date,
                                   to=to_date,# specifying time frame
                                   periodicity="daily", # specifying daily data
                                   auto.assign=FALSE)[,4] # specifying closing price

colSums(is.na(benchmarkPrices)) # checking for missing data

benchmarkReturns = na.omit(ROC(benchmarkPrices)) # calculating daily change in price

combined_returns = cbind(emc.portfolioReturn, benchmarkReturns) # combining emc total portfolio daily return and benchmark daily return

clean_daily_returns = combined_returns[!is.na(combined_returns$GSPTSE.Close), ] # removed non-trading days

# Calculate monthly returns for 'portfolio.returns' column
monthly_portfolio_returns = apply.monthly(clean_daily_returns[, "portfolio.returns"], FUN = function(x) {
  prod(1 + x) - 1  # Calculate the compounded return for the month
})

# Calculate monthly returns for 'GSPTSE.Close' column
monthly_benchmark_returns = apply.monthly(clean_daily_returns[, "GSPTSE.Close"], FUN = function(x) {
  prod(1 + x) - 1  # Calculate the compounded return for the month
})

monthly_returns = cbind(monthly_portfolio_returns, monthly_benchmark_returns) # Combining the monthly returns into one xts object

MAR = 0.005
print(UpsideRisk(monthly_returns[,1], MAR, stat="risk")) #expected 0.03841214
print(UpsideRisk(monthly_returns[,1], MAR, stat="variance")) #expected 0.001475492
print(UpsideRisk(monthly_returns[,1], MAR, stat="potential")) #expected 0.02156759

# QQ plot to assess normality of monthly returns
qqnorm(monthly_returns[,1], 
       main="QQ Plot of Selected Monthly Portfolio Returns (EMC)")
qqline(monthly_returns[,1], col="red", lwd=2)

# Density plot of monthly portfolio returns
plot(density(monthly_returns[,1]), 
     main = "Density Plot of Selected Monthly Portfolio Returns (EMC)", 
     xlab = "Returns", 
     col = "blue", 
     lwd = 2)

# Add the filled area under the curve
polygon(density(monthly_returns[,1]), 
        col = adjustcolor("#69b3a2", alpha.f = 0.8), # Adjust color opacity
        border = NA)

kurtosis_value = kurtosis(monthly_returns[,1])
print(kurtosis_value) # 6.151027, leptokurtic behaviour 

VaR_value = VaR(monthly_returns[,1], p = 0.95, method = "modified")
print(VaR_value) # with 95% confidence, we can expect the worst case scenario that the portfolio loses up to 11.08% of its value for a one month time horizon 

charts.PerformanceSummary(
  monthly_returns,
  main = "Selected Assets (EMC) Summary",      # Custom Title
  col = c("blue", "red"),                   # Custom colors for performance and drawdown
  rug = FALSE,                              # Turn off rug plot
  cex.axis = 1.2,                           # Increase axis label size
  cex.lab = 1.5,                            # Increase axis label size
  table = c("Return", "Risk", "Drawdown"),  # Display selected performance metrics
  layout = c(1, 3),                         # Set the layout of the charts
  dateFormat = "%Y-%m"                      # Customize date format on x-axis
)

chart.VaRSensitivity(
  R = monthly_returns$portfolio.returns,  # Specify the returns column for VaR calculation
  methods = c("GaussianVaR", "ModifiedVaR", "HistoricalVaR", "GaussianES", "ModifiedES", "HistoricalES"),
  clean = c("none", "boudt", "geltner"),  # Clean the data if necessary
  elementcolor = "darkgray",              # Color for the plot elements
  reference.grid = TRUE,                  # Add a reference grid
  xlab = "Confidence Level",              # X-axis label
  ylab = "Value at Risk",                 # Y-axis label
  type = "l",                             # Type of plot (line plot)
  lty = c(1, 2, 4),                       # Line types for the methods
  lwd = 1,                                # Line width
  colorset = (1:12),                      # Color set for different methods
  pch = (1:12),                           # Point types for the plot
  legend.loc = "bottomleft",              # Location for the legend
  cex.legend = 0.8,                       # Size of the legend
  main = "VaR and ES Sensitivity Selected Assets (EMC)",        # Main title
  ylim = NULL                              # Optional Y-axis limits
)