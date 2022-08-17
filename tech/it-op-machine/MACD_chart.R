stock_data <- getSymbols(
  Symbols = "TSLA",
  src = "yahoo",
  from = Sys.Date()-365,
  to = Sys.Date(),
  auto.assign = FALSE
)
stock_data <- Cl(stock_data)

#charting
chart_Series(stock_data, col = "black")
add_SMA(n = 100, on = 1, col = "red")
add_SMA(n = 20, on = 1, col = "black")
add_RSI(n = 14, maType = "SMA")
add_BBands(n = 20, maType = "SMA", sd = 1, on = -1)
add_MACD(fast = 12, slow = 25, signal = 9, maType = "SMA", histogram = TRUE)
