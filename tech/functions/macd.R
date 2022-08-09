macd.packages.load <- function() {
  require(scales)
  require(AER)
  require(tidyverse)
  require(broom)
  require(quantmod)
  require(tseries)
  require(quantmod)
  require(BatchGetSymbols)
  require(rvest) 
  require(datetime)
  require(lubridate)
  require(expss)
  require(dplyr)
  require(knitr)
  require(bizdays)
  require(fpp2)
  require(zoo)
  require(tidyr) #for crossing
  require(httr)
  require(jsonlite)
  require(rameritrade)
  require(matrixStats)
  require(readxl)
  require(sys)
  require(splitstackshape)
  require(googledrive)
  library(randNames)
  library(udpipe)
  require(RMySQL)
  require(DBI)
  require(xfun)
  require(ggplot2)
  "%!in%" <- function(x,y)!("%in%"(x,y))
  #devtools::install_github("msperlin/yfR", force = TRUE)
  library(yfR)
  library(caTools)
  library(forecast)
}

macd.data.close <- function(ticker, start.date, end.date, back) {
  stock.data <- getSymbols(
    Symbols = ticker,
    src = "yahoo",
    from = start.date - back.grab,
    to = end.date,
    auto.assign = FALSE
  ) %>%
    `colnames<-`(c("open","high","low","close","volume","adjusted"))
  cl.data <- Cl(stock.data)
  return(cl.data)
}

macd.data.open <- function(ticker, start.date, end.date, back) {
  stock.data <- getSymbols(
    Symbols = ticker,
    src = "yahoo",
    from = start.date - back.grab,
    to = end.date,
    auto.assign = FALSE
  ) %>%
    `colnames<-`(c("open","high","low","close","volume","adjusted"))
  op.data <- Op(stock.data)
  return(op.data)
}

macd.add.macd <- function (price, S, L, K){
  MACD <- EMA(price,S) - EMA(price,L)
  signal <- EMA(MACD,K)
  output <- cbind(MACD,signal)
  output <- transform(output, diff = MACD - signal)
  colnames(output) <- c("MACD","signal","diff")
  return(output)
}

macd.data.frame.back <- function(ts.data) {
  df <- as.data.frame(ts.data) %>%
    mutate(refdate = as.Date(rownames(.)))
  return(df)
}

macd.data.frame <- function(ts.data, start, end) {
  df <- as.data.frame(ts.data) %>%
    mutate(refdate = as.Date(rownames(.))) %>%
    filter(refdate >= start & refdate <= end)
  return(df)
}

macd.sim.dates <- function(ts.data, start, end) {
  df <- macd.data.frame(ts.data, start, end)
  sim.dates <- c(df$refdate)
  return(sim.dates)
}

macd.research.data <- function(ts.data, current.date, back) {
  max.idx <- grep(current.date, index(ts.data))
  ret <- ts.data[c((max.idx - back):(max.idx-1))]
  return(ret)
}

macd.research.trend <- function(macd.data) {
  macd.data <- transform(macd.data, trend = 0)
  for(i in seq(nrow(macd.data))) {
    if(!is.na(macd.data$MACD[i]) & !is.na(macd.data$signal[i])) {
      if(!is.na(macd.data$diff[i])) {
        #calculate
        old <- as.numeric(as.data.frame(macd.data$diff[i-1])[1,1])
        new <- as.numeric(as.data.frame(macd.data$diff[i])[1,1])
        macd.data$trend[i] <- ifelse(new - old > 0, 1, ifelse(new - old == 0, 0, -1))
      } else {
        #leave as NA, no prior value
        macd.data$trend[i] <- NA
      }
    }
  }
  return(macd.data)
}

macd.sim.get.signal <- function(trend.data, trip.margin, trip.back) {
  idx <- nrow(trend.data)
  #if under 0 within margin and past back have been up, buy signal
  if(trend.data$diff[idx] >= (0 - trip.margin) & trend.data$diff[idx] < 0) {
    #within trip margin BUY side, check for trend
    if(sum((trend.data$trend[c((idx - trip.back + 1):idx)])$trend[1:trip.back]) == trip.back) {
      #success
      signal <- "BUY"
    } else {
      signal <- "HOLD"
    }
  #if above 0 within margin and past back have been down, sell signal
  } else if (trend.data$diff[idx] <= (0 + trip.margin) & trend.data$diff[idx] > 0) {
    #within trip margin SELL side, check for trend
    if(sum((trend.data$trend[c((idx - trip.back + 1):idx)])$trend[1:trip.back]) == -trip.back) {
      #success
      signal <- "SELL"
    } else {
      signal <- "HOLD"
    }
  } else {
    signal <- "HOLD"
  }
  return(signal)
}

macd.track.setup <- function(day.zero, start.value) {
  pf <- tribble(
    ~ticker, ~shares, ~cost,
    "BLANK", 0, 0.0
  ) %>%
    filter(ticker != "BLANK")
  
  t <- tribble(
    ~ticker, ~type, ~date, ~shares, ~price, ~cf,
    "BLANK", "BUY", as.Date("2021-01-01"), 0, 0.0, 0.0
  ) %>%
    filter(ticker != "BLANK")
  
  pl <- tribble(
    ~date, ~total.sold, ~total.bought, ~total.cf, ~positions, ~cash, ~current.value, ~profit,
    day.zero, 0.0, 0.0, 0.0, 0.0, start.value, start.value, 0.0
  )
  
  s <- tribble(
    ~date, ~shares,
    day.zero, 0
  )
  
  items <- list(pf, t, pl, s) %>%
    `names<-`(c("pf","t","pl","s"))
  return(items)
}

macd.sim.sell <- function(ticker, track, open, current.date) {
  loc <- grep(ticker, track$pf$ticker)
  loc.b <- grep(current.date, track$pl$date)
  s <- track$pf$shares[loc]
  
  track$pf$shares[loc] <- 0
  track$pf <- track$pf %>%
    filter(shares > 0)
  
  op.price <- as.data.frame(open[index(open) %in% c(current.date)])[1,1]
  cashflow <- (op.price) * s
  
  track$t <- add_row(track$t,
                     ticker = ticker,
                     type = "SELL",
                     date = as.Date(current.date),
                     shares = -s,
                     price = op.price,
                     cf = cashflow)

  track$pl$total.sold[loc.b] <- track$pl$total.sold[loc.b] + cashflow
  track$pl$total.cf[loc.b] <- track$pl$total.cf[loc.b] + cashflow
  track$pl$cash[loc.b] <- track$pl$cash[loc.b] + cashflow
  track$pl$positions[loc.b] <- 0
  track$pl$current.value[loc.b] <- track$pl$positions[loc.b] + track$pl$cash[loc.b]
  return(track)
}

macd.sim.buy <- function(ticker, track, open, current.date) {
  loc.b <- grep(current.date, track$pl$date)
  op.price <- as.data.frame(open[index(open) %in% c(as.Date(current.date))])[1,1]
  s <- floor((track$pl$current.value[loc.b]) / op.price)
  
  if(nrow(track$t) == 0) {
    track$s <- track$s %>%
      add_row(
        date = as.Date(current.date),
        shares = s
      )
  }
  
  track$pf <- track$pf %>%
    add_row(
      ticker = ticker,
      shares = 0,
      cost = 0
    )
  loc <- grep(ticker, track$pf$ticker)
  track$pf$shares[loc] <- s
  track$pf$cost[loc] <- op.price
  
  cashflow <- (op.price) * s * (-1)
  
  track$t <- add_row(track$t,
                     ticker = ticker,
                     type = "BUY",
                     date = as.Date(current.date),
                     shares = s,
                     price = op.price,
                     cf = cashflow)
  
  track$pl$total.bought[loc.b] <- track$pl$total.bought[loc.b] + abs(cashflow)
  track$pl$total.cf[loc.b] <- track$pl$total.cf[loc.b] + cashflow
  track$pl$cash[loc.b] <- track$pl$cash[loc.b] + cashflow
  track$pl$positions[loc.b] <- abs(cashflow)
  track$pl$current.value[loc.b] <- track$pl$positions[loc.b] + track$pl$cash[loc.b]
  return(track)
}

macd.sim.eod <- function(track, data, current.date, start.value) {
  if(nrow(track$pf) == 0) {
    track$pl$positions[nrow(track$pl)]  <- 0
  } else {
    track$pl$positions[nrow(track$pl)]  <- track$pf$shares[1] * data$close[index(data) %in% c(current.date)]
  }
  track$pl$current.value[nrow(track$pl)] <- track$pl$positions[nrow(track$pl)] +
    track$pl$cash[nrow(track$pl)]
  track$pl$profit[nrow(track$pl)] <- track$pl$current.value[nrow(track$pl)] - start.value
  return(track)
}