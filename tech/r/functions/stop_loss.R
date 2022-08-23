#---functions---
macd.packages.load <- function() {
  if(!require(scales)){
    install.packages("scales")
    library(scales)
  }
  if(!require(AER)){
    install.packages("AER")
    library(AER)
  }
  if(!require(tidyverse)){
    install.packages("tidyverse")
    library(tidyverse)
  }
  if(!require(broom)){
    install.packages("broom")
    library(broom)
  }
  if(!require(quantmod)){
    install.packages("quantmod")
    library(quantmod)
  }
  if(!require(tseries)){
    install.packages("tseries")
    library(tseries)
  }
  if(!require(BatchGetSymbols)){
    install.packages("BatchGetSymbols")
    library(BatchGetSymbols)
  }
  if(!require(rvest)){
    install.packages("rvest")
    library(rvest)
  }
  if(!require(datetime)){
    install.packages("datetime")
    library(datetime)
  }
  if(!require(lubridate)){
    install.packages("lubridate")
    library(lubridate)
  }
  if(!require(expss)){
    install.packages("expss")
    library(expss)
  }
  if(!require(dplyr)){
    install.packages("dplyr")
    library(dplyr)
  }
  if(!require(knitr)){
    install.packages("knitr")
    library(knitr)
  }
  if(!require(bizdays)){
    install.packages("bizdays")
    library(bizdays)
  }
  if(!require(fpp2)){
    install.packages("fpp2")
    library(fpp2)
  }
  if(!require(zoo)){
    install.packages("zoo")
    library(zoo)
  }
  if(!require(tidyr)){
    install.packages("tidyr")
    library(tidyr)
  }
  if(!require(httr)){
    install.packages("httr")
    library(httr)
  }
  if(!require(jsonlite)){
    install.packages("jsonlite")
    library(jsonlite)
  }
  if(!require(rameritrade)){
    install.packages("rameritrade")
    library(rameritrade)
  }
  if(!require(matrixStats)){
    install.packages("matrixStats")
    library(matrixStats)
  }
  if(!require(readxl)){
    install.packages("readxl")
    library(readxl)
  }
  if(!require(sys)){
    install.packages("sys")
    library(sys)
  }
  if(!require(splitstackshape)){
    install.packages("splitstackshape")
    library(splitstackshape)
  }
  if(!require(googledrive)){
    install.packages("googledrive")
    library(googledrive)
  }
  if(!require(RMySQL)){
    install.packages("RMySQL")
    library(RMySQL)
  }
  if(!require(DBI)){
    install.packages("DBI")
    library(DBIv)
  }
  if(!require(xfun)){
    install.packages("xfun")
    library(xfun)
  }
  if(!require(ggplot2)){
    install.packages("ggplot2")
    library(ggplot2)
  }
  if(!require(yfR)){
    install.packages("yfR")
    library(yfR)
  }
  if(!require(forecast)){
    install.packages("forecast")
    library(forecast)
  }
  if(!require(caTools)){
    install.packages("caTools")
    library(caTools)
  }
  return("%!in%" <- function(x,y)!("%in%"(x,y)))
}
macd.data.close <- function(ticker, start.date, end.date, back.grab) {
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
macd.data.open <- function(ticker, start.date, end.date, back.grab) {
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
macd.variables.set <- function(ticker, look.back, stop.loss, up.buffer, dn.buffer, seed.money, start.date, end.date) {
  return(
    list(
      ticker,
      look.back,
      stop.loss,
      up.buffer,
      dn.buffer,
      seed.money,
      as.Date(start.date),
      as.Date(end.date)
    ) %>%
      `names<-`(
        c(
          "ticker", "lb", "stpl", "upBuff",
          "dnBuff","seed.money","start.date","end.date"
        )
      )
  )
}
macd.track.init <- function(open.data, close.data, vars) {
  df <- open.data %>%
    as.data.frame() %>%
    mutate(date = rownames(.)) %>%
    select(date, open) %>%
    left_join(close.data%>%as.data.frame()%>%mutate(date=rownames(.)),by=c("date"="date")) %>%
    transform(date = as.Date(date)) %>%
    filter(date %in% sim.dates) %>%
    mutate(
      diff = 0,
      deriv = 0,
      trigger = NA,
      cash = vars$seed.money,
      shares = 0,
      cost = 0,
      execution = NA,
      pos = shares * cost,
      port.value = cash + pos,
      port.ret = 0,
      port.cumul.ret = 0,
      stock.ret = 0,
      stock.cumul.ret = 0
    )
  return(df)
}

#---script---
"%!in%" <- macd.packages.load()

#---inputs---
v <- macd.variables.set("AAPL", 120, 0.10, 0.00, 0.00, 100000, "2021-08-01", "2022-07-31")

#---data---
data.close <- macd.data.close(v$ticker, v$start.date-180, v$end.date, v$lb)
data.open <- macd.data.open(v$ticker, v$start.date-180, v$end.date, v$lb)
dates <- as.Date(unique(index(data.close)))
sim.dates <- dates[dates >= v$start.date & dates <= v$end.date]

#---track-info---
trade <- macd.track.init(data.open, data.close, v)

#---simulation---
for(d in seq(length(sim.dates))) {
  lb.dates <- dates[dates <= sim.dates[d] & dates >= sim.dates[d]-v$lb]
  c.data <- data.close[index(data.close) %in% lb.dates]
  #replace current day price with open. we dont know close yet, but we basically know open to run before
  # market opens. this gets us as close as possible with trigger
  c.data$close[length(index(c.data))] <- data.open$open[index(data.open) %in% c(sim.dates[d])]
  #get MACD data by closing prices with current day's open used
  data.macd <- macd.add.macd(c.data, 12, 26, 9) %>%
    as.data.frame() %>%
    filter(!is.na(MACD) & !is.na(signal) & !is.na(diff)) %>%
    mutate(deriv = 0) %>%
    mutate(trigger = NA)
  #get derivative of difference
  for(a in seq(length(index(data.macd)))) {
    if(a > 1) {
      #only change starting index 2. first has no change basis, keep 0
      data.macd$deriv[a] <- data.macd$diff[a] - data.macd$diff[a-1]
    }
  }#end deriv for loop
  #get signals
  for(b in seq(length(index(data.macd)))) {
    if(b == 1) {
      data.macd$trigger[b] <- "HOLD"
    } else {
      last <- data.macd$deriv[b-1]
      now <- data.macd$deriv[b]
      if(last < (0 - dnBuffer) & now > (0 + upBuffer)) {
        data.macd$trigger[b] <- "BUY"
      } else if(last > (0 + upBuffer) & now < (0 - dnBuffer)) {
        data.macd$trigger[b] <- "SELL"
      } else {
        data.macd$trigger[b] <- "HOLD"
      }
    }#end non-first-iteration else
  }#end trigger set for loop
  #set factors in trade log for current day
  idx <- length(index(macd.data))
  trade$diff[d] <- macd.data$diff[idx]
  trade$deriv[d] <- macd.data$deriv[idx]
  trade$trigger[d] <- macd.data$trigger[idx]
  #actions
  if(d == 1) {
    #first iteration
    if(trade$trigger[d] == "BUY") {
      trade$cost[d] <- trade$open[d]
      trade$shares[d] <- floor(trade$cash[d] / trade$cost[d])
      trade$cash[d] <- trade$cash[d] - (trade$cost[d] * trade$shares[d])
    }
  } else {
    #remaining iterations
    if(trade$trigger[d] == "BUY") {
      if(trade$shares[d-1] == 0) {
        #BUY
        trade$cost[d] <- trade$open[d]
        trade$shares[d] <- floor(trade$cash[d-1] / trade$cost[d])
        trade$cash[d] <- trade$cash[d-1] - (trade$cost[d] * trade$shares[d])
        trade$execution[d] <- "EXECUTED"
      } else {
        #HOLD and carry, repeat buy signal
        trade$cost[d] <- trade$cost[d-1]
        trade$shares[d] <- trade$shares[d-1]
        trade$cash[d] <- trade$cash[d-1]
        trade$execution[d] <- "IGNORED"
      }
    } else if(trade$trigger[d] == "SELL") {
      if(trade$shares[d-1] > 0) {
        #SELL
        trade$cost[d] <- 0
        trade$shares[d] <- 0
        trade$cash[d] <- trade$cash[d-1] + (trade$shares[d-1] * trade$open[d])
        trade$execution[d] <- "EXECUTED"
      } else {
        #HOLD and carry, repeat sell signal
        trade$cost[d] <- trade$cost[d-1]
        trade$shares[d] <- trade$shares[d-1]
        trade$cash[d] <- trade$cash[d-1]
        trade$execution[d] <- "IGNORED"
      }
    } else {
      #HOLD and carry
      trade$cost[d] <- trade$cost[d-1]
      trade$shares[d] <- trade$shares[d-1]
      trade$cash[d] <- trade$cash[d-1]
    }
  }#end of buy/sell else statement
  #calculate end of day portfolio value and P&L
  trade$pos[d] <- trade$shares[d] * trade$close[d]
  trade$port.value[d] <- trade$cash[d] + trade$pos[d]
  if(d > 1) {
    trade$port.ret[d] <- ((trade$port.value[d] / trade$port.value[d-1]) - 1)
    trade$port.cumul.ret[d] <- ((trade$port.value[d] / seed.money) - 1)
    trade$stock.ret[d] <- ((trade$close[d] / trade$close[d-1]) - 1)
    trade$stock.cumul.ret[d] <- ((trade$close[d] / trade$close[1]) - 1)
  }#end return calculations
} #end sim for loop
