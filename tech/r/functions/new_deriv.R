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
  require(RMySQL)
  require(DBI)
  require(xfun)
  require(ggplot2)
  "%!in%" <- function(x,y)!("%in%"(x,y))
  #devtools::install_github("msperlin/yfR", force = TRUE)
  require(yfR)
  require(caTools)
  require(forecast)
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
macd.derivative.equation <- function(equation) {
  int.loc <- grep("(Intercept)",names(equation$coefficients))
  if(length(int.loc)>0) {
    coeff <- equation$coefficients[-int.loc]
  }
  new.equation <- c(coeff[1] %>% `names<-`(c("Intercept")))
  for(n in c(2:length(coeff))) {
    new.power <- n-1
    new.equation <- append(new.equation, (coeff[n] * n) %>% `names<-`(c(new.power)))
  }
  for(i in seq(length(new.equation))) {
    if(is.na(new.equation[i])) {
      new.equation[i] <- 0
    }
  }
  return(new.equation)
}
macd.derivative.calculate <- function(equation, x) {
  intercept <- equation[grep("Intercept",names(equation))] %>% `names<-`(c())
  sum <- intercept
  for(n in c(2:length(equation))) {
    sum <- sum + (equation[n] * (x^(n-1)))
  }
  return(sum)
}
macd.equation.fit <- function(df, max.fit) {
  for(i in c(1:max.fit)) {
    if(i == 1) {
      fits <- tribble(
        ~fit,~r.squared,
        0,0.0
      ) %>%
        filter(fit != 0)
      fit <- lm(y~x, data=df)
    } else {
      fit <- lm(y~poly(x,i,raw=TRUE), data=df)
    }
    temp <- data.frame(x = i, y = summary(fit)$adj.r.squared)
    fits <- rbind(fits, temp)
  }
  fits<- fits %>%
    arrange(desc(y))
  if(fits$x[1] == 1) {
    fit <- lm(y~x, data=df)
  } else {
    fit <- lm(y~poly(x,fits$x[1],raw=TRUE), data=df)
  }
  items <- list(fit, fits) %>%
    `names<-`(c("equation","r.squares"))
  return(items)
}
macd.derivative.equation.next <- function(equation) {
  equation <- equation[-grep("Intercept",names(equation))]
  #non equation, just vector. used for second and more
  if(length(equation) > 1) {
    new.equation <- c(equation[1] %>% `names<-`(c("Intercept")))
    for(n in c(2:length(equation))) {
      new.power <- n-1
      new.equation <- append(new.equation, (equation[n] * n) %>% `names<-`(c(new.power)))
    }
  }
  for(i in seq(length(new.equation))) {
    if(is.na(new.equation[i])) {
      new.equation[i] <- 0
    }
  }
  return(new.equation)
}

#-------SCRIPT-STARTS-HERE-----------------

macd.packages.load()

#basic data
ticker <- "GE"
lb <- 120
data.close <- macd.data.close(ticker, Sys.Date()-365, Sys.Date(),lb)
data.open <- macd.data.open(ticker, Sys.Date()-365, Sys.Date(),lb)

#dates
dates <- as.Date(unique(index(data.close)))
dates
start.date <- as.Date("2021-08-01")
end.date <- as.Date("2022-07-31")
sim.dates <- dates[dates >= start.date & dates <= end.date]
sim.dates

upBuffer <- 0.00
dnBuffer <- 0.00
seed.money <- 10000

#initialize tracker table. open, close, diff, deriv, trigger, shares, cost, pos, cash, value, return, etc.

trade <- data.open %>%
  as.data.frame() %>%
  mutate(date = rownames(.)) %>%
  select(date, open) %>%
  left_join(data.close%>%as.data.frame()%>%mutate(date=rownames(.)),by=c("date"="date")) %>%
  transform(date = as.Date(date))
trade <- trade %>%
  filter(date %in% sim.dates) %>%
  mutate(
    diff = 0,
    deriv = 0,
    trigger = NA,
    cash = seed.money,
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

#simulation
for(d in seq(length(sim.dates))) {
  lb.dates <- dates[dates <= sim.dates[d] & dates >= sim.dates[d]-lb]
  c.data <- data.close[index(data.close) %in% lb.dates]
  #replace current day price with open. we dont know close yet, but we basically know open to run before
  # market opens. this gets us as close as possible with trigger
  c.data$close[length(index(c.data))] <- data.open$open[index(data.open) %in% c(sim.dates[d])]
  macd.data <- macd.add.macd(c.data, 12, 26, 9) %>%
    as.data.frame() %>%
    filter(!is.na(MACD) & !is.na(signal) & !is.na(diff)) %>%
    mutate(deriv = 0)
  for(a in seq(length(index(macd.data)))) {
    if(a > 1) {
      #only change starting index 2. first has no change basis, keep 0
      macd.data$deriv[a] <- macd.data$diff[a] - macd.data$diff[a-1]
    }
  }#end deriv for loop
  macd.data <- macd.data %>%
    mutate(trigger = NA)
  for(b in seq(length(index(macd.data)))) {
    if(b == 1) {
      macd.data$trigger[b] <- "HOLD"
    } else {
      last <- macd.data$deriv[b-1]
      now <- macd.data$deriv[b]
      if(last < (0 - dnBuffer) & now > (0 + upBuffer)) {
        macd.data$trigger[b] <- "BUY"
      } else if(last > (0 + upBuffer) & now < (0 - dnBuffer)) {
        macd.data$trigger[b] <- "SELL"
      } else {
        macd.data$trigger[b] <- "HOLD"
      }
    }#end non-first-iteration else
  }#end trigger set for loop
  #set factors in trade log for current day
  idx <- length(index(macd.data))
  trade$diff[d] <- macd.data$diff[idx]
  trade$deriv[d] <- macd.data$deriv[idx]
  trade$trigger[d] <- macd.data$trigger[idx]
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
  }
}#end simulation for loop


write_csv(trade, paste0("./",ticker,"_trade",round(trade$port.cumul.ret[nrow(trade)],5),".csv"))
