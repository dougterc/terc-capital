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
#-------
macd.packages.load()

ticker <- "NFLX"
data.close <- macd.data.close(ticker, Sys.Date()-365, Sys.Date(),120)
data.open <- macd.data.open(ticker, Sys.Date()-365, Sys.Date(),120)
data.macd <- macd.add.macd(data.close, 12, 26, 9) %>%
  as.data.frame() %>%
  filter(!is.na(MACD) & !is.na(signal) & !is.na(diff)) %>%
  mutate(
    chgA = 0,
    chgB = 0
  )
for(a in seq(length(index(data.macd)))) {
  if(a > 1) {
    data.macd$chgA[a] <- data.macd$diff[a] - data.macd$diff[a-1]
  }
}
for(b in seq(length(index(data.macd)))) {
  if(b > 1) {
    data.macd$chgB[b] <- data.macd$chgA[b] - data.macd$chgA[b-1]
  }
}
data.returns <- data.close %>%
  as.data.frame() %>%
  mutate(ret = 0)
for(i in seq(nrow(data.returns))) {
  if(i > 1) {
    data.returns$ret[i] <- (data.returns$close[i] / data.returns$close[i-1]) - 1
  }
}
data.returns <- data.returns %>%
  mutate(cumulative = cumsum(ret)) %>%
  mutate(date = rownames(.))

data.macd <- data.macd %>%
  mutate(smaA = SMA(chgA)) %>%
  mutate(smaB = SMA(chgB)) %>%
  mutate(date = rownames(.)) %>%
  left_join(data.returns, by=c("date"="date")) %>%
  left_join(as.data.frame(data.open)%>%mutate(date=rownames(as.data.frame(data.open))), by=c("date"="date"))


rownames(data.macd) <-  data.macd$date
data.macd <- data.macd %>%
  select(MACD:smaB,close:open)

x_axis <- seq(1, nrow(data.macd), length=nrow(data.macd))
y_axis <- seq(-10, 10,length = 21)
ggplot(data.macd) +
  geom_col(aes(x = x_axis, y = diff)) +
  geom_line(aes(x = x_axis, y = chgA), color = "red") +
  geom_line(aes(x = x_axis, y = cumulative), color = "blue") +
  scale_x_continuous(minor_breaks = seq(0, length(x_axis), 1))

data.macd <- data.macd %>%
  mutate(trigger = NA)
upBuffer <- 0.00
dnBuffer <- 0.00
for(b in seq(length(index(data.macd)))) {
  if(b == 1) {
    data.macd$trigger[b] <- "HOLD"
  } else {
    last <- data.macd$chgA[b-1]
    now <- data.macd$chgA[b]
    if(last < (0 - dnBuffer) & now > (0 + upBuffer)) {
      data.macd$trigger[b] <- "BUY"
    } else if(last > (0 + upBuffer) & now < (0 - dnBuffer)) {
      data.macd$trigger[b] <- "SELL"
    } else {
      data.macd$trigger[b] <- "HOLD"
    }
  }
}
data.macd

#buy and sell at open, recognizd P&L at close
cash <- 10000
data.trade <- data.macd %>%
  mutate(
    shares = 0,
    cost = 0,
    cash = cash,
    pos = 0,
    pv = cash
    )

for(i in seq(length(index(data.trade)))) {
  if(i == 1) {
    if(data.trade$trigger[i] == "BUY") {
      data.trade$cost[i] <- data.trade$open[i]
      data.trade$shares[i] <- floor(data.trade$cash[i] / data.trade$cost[i])
      data.trade$cash[i] <- cash - (data.trade$cost[i] * data.trade$shares[i])
    }
  } else {
    if(data.trade$trigger[i] == "BUY") {
      if(data.trade$shares[i-1] == 0) {
        #BUY
        data.trade$cost[i] <- data.trade$open[i]
        data.trade$shares[i] <- floor(data.trade$cash[i-1] / data.trade$cost[i])
        data.trade$cash[i] <- data.trade$cash[i-1] - (data.trade$cost[i] * data.trade$shares[i])
      } else {
        #Nothing
        data.trade$cost[i] <- data.trade$cost[i-1]
        data.trade$shares[i] <-  data.trade$shares[i-1]
        data.trade$cash[i] <- data.trade$cash[i-1] 
      }
    } else if(data.trade$trigger[i] == "SELL") {
      if(data.trade$shares[i-1] > 0) {
        data.trade$cost[i] <- 0
        data.trade$shares[i] <- 0
        data.trade$cash[i] <- data.trade$cash[i-1] + (data.trade$shares[i-1]*data.trade$open[i])
      } else {
        #Nothing
        data.trade$cost[i] <- data.trade$cost[i-1]
        data.trade$shares[i] <-  data.trade$shares[i-1]
        data.trade$cash[i] <- data.trade$cash[i-1] 
      }
    } else {
      #Hold
      data.trade$cost[i] <- data.trade$cost[i-1]
      data.trade$shares[i] <-  data.trade$shares[i-1]
      data.trade$cash[i] <- data.trade$cash[i-1] 
    }
  }
  data.trade$pos[i] <- data.trade$shares[i] * data.trade$close[i]
  data.trade$pv[i] <- data.trade$pos[i] + data.trade$cash[i]
} 

x_axis <- seq(1, nrow(data.trade), length=nrow(data.trade))

data.trade <- data.trade %>%
  mutate(
    port.ret = 0,
    total.ret = 0
  )
for(i in seq(length(index(data.trade)))) {
  if(i == 1) {
    
  } else {
    data.trade$port.ret[i] <- ((data.trade$pv[i] / data.trade$pv[i-1]) - 1)
    data.trade$total.ret[i] <- (data.trade$pv[i] - data.trade$pv[1]) / data.trade$pv[1]
  }
} 

# data.trade <- data.trade %>%
#   rowwise() %>%
#   mutate(total.ret = (pv - cash) / cash) %>%
#   ungroup()

ggplot(data.trade) +
  geom_col(aes(x = x_axis, y = diff)) +
  geom_line(aes(x = x_axis, y = chgA), color = "red") +
  geom_line(aes(x = x_axis, y = total.ret), color = "blue") +
  scale_x_continuous(minor_breaks = seq(0, length(x_axis), 1))

paste0("Portfolio: ",
       round(data.trade$total.ret[length(index(data.trade))]*100,2),"% | ",
       ticker,": ",round(data.trade$cumulative[length(index(data.trade))]*100,2),"%")

