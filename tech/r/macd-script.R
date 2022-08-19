source("./tech/functions/macd.R")

macd.packages.load()

ticker <- "F"
start.date <- as.Date("2022-01-01")
back.grab <- 180
end.date <- as.Date("2022-07-31")
pf.value <- 50000


data <- macd.data.close(ticker, start.date, end.date, back.grab)
open <- macd.data.open(ticker, start.date, end.date, back.grab)
#open <- as.data.frame(open)
sim.dates <- macd.sim.dates(data, start.date, end.date)

trip.margin <- 2
trip.back <- 3

track <- macd.track.setup(
  as.Date(index(data)[grep(sim.dates[1], index(data))-1]),
  pf.value)

for(i in seq(length(sim.dates))) {
  print(sim.dates[i])
  track$pl <- track$pl %>%
    rbind(track$pl[i,])
  track$pl$date[i+1] <- as.Date(sim.dates[i])
  
  res.data <- macd.research.data(data, sim.dates[i], 75)
  res.macd <- macd.add.macd(res.data, 12, 26, 9)
  #calculate trend
  res.trend <- macd.research.trend(res.macd)
  signal <- macd.sim.get.signal(res.trend, trip.margin, trip.back)
  print(signal)
  if(nrow(track$pf) == 0 & signal %in% c("SELL")) {
    #do nothing (HOLD)
  } else if (nrow(track$pf) > 0 & signal %in% c("SELL")) {
    #sell (you have a position)
    track <- macd.sim.sell(ticker, track, open, sim.dates[i])
  }
  if(nrow(track$pf) == 0 & signal %in% c("BUY")) {
    #buy (you have no positions)
    track <- macd.sim.buy(ticker, track, open, sim.dates[i])
  } else if (nrow(track$pf) > 0 & signal %in% c("BUY")) {
    #do nothing (already bought)
  }
  if(signal == "HOLD") {
    #do nothing
  }
  track <- macd.sim.eod(track, data, sim.dates[i], pf.value)
}
print(paste0(round(track$pl$profit[nrow(track$pl)] / pf.value*100,4),"%"))
stock <- as.data.frame(data[index(data) %in% track$pl$date]) %>%
  mutate(date = as.Date(rownames(.))) %>%
  mutate(gain = 0)
for(i in seq(nrow(stock))) {
  if(i == 1) {
    stock$gain[i] <- 0
  } else {
    stock$gain[i] <- stock$close[i] - stock$close[i-1]
  }
}
origShares <- as.data.frame(floor(pf.value / data$close[index(data) %in% c(sim.dates[1])]))[1,1]
stock <- stock %>%
  mutate(profit = cumsum(gain) * origShares)


ggplot() +
  geom_line(data = track$pl, aes(x = date, y = profit), color = "red") +
  geom_line(data = stock, aes(x = date, y = profit))


macd.graph <- as.data.frame(macd.add.macd(data[index(data) %in% c(sim.dates),], 12, 26 ,9))
macd.graph <- macd.graph %>%
  mutate(date = as.Date(rownames(.)))

ggplot() +
  geom_line(data = macd.graph, aes(x = date, y = MACD), color = "red") +
  geom_line(data = macd.graph, aes(x = date, y = signal), color = "blue") +
  geom_col(data = macd.graph, aes(x = date, y = diff), color = "green") +
  geom_vline(data = track$t, xintercept = c((track$t %>% filter(type=="BUY"))$date), color="black") +
  geom_vline(data = track$t, xintercept = c((track$t %>% filter(type=="SELL"))$date), color="purple")


cdf <- as.data.frame(sim.dates) %>%
  `colnames<-`(c("date")) %>%
  left_join(track$pl%>%select(date,profit), by=c("date"="date")) %>%
  left_join(stock%>%select(date,profit), by=c("date"="date")) %>%
  left_join(macd.graph%>%select(date,MACD,signal,diff), by=c("date"="date")) %>%
  `colnames<-`(c("date","trader","leftalone","MACD","signal","diff")) %>%
  left_join(track$t%>%filter(type=="BUY")%>%select(date,type)) %>%
  `colnames<-`(c("date","trader","leftalone","MACD","signal","diff","buys")) %>%
  left_join(track$t%>%filter(type=="SELL")%>%select(date,type)) %>%
  `colnames<-`(c("date","trader","leftalone","MACD","signal","diff","sells"))


ggplot(cdf, aes(x = date, y = value)) + 
  geom_line(aes(color = variable)) + 
  facet_grid(variable ~ ., scales = "free_y") + theme(legend.position = "none")
