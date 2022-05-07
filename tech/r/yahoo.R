#packages
library(dplyr)
library(rvest)

#exchange rate URL
url <- "https://finance.yahoo.com/quote/AAPL/history?period1=1620259200&period2=1651795200&interval=1d&filter=history&frequency=1s&includeAdjustedClose=true"
page <- read_html(url)
  
#First line creates the variable, second line finds the ticker,
#third line turns it into the text we actually need.

metrics <- page %>%
  html_nodes("td:nth-child(1)") %>%
  html_text()
figures <- page %>%
  html_nodes("td:nth-child(2)") %>%
  html_text()

financials <- data.frame(
    ticker = ticker,
    metric = metrics,
    value = figures
)


View(financials)


t1 <- as.integer(ISOdate(2022, 5, 5,hour=0))
t2 <- as.integer(ISOdate(2022, 5, 2,hour=0))

stock <- "AAPL"
url <- paste("https://query1.finance.yahoo.com/v7/finance/download/",
 stock,
 "?period1=",
 as.integer(t1),
 "&period2=",
 as.integer(t2),
 "&interval=1d&events=history",
 sep="")

url

dataset <- read.csv(url)
View(dataset)

z <- yfR::yf_get("AAPL",Sys.Date()-30,Sys.Date())
