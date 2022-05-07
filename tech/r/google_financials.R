#packages
library(dplyr)
library(rvest)

#exchange rate URL
ticker <- "TRVN"
exchange <- "NASDAQ"
url <- paste0("https://www.google.com/finance/quote/", ticker, ":", exchange)
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
