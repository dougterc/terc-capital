#packages
library(dplyr)
library(rvest)

#exchange rate URL
url <- "https://www.x-rates.com/historical/?from=USD"
page <- read_html(url)
  
#First line creates the variable, second line finds the ticker,
#third line turns it into the text we actually need.

currency <- page %>% 
  html_nodes("td:nth-child(1)") %>%
  html_text()

USD_Target <- page %>% 
  html_nodes(".rtRates:nth-child(2)") %>%
  html_text()

Target_USD <- page %>% 
  html_nodes(".rtRates+ .rtRates") %>%
  html_text()

x_rates <- data.frame(
  currency = currency,
  One_USD_gets_you = USD_Target,
  One_currency_gets_you_x_USD = Target_USD
)


View(x_rates)
