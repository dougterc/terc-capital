#libraries. Install if necessary
library(rvest)
library(dplyr)

#Insider trading URL
url <- "https://finviz.com/insidertrading.ashx"
page <- read_html(url)

#code that pulls the numbers. put in brackets to make cleaner
{
  #First line creates the variable, second line finds the ticker,
  #third line turns it into the text we actually need.
  columns <- page %>% 
    html_nodes(".table-top") %>%
    html_text()
  #View(columns)
  
  ticker <- page %>% 
    html_nodes(".cursor-pointer td:nth-child(1)") %>%
    html_text()
  #View(ticker)
  
  owner <- page %>% 
    html_nodes(".cursor-pointer td:nth-child(2)") %>%
    html_text()
  #View(owner)
  
  relationship <- page %>% 
    html_nodes(".cursor-pointer td:nth-child(3)") %>%
    html_text()
  #View(relationship)
  
  transaction_date <- page %>% 
    html_nodes(".cursor-pointer td:nth-child(4)") %>%
    html_text()
  #View(transaction_date)
  
  transaction <- page %>% 
    html_nodes(".cursor-pointer td:nth-child(5)") %>%
    html_text()
  #View(transaction)
  
  share_price <- page %>% 
    html_nodes(".cursor-pointer td:nth-child(6)") %>%
    html_text()
  #View(share_price)
  
  num_shares <- page %>% 
    html_nodes(".cursor-pointer td:nth-child(7)") %>%
    html_text()
  #View(num_shares)
  
  dollar_value <- page %>% 
    html_nodes(".cursor-pointer td:nth-child(8)") %>%
    html_text()
  #View(dollar_value)
  
  total_shares <- page %>% 
    html_nodes(".cursor-pointer td:nth-child(9)") %>%
    html_text()
  #View(total_shares)
  
  filing_date <- page %>% 
    html_nodes(".cursor-pointer td:nth-child(10)") %>%
    html_text()
  #View(filing_date)
  
  FinViz <- data.frame(
    ticker = ticker,
    owner = owner,
    relationship = relationship,
    transaction_date = transaction_date,
    transaction = transaction,
    share_price = share_price,
    num_shares = num_shares,
    dollar_value = dollar_value,
    total_shares = total_shares,
    filing_date = filing_date
  )
}

View(FinViz)