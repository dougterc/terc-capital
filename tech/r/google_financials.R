google.financials.get <- function(db,ticker) {
  #packages
  require(dplyr)
  require(rvest)
  line <- dbGetQuery(
    db,
    paste0("SELECT * FROM SCREENER WHERE Ticker = '", ticker, "';")
    )
  url <- paste0(
    "https://www.google.com/finance/quote/",
    line$ticker[1],
    ":",
    line$exchange[1]
    )
  page <- read_html(url)
  metrics <- page %>%
    html_nodes("td:nth-child(1)") %>%
    html_text()
  quarter <- page %>%
    html_nodes("td:nth-child(2)") %>%
    html_text()
  year_over_year <- page %>%
    html_nodes("td:nth-child(3)") %>%
    html_text()
  financials <- data.frame(
      ticker = ticker,
      metric = metrics,
      quarter = quarter,
      yoy = year_over_year
  )
  return(financials)
}