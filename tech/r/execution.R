source("~/terc-capital/tech/r/packages.R")
source("~/terc-capital/tech/r/db-connection.R")
source("~/terc-capital/tech/r/screener.R")
source("~/terc-capital/tech/r/historical.R")

#load packages
packages.load()
#establish connection to database
mydb <- database.connect(TRUE,'root','Rangers2014!','terc-capital')
screener.update(mydb)
tickers <- dbGetQuery(
        mydb,
        "SELECT Ticker FROM SCREENER WHERE active = 1;"
        ) %>%
        transform(Ticker = gsub(" ", "", Ticker))
tickers <- c(tickers$Ticker)
z <- historical.equities.download(
    as.vector(tickers),
    Sys.Date() - 365,
    Sys.Date(),
    0)
