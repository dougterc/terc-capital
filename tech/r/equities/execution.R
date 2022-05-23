source("~/terc-capital/tech/r/equities/packages.R")
source("~/terc-capital/tech/r/equities/db-connection.R")
source("~/terc-capital/tech/r/equities/screener.R")
source("~/terc-capital/tech/r/equities/historical.R")

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
for (ticker in tickers) {
    z <- historical_equities_get(
        ticker,
        Sys.Date() - 365,
        Sys.Date(),
        0
    )
    if (nrow(z) == 0) {
        next
    } else {
        historical_equities_insert_sql(mydb, z)
        Sys.sleep(0.25)
    }
}
