directory = getwd()

source(paste(directory, "r functions/packages.R", sep = "/"))
source(paste(directory, "r functions/db-connection.R", sep = "/"))
source(paste(directory, "r functions/screener.R", sep = "/"))
source(paste(directory, "r functions/historical.R", sep = "/"))

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

#delete
dbGetQuery(mydb, "DELETE FROM HIST_DATA WHERE ticker != ' ';")
dbGetQuery(mydb, "ALTER TABLE tablename AUTO_INCREMENT = 1;")

failcount <- historical_equities_update(mydb)
