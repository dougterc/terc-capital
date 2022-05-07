source("~/terc-capital/tech/r/packages.R")
source("~/terc-capital/tech/r/db-connection.R")
source("~/terc-capital/tech/r/screener.R")
source("~/terc-capital/tech/r/historical.R")

#load packages
packages.load()
#establish connection to database
mydb <- database.connect(TRUE,'root','Rangers2014!','terc-capital')
node <- screener.find_node()
#s <- screener.get("","Overview",0,node)
#screener.insert_sql(s,mydb)
#update screener
screener.update(mydb)

z <- historical.equities.raw_data(
    c("AAPL","MSFT","LULU"),
    as.Date('2021-01-01'),
    Sys.Date(),
    0)


install.packages("Quandl")
library(Quandl)
install.packages("devtools")
library(devtools)

