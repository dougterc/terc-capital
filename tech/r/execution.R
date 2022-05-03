source("~/terc-capital/tech/r/packages.R")
source("~/terc-capital/tech/r/db-connection.R")
source("~/terc-capital/tech/r/screener.R")

#load packages
packages.load()

#establish connection to database
mydb <- database.connect(TRUE,'root','Rangers2014!','terc-capital')

#initial screener insertion
screener.insert_sql(screener.get("","Overview",0,screener.find_node()),mydb)



