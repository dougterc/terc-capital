source("~/terc-capital/tech/r/packages.R")
source("~/terc-capital/tech/r/db-connection.R")
source("~/terc-capital/tech/r/screener.R")

#load packages
packages.load()
#establish connection to database
mydb <- database.connect(TRUE,'root','Rangers2014!','terc-capital')
node <- screener.find_node()
s <- screener.get("","Overview",0,node)
screener.insert_sql(s,mydb)
#update screener
screener.update(mydb)



#initial screener insertion
screener.insert_sql(screener.get("","Overview",0,screener.find_node()),mydb)



