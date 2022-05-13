convert_sql_date <- function(date) {
  dt <- gsub("-","",date)
  loc <- str_locate(dt,":")[1][1]
  hr <- as.numeric(substr(dt,loc-2,loc-1)) 
  dt <- paste0(dt,ifelse(hr >= 12," PM"," AM"))
  sloc <- str_locate(dt," ")[1][1]
  if(hr > 12) {
    hr <- hr-12
    fp <- substr(dt,1,sloc)
    ep <- substr(dt,loc,nchar(dt))
    dt <- paste0(fp,hr,ep)
  }
  return(dt)
}
