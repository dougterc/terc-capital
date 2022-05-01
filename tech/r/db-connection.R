connectDB <- function(local,user,pswd,name) { 
  out <- tryCatch(
    {
      conn <- NA
      if(local == TRUE) {
        #establish SQL connection
        mydb = dbConnect(MySQL(), 
                         user=user, 
                         password=pswd, 
                         dbname=name, 
                         host='localhost')
        conn <- mydb
      } else {
        
      }
      message("Database Connected.")
      return(conn)
    },
    error=function(cond) {
      message("Database Connection Failed")
      message("Here's the original error message:")
      message(cond)
      # Choose a return value in case of error
      return(NA)
    },
    warning=function(cond) {
      message("Database Connection Caused A Warning")
      message("Here's the original warning message:")
      message(cond)
      # Choose a return value in case of warning
      return(NULL)
    },
    finally={
      
    }
  )    
  return(out)
}