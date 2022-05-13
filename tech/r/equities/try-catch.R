funcName <- function() {
  out <- tryCatch({
      #success
      message("")
      return()
    },
    error = function(cond) {
      #failure
      message("")
      return(NA)
    },
    warning = function(cond) {
      #warning
      message("")
      return(NULL)
    },
    finally = {
      #regardless
    }
  )
  return(out)
}