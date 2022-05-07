historical.equities.get <- function(ticker, first_date, last_date, tbd) {
  temp <- yfR::yf_get(
    ticker,
    first_date = first_date,
    last_date = last_date,
    thresh_bad_data = tbd,
    freq_data = "daily") %>%
      filter(!is.na(ret_adjusted_prices))
  horizontal <- yfR::yf_convert_to_wide(vertical)
  data <- list(vertical, horizontal)
  return(temp)
}

historical.equities.download <- function(tickers, first_date, last_date, tbd) {
  skip_to_next <- FALSE
  message("Downloading data.")
  for (ticker in (tickers)) {
    out <- tryCatch({
        s <- historical.equities.get(ticker, first_date, last_date, tbd)
        return(s)
      },
      error = function(cond) {
        skip_to_next <<- TRUE
        return(NA)
      },
      finally = {
      }
    )
    #if skip bool is true, increase node option by 1
    if (skip_to_next) {
      next
    }
  }
}
