historical_equities_get <- function(ticker, first_date, last_date, tbd) {
  out <- tryCatch({
      #success
      temp <- yfR::yf_get(
        ticker,
        first_date = first_date,
        last_date = last_date,
        thresh_bad_data = tbd,
        freq_data = "daily") %>%
          filter(!is.na(ret_adjusted_prices))
      message(paste0(ticker, " read in successfully."))
      return(temp)
    },
    error = function(cond) {
      #failure
      message(paste0(ticker, " failed to read in."))
      return(NA)
    },
    warning = function(cond) {
      #warning
      message(paste0(ticker, " read in successfully with an error."))
      return(temp)
    },
    finally = {
      #regardless
    }
  )
  return(out)
}

#local function to insert historical into database
historical_equities_insert_sql <- function(db, histdata) {
  t <- unique(histdata$ticker)
  fromdb <- dbGetQuery(db,
    paste0("SELECT * FROM HIST_DATA WHERE Ticker = '", t, "';")
    )
  if (nrow(fromdb) > 0) {
    #existing rows in db, check dates
  } else {
    #insert full
    ins <- histdata %>%
      mutate(insert = paste0("(",
                            "'", ref_date, "'", ",",
                            "'", ticker, "'", ",",
                            price_open, ",",
                            price_high, ",",
                            price_low, ",",
                            price_close, ",",
                            price_adjusted, ",",
                            ret_closing_prices, ",",
                            ret_adjusted_prices, ",",
                            volume, ",",
                            "'", Sys.time(), "'", ")")
    )
    insert_state <- paste0(
        "INSERT INTO HIST_DATA ",
        "(", paste0(
          dbListFields(
            db,
            "HIST_DATA"
            )[-1], collapse = ", "
            ),
        ")",
        " VALUES ",
        paste(ins$insert, collapse = ","),
        ";"
      )
      dbGetQuery(db, insert_state)
  }
}