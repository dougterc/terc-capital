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
      message(paste0(ticker, " read in successfully with a warning."))
      return(NA)
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

last_mkt_day <- function(curr_day) {
  wd_today <- wday(curr_day)
  if (wd_today == 1) {
    #Sunday
    lm_day <- curr_day - 2
  } else if (wd_today == 2) {
    #Monday
    lm_day <- curr_day - 3
  } else {
    lm_day <- curr_day - 1
  }
  return(as.Date(lm_day))
}

next_mkt_day <- function(last_mkt_day) {
  wd_lmd <- wday(last_mkt_day)
  if (wd_lmd == 6) {
    #Friday
    nm_day <- last_mkt_day + 3
  } else {
    nm_day <- last_mkt_day + 1
  }
  return(as.Date(nm_day))
}

historical_equities_cyclical <- function(db, df) {
  failcount <- 0
  sleep_trigger <- 0
  for (i in seq(nrow(df))) {
      z <- historical_equities_get(
          df$Ticker[i],
          as.Date(df$MinDate[i]),
          as.Date(df$MaxDate[i]),
          0
      )
      if (nrow(z) == 0 || is.na(z)) {
          failcount <- failcount + 1
          sleep_trigger <- sleep_trigger + 1
          next
      } else {
          historical_equities_insert_sql(db, z)
          Sys.sleep(0.25)
      }
      if (sleep_trigger > 5) {
          Sys.sleep(5)
          sleep_trigger <- 0
      }
  }
  return(list(failcount, nrow(df)))
}

historical_equities_update <- function(db) {
  #get latest database dates for each stock
  max_dates <- dbGetQuery(
    db,
    "SELECT SCREENER.ticker, MAX(refdate)
    FROM HIST_DATA
    LEFT JOIN SCREENER ON HIST_DATA.ticker = SCREENER.ticker
    WHERE active = 1
    GROUP BY SCREENER.ticker;"
  ) %>%
  `colnames<-`(c("Ticker", "MinDate")) %>%
  transform(MinDate = as.Date(MinDate))
  #get last market day
  lm_day <- last_mkt_day(Sys.Date())
  #remaining stocks needing historical data
  other_stocks <- dbGetQuery(
    db,
    paste0(
      "SELECT ticker
      FROM SCREENER
      WHERE active = 1
      AND Ticker NOT IN (",
      paste0(
        (max_dates %>%
          mutate(coll = paste0(
            "'",
            Ticker,
            "'"
            )
          )
        )$coll,
        collapse = ","
      ),
      ");"
    )
  ) %>%
  `colnames<-`(c("Ticker")) %>%
  mutate(MinDate = as.Date(lm_day - 365)) %>%
  rowwise() %>%
  transform(MinDate = last_mkt_day(MinDate)) %>%
  ungroup()
  #combine stocks with data and stocks without
  max_dates <- rbind(
    max_dates,
    other_stocks
  ) %>%
  transform(MinDate = as.Date(MinDate))
  #add on current day for span
  max_dates <- max_dates %>%
    mutate(MaxDate = lm_day)
  #filter
  max_dates <- max_dates %>%
    filter(MinDate < MaxDate) %>%
    arrange(desc(MinDate))
  #get data
  if (nrow(max_dates) > 0) {
    fc <- historical_equities_cyclical(db, max_dates)
  }
  return(fc)
}

