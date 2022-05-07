historical.equities.raw_data <- function(tickers,startDate,endDate,tbd) {
    require(BatchGetSymbols)
    #call BatchGetSymbols with arguments, returns list of $df.control 
    # and $df.tickers
    rawData <- BatchGetSymbols(
        tickers,
        first.date = startDate,
        last.date = endDate,
        thresh.bad.data = tbd
    )
    #reassign rawData$df.tickers such that column names are clean, uniformed, & 
    # reordered, and missing return observations are filtered out
    rawData$df.tickers <- rawData$df.tickers %>%
        `colnames<-`(
            c("Open",
            "High",
            "Low",
            "Close",
            "Volume",
            "Adjusted",
            "RefDate",
            "Ticker",
            "RetAdj",
            "RetClose")) %>%
        select(
            RefDate,
            Ticker,
            Open,
            High,
            Low,
            Close,
            RetClose,
            Adjusted,
            RetAdj,
            Volume) %>%
        filter(is.na(RetClose) == FALSE)
    #reassign rawData$df.control such that column names are clean and uniform
    rawData$df.control <- rawData$df.control %>%
        `colnames<-`(
            c("Ticker",
            "SRC",
            "Status",
            "Obs",
            "Perc",
            "Decision"))
    #call TercVentures.gen.removeDuplicateDates to remove any duplicate date
    # observations, returns list of $Control and $Historical
#rawData <- historical.equities.unique_dates(rawData)
    #return rawData as list of $Control and $Historical
    return(rawData)
}

historical.equities.unique_dates <- function(rawData) {
  #assign tLength as numerical length such that it is the amount of tickers in
  # the dataset
  tLength <- length(c(rawData$df.control$Ticker))
  #for loop between 1 and the amount of tickers in the dataset
  for(i in seq(rawData$df.control$Ticker)) {
    #print status and wipe console
    cat(paste("Searching for duplicates in ",rawData$df.control$Ticker[i],
              " : ",round(i/tLength*100,digits=2),"%",sep=""))
    Sys.sleep(0.005)
    cat("\014")
    #if duplicated dates are found within the current ticker element
    if(TRUE %in% c(
      duplicated(
        (
          rawData$df.tickers %>%
            filter(Ticker == rawData$df.control$Ticker[i])
        )$RefDate
      )
    )
    ) {
      #action to remove
      #assign findDate as date where the duplicated element is found in
      # rawData$df.tickers
      findDate <- (rawData$df.tickers %>%
                    filter(Ticker == rawData$df.control$Ticker[i]) %>%
                    filter(duplicated(RefDate) == TRUE))$RefDate
      #assign indexes as a vector of the intersection between locations in
      # rawData$df.tickers where the duplicated date exists and locations
      # of the current ticker element, thus the currect duplicated date
      # locations
      indexes <- c(intersect(
                        grep(findDate,rawData$df.tickers$RefDate),
                        grep(rawData$df.control$Ticker[i],
                                rawData$df.tickers$Ticker)
                        ))
      #assign newLine as single line of data which is the condensed multiple
      # lines of duplicated dates subset the currect rows of duplicated dates,
      # group by Ticker, properly condense, reorder columns
      newLine <- rawData$df.tickers[indexes, ] %>%
        group_by(Ticker) %>%
        summarize(RefDate = max(RefDate),
                  Open = min(Open),
                  High = max(High),
                  Low = min(Low),
                  Close = max(Close),
                  RetClose = max(RetClose),
                  Adjusted = max(Adjusted),
                  RetAdj = max(RetAdj),
                  Volume = max(Volume)
        ) %>%
        select(
            RefDate,
            Ticker,
            Open,
            High,
            Low,
            Close,
            RetClose,
            Adjusted,
            RetAdj,
            Volume)
      #reassign newLine to rawData$df.tickers at maximum row of duplicated rows
      rawData$df.tickers[indexes[length(indexes)], ] <- newLine
      #reassign all indexes that are not the maximum row of duplicated rows
      # (which was just used) to indexes, for removal in next step
      indexes <- c(indexes[!indexes %in% indexes[length(indexes)]])
      #remove rows from rawData$df.tickers defined by indexes and reassign to
      # rawData$df.tickers
      rawData$df.tickers <- rawData$df.tickers[-indexes, ]
      #adjust number of observations in rawData$df.control by subtracting
      # indexes, thus the amount of deleted rows
      rawData$df.control$Obs[i] <- rawData$df.control$Obs[i] - length(indexes)
      #adjust the Perc column in rawData$df.control to be the proper percentage
      # of each row's total obs, such that each rows number of observations is
      # divided by the maximum instance of rows in the dataset
      rawData$df.control <- rawData$df.control %>%
        transform(Perc = Obs / max(Obs))
      #set rownames of rawData$df.tickers to NULL
      rownames(rawData$df.tickers) <- c()
    } else { 
    }
  }
  #set names in rawData list
  names(rawData) <- c("Control",
                        "Historical")
  #return the list of $Control and $Historical as rawData
}


historical.equities.get <- function(tickers,startDate,endDate,tbd) {
    raw <- historical.equities.raw_data(tickers,startDate,endDate,tbd)


}