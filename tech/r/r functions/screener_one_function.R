screener.fetch <- function(filters,header,page) {
  #child function to get screener and format
  get_screener <- function(filters,header,page,node) {
    #stop as a bool variable to stop loop
    stop <- FALSE
    #set iterator i to 1 to be used in read-in while loop
    i <- 1
    #set table hCodes as text-to-number key for proper numerical codes for link 
    # construction
    #headers are from the FinViz screener tab. Use Overview for general info
    hCodes <- tribble(
      ~Header,~Code,
      "Overview",111,
      "Valuation",121,
      "Financial",161,
      "Ownership",131,
      "Performance",141,
      "Technical",171
    )
    #assign hCode as a numerical code used in link construction
    hCode <- hCodes$Code[grep(header,hCodes$Header)]
    #assign pCode as a numerical page counter in loop based on page argument
    # such that page==0 means all pages, otherwise the amount of pages to be 
    # read in
    pCode <- case_when(page==0 ~ 600, page!=0 ~ page)
    #while loop such that stop variable is FALSE and the iterator i is less than or
    # equal to pCode
    while(stop == FALSE & i <= pCode) {
      #assign url as html object such that a link is built and used as argument in a 
      # call to read_html()
      #v argument in url is header
      #r argument in url is index of first element on each page
      url <- read_html(
        paste("https://finviz.com/screener.ashx?",
              "v=",hCode,filters,"&r=",(((i-1)*20)+1),
              sep=""))
      #message status
      #if less than 2, it is testing nodes
      if(i <= 2) {
        cat(paste0("Searching Node[",node,"]...")," \r")
        flush.console()
      } else {
        #if past 2, it is working and will print messages each page
        cat(paste0("Reading and Converting | ",(((i-1)*20)+1))," \r")
        flush.console()
      }
      #assign tables as html nodes
      tables <- html_nodes(url,"table")
      #assign screen as dataframe with proper node found
      screen <- tables %>% html_nodes("table") %>% .[node] %>% 
        html_table(fill=TRUE) %>% data.frame()
      #set columnnames of screen as the first row from screen, as this is how data 
      # was brought in
      colnames(screen) <- screen[1,]
      #remove first row of screen as they are column names and real column names 
      # have been set
      screen <- screen[-1,]
      #clear rownames of screen
      rownames(screen) <- c()
      #if in first iteration
      if(i == 1) {
        #assign cScreener as screen
        cScreener <- screen
      } 
      if(nrow(screen)==20 & i != 1) {
        #if iteration is greater than one and screen is full, thus 20 rows long, 
        # combine screen onto existing cScreener and assign to cScreener
        cScreener <- rbind(cScreener,screen)
      }
      if(nrow(screen)!=20 & i != 1) {
        #if iteration is greater than one and screen is not full, thus not 20 rows 
        # long, combine screen onto existing cScreener and assign to cScreener, then 
        # assign value of TRUE to stop variable
        cScreener <- rbind(cScreener,screen)
        stop <- TRUE
      }
      #upgrade iterator i by 1
      i <- i+1
      #sys sleep to allow catching up and prevent crashing
      Sys.sleep(0.25)
    }
    #if page wanted is Overview
    if(hCode == 111) {
      cScreener <- cScreener %>%
        select(Ticker:Volume) %>%
        `colnames<-`(
          c("Ticker","Company","Sector","Industry","Country",
            "MktCap","PE","Price","Change","Volume")) %>%
        #convert large numbers to units of B, M, or K
        transform(MktCap = case_when(
          MktCap == "-" ~ 0,
          grepl("B", MktCap, fixed=TRUE) ~ as.numeric(gsub("B",'',MktCap))*1000000000,
          grepl("M", MktCap, fixed=TRUE) ~ as.numeric(gsub("M",'',MktCap))*1000000,
          grepl("K", MktCap, fixed=TRUE) ~ as.numeric(gsub("K",'',MktCap))*1000),
          #convert - to 0 in PE ratio
          PE = case_when(PE == "-" ~ 0, PE != "-" ~ as.numeric(PE)),
          #remove symbols and convert to numeric
          Price = as.numeric(Price),
          Change = as.numeric(gsub("%",'',Change)),
          Volume = as.numeric(gsub(",",'',Volume))
        ) %>%
        #remove duplicate rows
        unique() %>%
        #add date updated
        mutate(DateUpdated = Sys.Date())
    }
    #if page wanted is Valuation
    if(hCode == 121) {
      cScreener <- cScreener %>%
        select(Ticker:Volume) %>%
        `colnames<-`(
          c("Ticker","MktCap","PE","FwdPE","PEG","PS","PB","PC","PFCF","EPSthisY",
            "EPSnextY","EPSpast5Y","EPSnext5Y","SalesPast5Y","Price",
            "Change","Volume")) %>%
        transform(MktCap = case_when(
          MktCap == "-" ~ 0,
          grepl("B", MktCap, fixed=TRUE) ~ as.numeric(gsub("B",'',MktCap))*1000000000,
          grepl("M", MktCap, fixed=TRUE) ~ as.numeric(gsub("M",'',MktCap))*1000000,
          grepl("K", MktCap, fixed=TRUE) ~ as.numeric(gsub("K",'',MktCap))*1000),
          PE = case_when(PE == "-" ~ 0, PE != "-" ~ as.numeric(PE)),
          FwdPE = case_when(FwdPE == "-" ~ 0, FwdPE != "-" ~ as.numeric(FwdPE)),
          PEG = case_when(PEG == "-" ~ 0, PEG != "-" ~ as.numeric(PEG)),
          PS = case_when(PS == "-" ~ 0, PS != "-" ~ as.numeric(PS)),
          PB = case_when(PB == "-" ~ 0, PB != "-" ~ as.numeric(PB)),
          PC = case_when(PC == "-" ~ 0, PC != "-" ~ as.numeric(PC)),
          PFCF = case_when(PFCF == "-" ~ 0, PFCF != "-" ~ as.numeric(PFCF)),
          EPSthisY = case_when(EPSthisY == "-" ~ 0, 
                               EPSthisY != "-" ~ as.numeric(
                                 gsub("%",'',EPSthisY))),
          EPSnextY = case_when(EPSnextY == "-" ~ 0, 
                               EPSnextY != "-" ~ as.numeric(
                                 gsub("%",'',EPSnextY))),
          EPSpast5Y = case_when(EPSpast5Y == "-" ~ 0, 
                                EPSpast5Y != "-" ~ as.numeric(
                                  gsub("%",'',EPSpast5Y))),
          EPSnext5Y = case_when(EPSnext5Y == "-" ~ 0, 
                                EPSnext5Y != "-" ~ as.numeric(
                                  gsub("%",'',EPSnext5Y))),
          SalesPast5Y = case_when(SalesPast5Y == "-" ~ 0, 
                                  SalesPast5Y != "-" ~ as.numeric(
                                    gsub("%",'',SalesPast5Y))),
          Price = as.numeric(Price),
          Change = as.numeric(gsub("%",'',Change)),
          Volume = as.numeric(gsub(",",'',Volume))
        ) %>%
        unique() %>%
        mutate(DateUpdated = Sys.Date())
    }
    #if page wanted is Financial
    if(hCode == 161) {
      cScreener <- cScreener %>%
        select(Ticker:Volume) %>%
        `colnames<-`(
          c("Ticker","MktCap","Dividend","ROA","ROE","ROI","CurrentRatio",
            "QuickRatio","LTDebtEq","DebtEq","GrossMarg","OperMarg",
            "ProfitMarg","Earnings","Price","Change","Volume")) %>%
        transform(MktCap = case_when(
          MktCap == "-" ~ 0,
          grepl("B", MktCap, fixed=TRUE) ~ as.numeric(gsub("B",'',MktCap))*1000000000,
          grepl("M", MktCap, fixed=TRUE) ~ as.numeric(gsub("M",'',MktCap))*1000000,
          grepl("K", MktCap, fixed=TRUE) ~ as.numeric(gsub("K",'',MktCap))*1000),
          Dividend = case_when(Dividend == "-" ~ 0, 
                               Dividend != "-" ~ as.numeric(
                                 gsub("%",'',Dividend))),
          ROA = case_when(ROA == "-" ~ 0, 
                          ROA != "-" ~ as.numeric(
                            gsub("%",'',ROA))),
          ROE = case_when(ROE == "-" ~ 0, 
                          ROE != "-" ~ as.numeric(
                            gsub("%",'',ROE))),
          ROI = case_when(ROI == "-" ~ 0, 
                          ROI != "-" ~ as.numeric(
                            gsub("%",'',ROI))),
          CurrentRatio = case_when(CurrentRatio == "-" ~ 0, 
                                   CurrentRatio != "-" ~ as.numeric(
                                     CurrentRatio)),
          QuickRatio = case_when(QuickRatio == "-" ~ 0, 
                                 QuickRatio != "-" ~ as.numeric(QuickRatio)),
          LTDebtEq = case_when(LTDebtEq == "-" ~ 0, 
                               LTDebtEq != "-" ~ as.numeric(LTDebtEq)),
          DebtEq = case_when(DebtEq == "-" ~ 0, 
                             DebtEq != "-" ~ as.numeric(DebtEq)),
          GrossMarg = case_when(GrossMarg == "-" ~ 0, 
                                GrossMarg != "-" ~ as.numeric(
                                  gsub("%",'',GrossMarg))),
          OperMarg = case_when(OperMarg == "-" ~ 0, 
                               OperMarg != "-" ~ as.numeric(
                                 gsub("%",'',OperMarg))),
          ProfitMarg = case_when(ProfitMarg == "-" ~ 0, 
                                 ProfitMarg != "-" ~ as.numeric(
                                   gsub("%",'',ProfitMarg))),
          Price = as.numeric(Price),
          Change = as.numeric(gsub("%",'',Change)),
          Volume = as.numeric(gsub(",",'',Volume))
        ) %>%
        unique() %>%
        mutate(DateUpdated = Sys.Date())
    }
    #if page wanted is Ownership
    if(hCode == 131) {
      cScreener <- cScreener %>%
        select(Ticker:Volume) %>%
        `colnames<-`(
          c("Ticker","MktCap","Outstanding","Float","InsiderOwn","InsiderTrans",
            "InstOwn","InstTrans","FloatShort","ShortRatio","AvgVolume","Price",
            "Change","Volume")) %>%
        transform(MktCap = case_when(
          MktCap == "-" ~ 0,
          grepl("B", MktCap, fixed=TRUE) ~ as.numeric(gsub("B",'',MktCap))*1000000000,
          grepl("M", MktCap, fixed=TRUE) ~ as.numeric(gsub("M",'',MktCap))*1000000,
          grepl("K", MktCap, fixed=TRUE) ~ as.numeric(gsub("K",'',MktCap))*1000),
          Outstanding = case_when(Outstanding == "-" ~ 0,
                                  grepl("B", Outstanding, fixed=TRUE) ~ 
                                    as.numeric(
                                      gsub("B",'',Outstanding))*1000000000,
                                  grepl("M", Outstanding, fixed=TRUE) ~ 
                                    as.numeric(
                                      gsub("M",'',Outstanding))*1000000,
                                  grepl("K", Outstanding, fixed=TRUE) ~ 
                                    as.numeric(
                                      gsub("K",'',Outstanding))*1000),
          Float = case_when(
            Float == "-" ~ 0,
            grepl("B", Float, fixed=TRUE) ~ as.numeric(
              gsub("B",'',Float))*1000000000,
            grepl("M", Float, fixed=TRUE) ~ as.numeric(
              gsub("M",'',Float))*1000000,
            grepl("K", Float, fixed=TRUE) ~ as.numeric(
              gsub("K",'',Float))*1000),
          InsiderOwn = case_when(
            InsiderOwn == "-" ~ 0, 
            InsiderOwn != "-" ~ as.numeric(gsub("%",'',InsiderOwn))),
          InsiderTrans = case_when(
            InsiderTrans == "-" ~ 0, 
            InsiderTrans != "-" ~ as.numeric(gsub("%",'',InsiderTrans))),
          InstOwn = case_when(
            InstOwn == "-" ~ 0, 
            InstOwn != "-" ~ as.numeric(gsub("%",'',InstOwn))),
          InstTrans = case_when(
            InstTrans == "-" ~ 0, 
            InstTrans != "-" ~ as.numeric(gsub("%",'',InstTrans))),
          FloatShort = case_when(
            FloatShort == "-" ~ 0, 
            FloatShort != "-" ~ as.numeric(gsub("%",'',FloatShort))),
          ShortRatio = case_when(
            ShortRatio == "-" ~ 0, 
            ShortRatio != "-" ~ as.numeric(ShortRatio)),
          AvgVolume = case_when(
            AvgVolume == "-" ~ 0,
            grepl("B", AvgVolume, fixed=TRUE) ~ as.numeric(
              gsub("B",'',AvgVolume))*1000000000,
            grepl("M", AvgVolume, fixed=TRUE) ~ as.numeric(
              gsub("M",'',AvgVolume))*1000000,
            grepl("K", AvgVolume, fixed=TRUE) ~ as.numeric(
              gsub("K",'',AvgVolume))*1000),
          Price = as.numeric(Price),
          Change = as.numeric(gsub("%",'',Change)),
          Volume = as.numeric(gsub(",",'',Volume))
        ) %>%
        unique() %>%
        mutate(DateUpdated = Sys.Date())
    }
    #if page wanted is Performance
    if(hCode == 141) {
      cScreener <- cScreener %>%
        select(Ticker:Volume) %>%
        `colnames<-`(
          c("Ticker","PerformW","PerformM","PerformQ","PerformH","PerformY",
            "PerformYTD","VolatilityW","VolatilityM","Recom","AvgVolume",
            "RelVolume","Price","Change","Volume")) %>%
        transform(PerformW = case_when(
          PerformW == "-" ~ 0, 
          PerformW != "-" ~ as.numeric(
            gsub("%",'',PerformW))),
          PerformM = case_when(
            PerformM == "-" ~ 0, 
            PerformM != "-" ~ as.numeric(
              gsub("%",'',PerformM))),
          PerformQ = case_when(
            PerformQ == "-" ~ 0, 
            PerformQ != "-" ~ as.numeric(
              gsub("%",'',PerformQ))),
          PerformH = case_when(
            PerformH == "-" ~ 0,
            PerformH != "-" ~ as.numeric(
              gsub("%",'',PerformH))),
          PerformY = case_when(
            PerformY == "-" ~ 0, 
            PerformY != "-" ~ as.numeric(
              gsub("%",'',PerformY))),
          PerformYTD = case_when(
            PerformYTD == "-" ~ 0, 
            PerformYTD != "-" ~ as.numeric(
              gsub("%",'',PerformYTD))),
          VolatilityW = case_when(
            VolatilityW == "-" ~ 0, 
            VolatilityW != "-" ~ as.numeric(
              gsub("%",'',VolatilityW))),
          VolatilityM = case_when(
            VolatilityM == "-" ~ 0, 
            VolatilityM != "-" ~ as.numeric(
              gsub("%",'',VolatilityM))),
          Recom = case_when(
            Recom == "-" ~ 0, 
            Recom != "-" ~ as.numeric(Recom)),
          AvgVolume = case_when(
            AvgVolume == "-" ~ 0,
            grepl("B", AvgVolume, fixed=TRUE) ~ as.numeric(
              gsub("B",'',AvgVolume))*1000000000,
            grepl("M", AvgVolume, fixed=TRUE) ~ as.numeric(
              gsub("M",'',AvgVolume))*1000000,
            grepl("K", AvgVolume, fixed=TRUE) ~ as.numeric(
              gsub("K",'',AvgVolume))*1000),
          RelVolume = case_when(
            RelVolume == "-" ~ 0,
            RelVolume != "-" ~ as.numeric(RelVolume)),
          Price = as.numeric(Price),
          Change = as.numeric(gsub("%",'',Change)),
          Volume = as.numeric(gsub(",",'',Volume))
        ) %>%
        unique() %>%
        mutate(DateUpdated = Sys.Date())
    }
    #if page wanted is Technical
    if(hCode == 171) {
      cScreener <- cScreener %>%
        select(Ticker:Volume) %>%
        `colnames<-`(
          c("Ticker","Beta","ATR","SMA20","SMA50","SMA200","High52W",
            "Low52W","RSI","Price","Change","fromOpen","Gap","Volume")) %>%
        transform(Beta = case_when(
          Beta == "-" ~ 0, 
          Beta != "-" ~ as.numeric(Beta)),
          ATR = case_when(
            ATR == "-" ~ 0, 
            ATR != "-" ~ as.numeric(ATR)),
          SMA20 = case_when(
            SMA20 == "-" ~ 0, 
            SMA20 != "-" ~ as.numeric(
              gsub("%",'',SMA20))),
          SMA50 = case_when(
            SMA50 == "-" ~ 0, 
            SMA50 != "-" ~ as.numeric(
              gsub("%",'',SMA50))),
          SMA200 = case_when(
            SMA200 == "-" ~ 0, 
            SMA200 != "-" ~ as.numeric(
              gsub("%",'',SMA200))),
          High52W = case_when(
            High52W == "-" ~ 0, 
            High52W != "-" ~ as.numeric(
              gsub("%",'',High52W))),
          Low52W = case_when(
            Low52W == "-" ~ 0, 
            Low52W != "-" ~ as.numeric(
              gsub("%",'',Low52W))),
          RSI = case_when(
            RSI == "-" ~ 0, 
            RSI != "-" ~ as.numeric(RSI)),
          Price = as.numeric(Price),
          Change = as.numeric(gsub("%",'',Change)),
          fromOpen = case_when(
            fromOpen == "-" ~ 0, 
            fromOpen != "-" ~ as.numeric(
              gsub("%",'',fromOpen))),
          Gap = case_when(
            Gap == "-" ~ 0, 
            Gap != "-" ~ as.numeric(
              gsub("%",'',Gap))),
          Volume = as.numeric(gsub(",",'',Volume))
        ) %>%
        unique() %>%
        mutate(DateUpdated = Sys.Date())
    }
    #return cScreener
    return(cScreener)
  }
  #sets variable for whether to increase node or not
  skip_to_next <- FALSE
  #message to console
  message("Finding node.")
  #loop through node possibilities
  for(node in c(1:40)) {
    out <- tryCatch(
      #success
      {
        #call get_screener by passing current node
        s <- get_screener(filters,header,page,node)
        #print new line when successful (no errors)
        cat("\n")
        #print success message
        message("Success.")
        #return screener
        return(s)
      },
      #failure
      error=function(cond) {
        #set skip bool to true to increase node option
        skip_to_next <<- TRUE
        #return value of NA
        return(NA)
      },
      #always do:
      finally={
        
      }
    )
    #if skip bool is true, increase node option by 1
    if(skip_to_next) { next }
  }
  #return result of screener search
  return(out)
}

#no filters
#Overview tab from finviz.com Screener
#Page 0 means as many pages as there are
screener.fetchh("","Overview",0)