packages.load <- function() {
  require(scales)
  require(AER)
  require(tidyverse)
  require(broom)
  require(quantmod)
  require(tseries)
  require(quantmod)
  require(BatchGetSymbols)
  require(rvest) 
  require(datetime)
  require(lubridate)
  require(expss)
  require(dplyr)
  require(knitr)
  require(bizdays)
  require(fpp2)
  require(zoo)
  require(tidyr) #for crossing
  require(httr)
  require(jsonlite)
  require(rameritrade)
  require(matrixStats)
  require(readxl)
  require(sys)
  require(splitstackshape)
  require(googledrive)
  library(randNames)
  library(udpipe)
  require(RMySQL)
  require(DBI)
  require(xfun)
  require(ggplot2)
  "%!in%" <- function(x,y)!("%in%"(x,y))
  #devtools::install_github("msperlin/yfR", force = TRUE)
  library(yfR)
  library(caTools)
  library(forecast)
}
combo.get <- function(n, r) {
  orig <- expand.grid(c(1:n))
  for(i in seq(c(1:r))) {
    if(i == 1) {
      grid <- expand.grid(c(1:n))
    } else {
      grid <- orig %>%
        mutate(key = 1) %>%
        left_join(grid%>%mutate(key = 1),by=c("key"="key"))
      grid <- grid[ ,-grep("key",colnames(grid))]
      for(z in c(1:(ncol(grid)-1))) {
        for(y in c((z+1):ncol(grid))) {
          grid <- grid[(grid[,z] < grid[,y]), ]
        }
      }
    }
    rownames(grid) <- c()
  }
  items <- list(grid, nrow(grid)) %>%
    `names<-`(c("grid","count"))
  return(items)
}

#script
packages.load()
library(plotly)

objects <- 20
stocks <- 10
for(n in c(1:objects)) {
  for(r in c(1:stocks)) {
    if(n > r) {
      print(c(n, r))
      if(n == 2 & r == 1) {
        df <- tribble(
          ~n,~r,~c,
          n,r,combo.get(n, r)$count
        )
      } else {
        df <- add_row(
          df,
          n = n,
          r = r,
          c = combo.get(n, r)$count
        )
      }
    } else {
      
    }
  }
}
#write_csv(df,"./combo_calc_dataset.csv")
fig <- plot_ly(df, x = ~n, y = ~r, z = ~c)
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'Objects'),
                                   yaxis = list(title = 'Sample'),
                                   zaxis = list(title = 'Row Count')))
fig
