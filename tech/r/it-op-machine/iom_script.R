source("./iom.R")
#get packages
iom.packages.load()

#set vars
group <- 20
select <- 15
iom.combo.get(group, select)$count

start.date <- as.Date("2020-01-01")
end.date <- as.Date("2022-07-31")

#screener <- iom.screener.full("Overview")
tickers <- sort(screener$Ticker[sample(nrow(screener),group)])

data <- iom.data.package(tickers,start.date,end.date,"return.close")

lb.start <- as.Date(max(data$stacked$refdate))
lb.end <- as.Date(lb.start - 180)

combos <- iom.combo.get(group, select)$grid
for(i in seq(ncol(combos))) {
  combos[ ,i] <- xlookup(combos[ ,i],c(1:group),tickers)
  colnames(combos)[i] <- paste0("Stk",i)
}

lb.data <- data$stacked %>%
  filter(refdate >= lb.end & refdate <= lb.start)

comboCount <- nrow(combos)
for(n in seq(comboCount)) {
  message(paste0(n,"/",comboCount," ",round(n/comboCount*100,2),"%"),"\r",appendLF=FALSE)
  flush.console()
  line <- combos[n, ]
  lb.hz <- iom.data.matrix(
    lb.data %>%
      filter(ticker %in% c(transpose(line)$V1)),
    "ticker",
    "refdate",
    "return.close"
  )
  ret.data <- as.matrix(lb.hz[ ,!sapply(lb.hz, anyNA)])
  opt <- portfolio.optim(ret.data, shorts = FALSE)
  temp <- tribble(
    ~comboNum,~return, ~risk, ~stocks,
    n, opt$pm, opt$ps, paste(c(transpose(line)$V1), collapse = "-")
  )
  if(n == 1) {
    results <- temp
  } else {
    results <- rbind(results,temp)
  }
} 

results %>%
  arrange(desc(return))


View(read_csv("~/combo_calc_dataset.csv"))


df <- data.frame(x=1:15,
                 y=c(1, 2, 3, 25, 23, 15, 9, 5, 9, 13, 17, 24, 32, 36, 46))

#create a scatterplot of x vs. y
plot(df$x, df$y, pch=19, xlab='x', ylab='y')

#fit polynomial regression models up to degree 5
fit1 <- lm(y~x, data=df)
fit2 <- lm(y~poly(x,2,raw=TRUE), data=df)
fit3 <- lm(y~poly(x,3,raw=TRUE), data=df)
fit4 <- lm(y~poly(x,4,raw=TRUE), data=df)
fit5 <- lm(y~poly(x,5,raw=TRUE), data=df)

#create a scatterplot of x vs. y
plot(df$x, df$y, pch=19, xlab='x', ylab='y')

#define x-axis values
x_axis <- seq(1, 15, length=15)

#add curve of each model to plot
lines(x_axis, predict(fit1, data.frame(x=x_axis)), col='green')
lines(x_axis, predict(fit2, data.frame(x=x_axis)), col='red')
lines(x_axis, predict(fit3, data.frame(x=x_axis)), col='purple')
lines(x_axis, predict(fit4, data.frame(x=x_axis)), col='blue')
lines(x_axis, predict(fit5, data.frame(x=x_axis)), col='orange')
#calculated adjusted R-squared of each model
summary(fit1)$adj.r.squared
summary(fit2)$adj.r.squared
summary(fit3)$adj.r.squared
summary(fit4)$adj.r.squared
summary(fit5)$adj.r.squared

#create a scatterplot of x vs. y
plot(df$x, df$y, pch=19, xlab='x', ylab='y')

#define x-axis values
x_axis <- seq(1, 15, length=15)

#add curve of fourth-degree polynomial model
lines(x_axis, predict(fit4, data.frame(x=x_axis)), col='blue')
summary(fit4)

fit4$coefficients
names(fit4$coefficients[-1])

test <- names(fit4$coefficients[-1])[1]
as.numeric(substr(test,unlist(gregexpr(")", test))+1,nchar(test)))



fit4
equation <- iom.derivative.equation(fit4)
der <- iom.derivative.calculate(equation, c(1:16))

plot(der)

