program: volatility-calculator.py
author - adrian zeni

description:
this program will ask the user for either the filename of the input file (if it is in the same
directory as the python file), or for the full directory path. then, it will determine the volatility of that given equity, and will return the equity name, timeframe, periods, and volatility back to the user.

to add:
ask the user for the filename/directory
make it so that the 
    first row is equity name, 
    second is timeframe,
    third is periods


calculating volatility using historical returns
reference - https://www.wikihow.com/Calculate-Historical-Stock-Volatility

daily_return(close, previous_close):
R = ln( C(n) / (C(n-1) ))
or
return = ln(closing price / closing price of the previous day)
in short, this will return the daily return
input parameters: close[n], close[n-1], periods

mean(array, n):
m = (R1 + R2 + R3 + ... + Rn) / n
or 
mean return = (return of time period 1 + time period 2 ...) / number of returns
this finds the average return
input parameters: array of returns, number of time periods n

deviations(array, n, mean)
Dn = (Rn - m)
or 
deviation = (return on a given day - average return)
this finds the deviation (or better, difference) of the daily return from the average return.
deviation is Dn because this calculation happens as many times as there are days. D1, D2, D3, etc.
input parameters: array of returns, number of time periods n, mean

calculating variance:
S = (d1^2 + d2^2 + ... + Dn^2) / (n-1)
or
variance = (deviation of day 1 squared + deviation day 2 squared) / (number of days - 1)
remember squaring the deviance will make it smaller, since squaring a decimal makes it smaller
the variance is the measurement of spread of data, or how far each number is from the average
reference: https://www.investopedia.com/terms/v/variance.asp

calculating volatility:
V = sqrt(S)
or 
volatility is the square root of variance
note that this is also called standard deviation

calculating volatility:
V = sqrt(S)
or 
volatility is the square root of variance
note that this is also called standard deviation


#datafile - using this source: https://www.wsj.com/market-data/quotes/index/SPX/historical-prices, as of 7/15/22

















