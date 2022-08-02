import math
import fileinput

# datafile - using this source: https://www.wsj.com/market-data/quotes/index/SPX/historical-prices
# for documentation see volatility-calc-documentation.md

def daily_return(close, previous_close):
    return math.log(close / previous_close)

def mean(array, n):
    sum = 0
    final = 0
    i = 0

    for i in range(0, len(array)): 
        sum += array[i]

    final = sum / n

    return final

def deviations(array, n, mean):
    dev = []
    i = 0

    for i in range(0, (n - 1)):
        dev.append((array[i] - mean))
        i += 1

    return dev

def variances(deviations, n):
    s = 0
    i = 0
    temp = 0

    for i in range(0, (n - 1)):
        temp += pow(deviations[i], 2)
        i += 1

    s = temp / (n - 1)
    return s

def volatility(variance):
    temp = math.sqrt(variance)
    return temp

f = open("C:/Users/adria/OneDrive - High Point University/Documents/Coding Files/Personal Projects/terc-capital/terc-capital-azeni/tech/python/Volatility-Options Calculator/input-file.md", 'r')
lines = f.read().splitlines()
equity_name = lines[0]
#equity_name = equity_name.rstrip(equity_name[-1])
length = lines[1]
#length = int(length.rstrip(length[-1]))
periods = int(lines[2])
#periods = int(periods.rstrip(periods[-1]))
closing_price = lines[3:len(lines)]
print(closing_price)

something = []
for x in range(3, (periods - 1)):
    something.append(daily_return(int(closing_price[(x+1)]), int(closing_price[(x)])))

periods -= 4

value_storage = (mean(something, periods))


megamind = []
megamind = (deviations(something, periods, value_storage))

almost_done = (variances(megamind, periods))

final = volatility(almost_done)

print("\nEquity Name:")
print(equity_name)

print("\nTimeframe:")
print(length)

print("\nPeriods:")
print(periods)

print("\nVolatility:")
print(final)
print("\n")
