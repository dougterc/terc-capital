#moving average calculator
#takes input file, calculates moving average of investment

import math
import fileinput


#calculates moving average
def calculator(array, periods):
    i = 0
    sum = 0
    while i in range(0, len(array)):
        sum += int(array[i])
        i += 1
    x = float((sum/int(periods)))
    return x

# assumes that first line is equity & date, third is number of moving average periods, and the rest is returns
file = open("C:/Users/adria/OneDrive - High Point University/Documents/Coding Files/Personal Projects/terc-capital/terc-capital-azeni/tech/python/Volatility-Options Calculator/input-file.md", 'r')
lines = file.read().splitlines()
equity_name = lines[0]
length = lines[2]
closing_price = lines[3:len(lines)]

x = calculator(closing_price, length)

print("Equity name:", equity_name)
print("Time periods for average:", length)
print("Moving average:", x)

