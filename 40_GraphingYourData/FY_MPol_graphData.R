#
# FY_MPol_graphData.R
#
# Import data from a csv file and use it to draw some graphs with ggplot.
#
# DGB Mar 2020
#


#	We import the following US data series from the file FY_MPol_Data.csv:
#
#  Money supply (narrow) m1  
#  Money supply (broad) m3
#  Prices, p: consumer price index
#  Short rate(1), rs: 3-month T Bill rate
#  Short rate(2), rff: Fed Funds rate
#  Long rates, rl: 10-year interest rate (yield on 10-year gov bonds)
#  Output, y: Industrial production
#  Unemployment, u: Unemployment rate
#

rm(list = ls())  # This line clears R's memory of data so that we start from a clean slate
                 #    for this program with nothing hanging around from any programs we ran earlier.

#
# Import the data into a data frame, which we will call mpdata.
#

mpdata <- read.csv("FY_MPol_Data.csv", header=TRUE)


mpdata[1,]  # Print the first line of the data (plus the header) to the console.


head(mpdata)  # This prints the first 6 lines (plus the header).


tail(mpdata)  # And this prints the final 6 lines (plus the header).


# ------------------------------------------------------------ Plot (1) p and m1, (2) The 3 interest rates

# Load the package ggplot from the library of packages on this laptop.
library("ggplot2")

mpdata[["date"]] <- as.Date(mpdata[["date"]])  

# Tell R to treat the 'date' column as dates.
ggplot(mpdata, aes(date)) + geom_line(aes(y=m1),colour="red") + geom_line(aes(y=p*10),colour="blue")


ggplot(mpdata, aes(date)) + geom_line(aes(y=rs),colour="red") + geom_line(aes(y=rl),colour="blue") + geom_line(aes(y=rff),colour="green")


# Plot a sub period (see https://www.neonscience.org/dc-time-series-plot-ggplot-r):

startPlot <- as.Date("2011-01-01")
endPlot <- as.Date("2019-01-01")

plotDates <- c(startPlot, endPlot)
plotDates

ggplot(mpdata, aes(date)) + geom_line(aes(y=m1),colour="red") + geom_line(aes(y=p*10),colour="blue") +(scale_x_date(limits=plotDates))

