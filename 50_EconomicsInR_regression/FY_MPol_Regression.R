#
# FY_MPol_Regression.R
#
# OLS regessions using US monetary data
#
# DGB Apr 2020
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

rm(list = ls())  # This line clears R's memory of data so that we start from a clean slate for this program with nothing hanging around from any programs we ran earlier.

#
# Import the data into a data frame, which we will call mpdata.
#

mpdata = read.csv("FY_MPol_Data.csv", header=TRUE)

mpdata[["date"]] <- as.Date(mpdata[["date"]])  # Tell R to treat the 'date' column as dates and not character strings.


#
# Graph the data
#

library(ggplot2)
ggplot(mpdata, aes(date)) + geom_line(aes(y=m1),colour="red") + geom_line(aes(y=p*10),colour="blue") + geom_line(aes(y=m3),colour="green") 


#
# Estimate a very simple 'monetarist' equation: P = k*M
#

mod1 <- lm(p~m1, data = mpdata)
summary(mod1)


# --- Add another variable (y)
mod2 <- lm(p~m1 + y, data = mpdata)
summary(mod2)


# --- Restrict the sample period
mpdata_2 <- subset(mpdata, date > "1960-01-01" & date < "1985-01-01")
mod3 <- lm(p~m1 + y+I(m1/y), data = mpdata_2)
summary(mod3)


# --- Use the ratio of m and y
mod4 <- lm(p~I(m1/y), data = mpdata_2)
summary(mod4)



# --------------------- end of file --------------------


