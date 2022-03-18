#
# FY2020_Markets_Bonds_PricesAndYields.R
#
# Introduction to bond price and yield calculations.
#
# DGB June 2020
#

rm(list = ls()) 

library(ggplot2)
setwd("C:/Users/[your number]/Documents/FY_Rcourse/myCodeAndData")

#
# General investment rates of return.
#

# Repeated investments at a constant interest rate.

investment <- 100
numberOfYears <- 5
interestRate <- 5

compoundReturnFixedRate <- function(I,n,r) {
  return(I * (1+r/100)^n)
}

cat("Investing", investment, "at ", interestRate,"% for ", numberOfYears, 
    "years produces ", compoundReturnFixedRate(investment,numberOfYears,interestRate))


# Repeated investments with changing rates.

investment <- 100
interestRates <- c(11,3,7,1,3)

compoundReturnChangingRates <- function(I,r) {
  for (t in 1:length(r)){
    I <- I * (1+r[t]/100)
  }
  return(I)
}

# Compare 3 investments.

interestRates_a <- c(5,5,5,5,5)
I_tn_a <- compoundReturnChangingRates(investment,interestRates_a)
I_tn_a
# N.B. We cannot use I_t+n as a variable name, so we use I_tn instead.

interestRates_b <- c(1,3,5,7,9,2)
I_tn_b <- compoundReturnChangingRates(investment,interestRates_b)
I_tn_b

interestRates_c <- c(11,3,7,1,3)
I_tn_c <- compoundReturnChangingRates(investment,interestRates_c)
I_tn_c

# Investments over differing periods.
interestRates_d <- c(1,3,5,7,9,2)
I_tn_d <- compoundReturnChangingRates(investment,interestRates_d)
I_tn_d

interestRates_e <- c(11,3,7,1,3)
I_tn_e <- compoundReturnChangingRates(investment,interestRates_e)
I_tn_e


# Calculate the internal rate of return (irr)

irr <- function(I_t,I_tn,n){
  return(100 * ((I_tn / I_t)^(1/n) - 1))
}

irr(investment, I_tn_d ,6)
irr(investment, I_tn_e ,5)


#
# Zero-coupon bonds
#

# Find the irr on a 5-year zero-coupon bond.
irr(90, 100 ,2)

# Rewrite irr() as a bond yield function.

y_zc <- function(p,red,n){
  return(100 * ((red / p)^(1/n) - 1))
}

y_zc(90,100,2)

