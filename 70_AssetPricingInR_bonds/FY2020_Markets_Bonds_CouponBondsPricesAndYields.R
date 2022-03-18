#
# FY2020_Markets_Bonds_CouponBondsPricesAndYields.R
#
# Introduction to coupon-bond price and yield calculations.
#
# DGB June 2020
#
rm(list = ls()) 

library(ggplot2)
setwd("C:/Users/[your number]/Documents/FY_Rcourse/myCodeAndData")

#
# Bonds
#

# Find the yield on a 5-year zero-coupon bond.
irr(90, 100 ,2)


# Find the yield on a 2-year coupon bond.

yield_2yr <- function(Price,Coupon){
  a <- Price
  b <- -Coupon
  c <- -(100 + Coupon)
  
  x_1 <- (-b + sqrt(b^2 - 4*a*c) ) / (2 * a) 
  x_2 <- (-b - sqrt(b^2 - 4*a*c) ) / (2 * a) 
  y <- 100 * (x_1 - 1)
  return(y)
}
yield_2yr(100,7)

yield_2yr(85,0)

# Choosing the "wrong" solution.
yield_2yr_wrong <- function(Price,Coupon){
  a <- Price
  b <- -Coupon
  c <- -(100 + Coupon)
  
  x_1 <- (-b + sqrt(b^2 - 4*a*c) ) / (2 * a) 
  x_2 <- (-b - sqrt(b^2 - 4*a*c) ) / (2 * a) 
  y <- 100 * (x_2 - 1)
  return(y)
}
yield_2yr_wrong(100,7)

ADD LONG MAT BONDS