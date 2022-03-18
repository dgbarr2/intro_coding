#
# FY2020_Markets_YieldCurve_Data.R
#
# Load yield and price. Plot yield curves.
#
# DGB June 2020
#

rm(list = ls())  

library(ggplot2)

setwd("/Users/david/DocumentsCopy/Documents/Work/BoE/FYandCBQ/FY2019_20/FY2020_Rcourse/Rdoodling")

# Load yield data for end-year 2006 to 2019, plus 20 Jan 2020.
# Original data are from https://www.bankofengland.co.uk/statistics/yield-curves
# Maturities are at 6-monthly intervals, and run from 0.5 to 25 years up to 2015 and
# up to 40 years thereafter.
# 

library(ggplot2)

#
# UK bond price and yield data.
#

priceYieldData <- read.csv("DMO_edited - Gilt Reference Prices2017.csv")

# Deal with the dates.
priceYieldData[["RedemptionDate"]] <- as.Date(priceYieldData[["RedemptionDate"]])


# Plot yields
ggplot(priceYieldData,aes(RedemptionDate)) +
  geom_point(aes(y=Yield),color="blue",size=4)

# Plot prices
ggplot(priceYieldData,aes(RedemptionDate)) +
  geom_point(aes(y=CleanPrice),color="red",size=4)


#
# Plot 5 Bank of England yield curves.
#


yieldData <- 
  read.csv("/Users/david/DocumentsCopy/Documents/Work/BoE/FYandCBQ/FY2019_20/FY2020_Rcourse/Data/UK_SpotCurve_Annual_Dec2016-2019_25yrs.csv",
           header=TRUE)
head(yieldData)

ggplot(yieldData,aes(Years)) +
      geom_line(aes(y=X31Dec06), colour="blue", size=1.2) +
      geom_line(aes(y=X31Dec08), colour="purple", size=1.2) + 
      geom_line(aes(y=X31Dec18), colour="red", size=1.2) + 
      geom_line(aes(y=X29Jan20), colour="darkgreen", size=1.2)+ 
      geom_line(aes(y=X22Jun20), colour="pink", size=1.2)

ggplot(yieldData, aes(Years)) +
  coord_cartesian(xlim =c(0, 30), ylim = c(0, 6)) +
  geom_line(aes(y=X31Dec06, col="2006"), size=1.2) +
  geom_line(aes(y=X31Dec08, col="2008"), size=1.2) + 
  geom_line(aes(y=X31Dec18, col="2018"), size=1.2) + 
  geom_line(aes(y=X29Jan20, col="Jan2020"), size=1.2) +
  geom_line(aes(y=X22Jun20, col="Jun2020"), size=1.2) +
  scale_color_manual(values=c('blue','purple', 'red', 'darkgreen','pink')) +
  theme(legend.position=c(0.90,.6)) +
  labs(title="UK yield curves", x="Maturity", y="% p.a.", colour="Key")


extractYields <- function(yields){
  nYears <- length(yields) / 2
  extractedYields <- vector(length=nYears)
  for(i in 1:nYears) {
    extractedYields[i] <- yields[2*i]
  }
  return(extractedYields)
}

