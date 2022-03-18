#
# FY2020_MacroPru_RiskMeasurement.R
#
# Load stock price data and analyse the following:
#    var, cov, corr, empirical and fitted (parametric) distributions
#    and Value at Risk (VaR)

# DGB May 2020
#

#
# Data are from Yahoo Finance Historical Data.
# E.g. https://finance.yahoo.com/quote/AAPL/history?period1=345427200&period2=1589932800&interval=1d&filter=history&frequency=1d
#

rm(list = ls())  # This line clears R's memory of data so that we start from a clean slate
#    for this program with nothing hanging around from any programs we ran earlier.



library(ggplot2)

# ------------------------ Load the data: ----------------------

# SAMPLE is 2004-08-19 to 2020-05-19 for all series.
#     This is the longest common sample from Yahoo.
#     All csv files have 7 columns of data including Date. 
#     All prices ignore dividend payments.
sp500 <- read.csv("GSPC.csv", header=TRUE)
nasdaq <- read.csv("NDAQ.csv", header=TRUE)
apple <- read.csv("AAPL.csv", header=TRUE)
google <- read.csv("GOOG.csv", header=TRUE)
ford <- read.csv("FORD.csv", header=TRUE)
lmt <- read.csv("LMT.csv", header=TRUE)
boa <- read.csv("BAC.csv", header=TRUE)
citi <- read.csv("C.csv", header=TRUE)
jpm <- read.csv("JPM.csv", header=TRUE)
wfargo <- read.csv("WFC.csv", header=TRUE)

head(apple)

# Construct a dataframe with just the closing prices for all firms

stockPrices <- data.frame(
"Date"= sp500$Date, 
"sp500" = sp500$Close,
"nasdaq" = nasdaq$Close,
"ford" = ford$Close,
"apple" = apple$Close,
"google" = google$Close,
"lmt" = lmt$Close,
"boa" = boa$Close,
"citi" = citi$Close,
"jpm" = jpm$Close,
"wfargo" = wfargo$Close)
head(stockPrices)
tail(stockPrices )

#  ---------------------- Create user defined functions. ---------------- 

# A simple example.
myDivisionFunction <- function(n,d){
  r <- n/d
  return(r)
}

myDivisionFunction(100,2)

myV1 <- 9
myV2 <- 3

myResult <- myDivisionFunction(myV1,myV2)
myResult

n
d
r



#  Create a function to construct daily returns i.e. daily capital gains/losses.

makeReturns <- function(p){
  n <- length(p)
  r <- c(rep(12345,n))
  for(t in 2:n){
      r[t] <- 1 * (p[t] - p[t-1]) / p[t-1]  
    # Change 1 to 360 to get annualised returns. 
    # DGB to include the following lines for the video to
    #     demonstrate what is going on...
    # r[t] <- (p[t] - p[t-1]) / p[t-1]
    # r[t] <- p[t] - p[t-1]
    # r[t] <- p[t]
    # r[t] <- 6
  }
  return(r[2:n])
}

apple_returns_illus <- makeReturns(stockPrices$apple)
google_returns_illus <- makeReturns(stockPrices$google)
head(apple_returns_illus)


# Create a function to draw a histogram with a fitted Normal pdf.
drawHistogram <- function(r,firmName){
  r_mean <- mean(r)
  r_sd <- sd(r)
  hist(r, breaks=1000, freq=FALSE, 
       main=c("Histogram of daily stock returns:", firmName))
  curve(dnorm(x, mean=r_mean, sd=r_sd), add=TRUE, col="red", lwd=2)
  # Nothing to return().
}

drawHistogram(apple_returns_illus,"Apple")


# ------------------------ Create returns from the price data. --------------

# Construct dataframes of ex-post stock returns.
Dates <- sp500$Date[2:nrow(sp500)]  # This will be included in all the returns dataframes.
r_sp500 <- data.frame("Date" = Dates, "Return" = makeReturns(sp500$Close))
r_nasdaq <- data.frame("Date" = Dates, "Return" = makeReturns(nasdaq$Close))
r_apple <- data.frame("Date" = Dates, "Return" = makeReturns(apple$Close))
r_google <- data.frame("Date" = Dates, "Return" = makeReturns(google$Close))
r_ford <- data.frame("Date" = Dates, "Return" = makeReturns(ford$Close))
r_lmt <- data.frame("Date" = Dates, "Return" = makeReturns(lmt$Close))
r_boa <- data.frame("Date" = Dates, "Return" = makeReturns(boa$Close))
r_citi <- data.frame("Date" = Dates, "Return" = makeReturns(citi$Close))
r_jpm <- data.frame("Date" = Dates, "Return" = makeReturns(jpm$Close))
r_wfargo <- data.frame("Date" = Dates, "Return" = makeReturns(wfargo$Close))

# Put all the returns into a single dataframe.
stockReturns <- data.frame("Date" = r_sp500$Date,
                          "sp500" = r_sp500$Return,
                          "nasdaq" = r_nasdaq$Return,
                          "apple" = r_apple$Return,
                          "google" = r_google$Return,
                          "ford" = r_ford$Return,
                          "lmt" = r_lmt$Return,
                          "boa" = r_boa$Return,
                          "citi" = r_citi$Return,
                          "jpm" = r_jpm$Return,
                          "wfargo" = r_wfargo$Return)

# Tell R to recognise the Date column as dates.
stockReturns[["Date"]] <- as.Date(stockReturns[["Date"]])
head(stockReturns)

# Save the dataframe for another day, so that we 
#    don't have to load and process the all the 
#    individual series if we want to use them
#    in another program. You can call it whatever you like
#    inside the inverted commas (remember the name though).
# It will be save to whatever working directory
#    you set for this program.

saveRDS(stockReturns,file="stockReturnsDataframe.Rda")

# Don't forget to turn this line into a comment 
#  after you have run it (we don't want to save
#  the dataframe over and over again.)



head(stockReturns)
tail(stockReturns)

# --------------------- Create some graphs. ------------------------

# Plot a single firm's returns.
ggplot(stockReturns, aes(Date)) +
         geom_line(aes(y=apple),colour="red",lwd=.1) 
ggplot(stockReturns, aes(Date)) + 
         geom_line(aes(y=boa),colour="darkgreen",lwd=.1)
# Have a look at Ford's returns...very odd! 


# Plot all of the series on a single graph without having to list them
#      in the ggplot command.

install.packages("reshape2") # Comment this out after running it.
library(reshape2)

stockReturnsMelt <- melt(stockReturns, id.vars="Date",
                          variable.name="series")
# This (melt()) stacks all the returns into a single vector, 
#     which makes the ggplot command more compact. 
head(stockReturnsMelt)


ggplot(stockReturnsMelt, aes(Date,value)) + 
                              geom_line(aes(colour = series), lwd=.1)
ggplot(stockReturnsMelt, aes(Date,value)) + 
                              geom_line(lwd=0.1) + 
                              facet_grid(series ~ .)
# Which firms showed the greatest volatility during the 2007-09 Crisis?



# --------------- Calculate means, variances and covariances. ------------
         
mean(stockReturns$jpm) # Mean return of a single firm.
mean(stockReturns$citi)
     
max(stockReturns$jpm)  # Max return of a single firm.
min(stockReturns$jpm)  # Min return of a single firm.

# Construct the covariance matrix of the whole set of firms.
varcov <- var(stockReturns[c("sp500","nasdaq","apple","google")])
#varcov <- var(stockReturns)
varcov
diag(varcov)  # This pulls out the diagonal of the matrix i.e. the variances.
varcov[2,3]


# Construct the correlation matrix of the whole set of firms.
corrns <- cor(stockReturns[c("sp500","nasdaq","apple","google")])
corrns
diag(corrns)  # This pulls out the diagonal of the matrix i.e. a load of 1s.

var(stockReturns$apple)
var(stockReturns$apple,stockReturns$google)
sd(stockReturns$apple)

# -------------- Draw some histograms (these are not bar charts!) ----------
drawHistogram(stockReturns$jpm,"Apple")
drawHistogram(stockReturns$jpm,"JP Morgan Chase")
drawHistogram(stockReturns$sp500,"SP500")
drawHistogram(stockReturns$citi,"Citicorp")
drawHistogram(stockReturns$wfargo,"Wells Fargo")


# --------------------------- Value at Risk ---------------------------


#  Parametric VaR
qNormExample_1 = qnorm(.025,0,1)
qNormExample_1

VaR_rate_parametric = qnorm(.05,mean(stockReturns$apple),
                            sd(stockReturns$apple))
# 5% of this Normal pdf is below qnorm(0.05,...). 
VaR_rate_parametric

# Choose an illustrative portfolio size.
portfolioSizeInDollars <- 1000000
VaR_dollars_parametric <- portfolioSizeInDollars *
                             VaR_rate_parametric
VaR_dollars_parametric
# With a portfolio of portfolioSizeInDollars we have a 5% chance of
#  losing more than VaR_dollars_para according to
#  the parametric VaR measure.


#   Empirical VaR

# ----------- In order to calculate an 'empirical VaR' we -------------
# -----------    sort the data in ascending order.        -------------          

# Illustrate this with sp500 before we use it for VaR calculations.
ascendingReturnSeries <- sort(stockReturns$apple)
head(ascendingReturnSeries)

cutoffObs <- ceiling(0.05 * length(ascendingReturnSeries))
cutoffObs
VaR_rate_emp <- ascendingReturnSeries[cutoffObs]
VaR_rate_emp

VaR_dollars_emp <- portfolioSizeInDollars * VaR_rate_emp
VaR_dollars_emp
# With a portfolio portfolioSizeInDollars we have a 5% chance of
#  losing more than VaR_dollars_emp according to
#  the parametric VaR measure.

# Print the VaR results.
cat(" parametric VaR = ", VaR_dollars_parametric, "\n",
    "empirical VaR = ", VaR_dollars_emp, "\n")


# ------------------ end of file -------------------

