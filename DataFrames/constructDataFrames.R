#
# dataFrames_1.R
#
# Create a data frame.
#
# DGB Jan 2020
#

rm(list = ls())



#
# Create vectors of names and data

bank <- c("UK", "Japan", "EU", "Australia")
bank

rate <- c(0.75,-0.1,-0.5,0.75)
cat("Bank rates are: ", rate, "\n")

inflation <- c(1.3,0.5,1.3,1.7)  # From tradingeconomics.com, most recent data as at 15 Jan 2020.
inflation

#
# Create a data.frame with column names set by default to the vector names
#

monPol_1 <- data.frame(bank,rate,inflation,stringsAsFactors=FALSE)

monPol_1

#
# Choose the columm names yourself
#

monPol_2 <- data.frame(Bank=bank,Policy_rate=rate,Inflation_rate=inflation,stringsAsFactors=FALSE)

monPol_2


#
# Add a column of row names to replace the number we obtained above
#

rownames(monPol_1) <- c("UK_rn","Japan_rn","EU_rn","Australia_rn")
monPol_1














