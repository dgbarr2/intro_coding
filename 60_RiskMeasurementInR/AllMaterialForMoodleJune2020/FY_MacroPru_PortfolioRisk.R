#
# FY2020_MacroPru_PortfolioRisk.R
#
# Short program to create the classic mean-sd graph
#   for portfolios of stocks.
# 
# DGB May 2020.
#


rm(list = ls())  # This line clears R's memory of data so that we start from a clean slate
#    for this program with nothing hanging around from any programs we ran earlier.


# Load the data: We use the dataframe that we saved in the 
#  Risk Measurement session. We can call it whatever we like
#  on the left of the arrow. Here we use the same name
#  that it had in the earlier program.

stockReturns <- readRDS("stockReturnsDataframe.Rda")



# Create a function to calculate the mean and s.d. of returns
#   for a 2-asset portfolio. The proportion invested in the first
#   asset is w1. This would usually be done using matrices but the 
#   code below is easier to follow. (Matrices would make it easier 
#   to expand to more than 2 assets.)
portfolioMoments <- function(r1,r2,w1){
  mean_r1 <- mean(r1)
  mean_r2 <- mean(r2)
  Var_r1 <- var(r1)
  Var_r2 <- var(r2)
  Cov_r1r2 <- cov(r1,r2)
  w2 <- 1 - w1
  Mean_portfolio <- w1*mean_r1 + w2*mean_r2
  Var_portfolio <- w1*w1*Var_r1 + 2*w1*w2*Cov_r1r2 + w2*w2*Var_r2
  sd_portfolio <- sqrt(Var_portfolio)
  return(c(Mean_portfolio,sd_portfolio))
}

portfolioMoments(stockReturns$apple, stockReturns$google, 0.5)


# ------------------------ Portfolios -------------------------



# Non-matrix-based 2 asset portfolio analysis.

# Choose the 2 firms.
r1 <- stockReturns$nasdaq
r2 <- stockReturns$jpm


# Create a 2-asset dataframe without dates.
TwoStockReturns <- data.frame("r1" = r1,
                              "r2" = r2)


# Create an empty matrix which into which we will put the 
# mean and s.d. of 101 portfolios with different weights
# running from w1 = 0, to w1 = 1.
meanSDPortfolios <- matrix(nrow=101,ncol=2)
meanSDPortfolios

# Create the vector of weights.
w_i <- seq(0,1,0.01)
head(w_i)

# Use a for loop to fill the portfolio matrix
for(i in 1:length(w_i)){
  meanSDPortfolios[i,] <- portfolioMoments(r1,
                                           r2,w_i[i])
}
head(meanSDPortfolios)


# Put meanSDPortfolio into a dataframe so that we can use ggplot.
meanSDPortfolios_df <- data.frame("weight1" = w_i,
                                  "mu"=meanSDPortfolios[,1],
                                  "sigma" = meanSDPortfolios[,2])
head(meanSDPortfolios_df)
# Plot the results.
ggplot(meanSDPortfolios_df,aes(sigma)) + 
            geom_point(aes(y=mu),color="purple")


# ---------------- End of the session - some additional notes follow. ----



# Plot all firms' mean and sd

meanAll <- colMeans(stockReturns[,2:ncol(stockReturns)])
VarAll <- var(stockReturns[,2:ncol(stockReturns)])
sdAll <- sqrt(diag(VarAll))

momentsAll <- data.frame("muAll"= meanAll,"sigmaAll"= sdAll)

data.frame(meanAll,sdAll)

ggplot(momentsAll,aes(sigmaAll)) + geom_point(aes(y=muAll),color="red")


# ------------------ Illustrating the functions used in ---------------
# ------------- the user-defined-function 'portfolioMoments'. -----------

# var() will give us a covariance matrix if we give it more than 1 series.
# It provides just a varaince if we give it just 1 series.
varcov2 <- var(TwoStockReturns)
varcov2

# We can extract the moments from the resulting matrix as follows:
Var_r1 <- varcov2[1,1]
Cov_r1r2 <- varcov2[1,2]
Var_r2 <- varcov2[2,2]

# Print them as a vector.
c(Var_r1,Cov_r1r2,Var_r2)

# Calculate and print a portfolio variance. 
w1 <- 0.5
w2 <- 1 - w1
Var_Portfolio <- w1*w1*Var_r1 + 2*w1*w2*Cov_r1r2 + w2*w2*Var_r2
Var_Portfolio

# Alternatively we can calculate the moments one by one.
# We label them with a z to distinguish them from the ones above.
Var_r1z <- var(r1)
Var_r2z <- var(r2)
Cov_r1r2z <- cov(r1,r2)
c(Var_r1z,Cov_r1r2z,Var_r2z)

# Calculate the means of the returns.
mean_r1 <- mean(r1)
mean_r2 <- mean(r2)
c(mean_r1,mean_r2)

Mean_Portfolio <- w1 * mean_r1 + w2 * mean_r2
Mean_Portfolio


# ------------------ A note on using matrices to find mean and Var. ------------------

n_assets <- 2   # This programme is set up for only 2 assets.
w <- c(0.5, 0.5)  # Create a vector of 2 portfolio weights = 0.5
w

means <- colMeans(TwoStockReturns)
Mean_Portfolio_matrix <- w %*% means
Mean_Portfolio_matrix

Var_Portfolio_matrix <- w %*% varcov2 %*% w
Var_Portfolio_matrix

c(Mean_Portfolio_matrix, Var_Portfolio_matrix)
