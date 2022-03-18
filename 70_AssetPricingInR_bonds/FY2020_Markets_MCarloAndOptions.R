#
# FY2020_Markets_MCarloAndOptions.R
#
# Introduce Monte Carlo simulation using coin flip examples.
# Adapt the coin code to simulate the Black-Scholes European call result.
#
# June 2020
#

rm(list = ls())
library(ggplot2)
setwd("C:/Users/[your number]/Documents/FY_Rcourse/myCodeAndData")



singlePath <- function(nFlips, headsWins, tailsWins, probHeads){
  w_length = nFlips+1
  w <- vector(length=w_length)
  w[1] <- 0
  for(i in 2:w_length){
    trial <- runif(1)
    if(trial > 1 - probHeads){
      e <- headsWins
    } else {
      e <- tailsWins
    }
    w[i] = w[i-1] + e
  }
  return(w)
}

# Plot 5 flip results

numberOfFlips <- 5
pathData <- data.frame("flips" = seq(0,numberOfFlips,1),
                       "paths" = singlePath(numberOfFlips,0.5,-0.5,0.5))

ggplot(pathData, aes(flips)) +
  geom_line(aes(y=paths, col="Path"), size=2) +
  labs(title="Single path", x="n", y="£")               
                       
# Plot 1000 flip results

numberOfFlips <- 1000
pathData <- data.frame("flips" = seq(0,numberOfFlips,1),
                       "paths" = singlePath(numberOfFlips,0.5,-0.5,0.5))

ggplot(pathData, aes(flips)) +
  geom_line(aes(y=paths, col="Path"), size=2) +
  labs(title="Single path", x="n", y="£")  





# Create a function to generate and record multiple path final values.

payoff_distribution <- function(nPaths, nFlips, headsWins, tailsWins, probHeads) {
  w <- vector(length=nPaths)
  for(i in 1:nPaths){
    w[i] <- singlePath(nFlips, headsWins, tailsWins, probHeads)[nFlips+1]
  }
  return(w)
}

finalWealthDist <- payoff_distribution(50000,50,0.5,-0.5,0.5)
hist(finalWealthDist, breaks=seq(-51.5,51.5,1), freq=TRUE,  col="cornflowerblue",
     main=c("Frequency chart of final winnings."))

hist(finalWealthDist, breaks=seq(-51.5,51.5,1), freq=FALSE,  col="cornflowerblue",
     main=c("Density chart of final winnings."))





mu <- 50 * (0.5*1 + 0.5*(-1))
sigma <- sqrt(50*((0.5*0.5^2)+(0.5*(-0.5)^2))) 
curve(dnorm(x, mean=mu, sd=sigma), add=TRUE, col="red", lwd=2)

# Check the simulated mean and s.d. against the theoretical ones.
mu_sim <- mean(finalWealthDist)
sigma_sim <- sqrt(var(finalWealthDist))
c(mu,sigma)
c(mu_sim,sigma_sim)

# Valuing the game

cat("The value of the game to a risk-neutral player is estimated to be:",mean(finalWealthDist))

#
# Option valuation
# 
K <- 5
underlying_payoffs <- payoff_distribution(50000,50,0.5,-0.5,0.5)
net_payoffs <- underlying_payoffs - K
option_payoffs <- replace(net_payoffs,net_payoffs <= 0,0)
cat("Call option price with strike = ", K, ", is ",mean(option_payoffs))
option_payoffs <- replace(net_payoffs,net_payoffs <= 0,NA)
hist(option_payoffs, breaks=seq(-51.5,51.5,1), freq=TRUE,  col="cornflowerblue",
     main=c("Frequency chart of option payoffs."))


# Pricing a knock-out option.

singlePath_barrier <- function(nFlips, headsWins, tailsWins, probHeads,B){
  w_length = nFlips+1
  w <- vector(length=w_length)
  w[1] <- 0
  for(i in 2:w_length){
    trial <- runif(1)
    if(trial > 1 - probHeads){
      e <- headsWins
    } else {
      e <- tailsWins
    }
    if(w[i-1] > B){
      w[i] = w[i-1] + e
    } else {
      w[i] = w[i-1]
    }
  }
  return(w)
}
# Set the barrier value.
barrierValue <- -2

# Draw a single path with the possibility of hitting the barrier.

barrierPath <- singlePath_barrier(100,0.5,-0.5,0.5,barrierValue)
pathData_barrier <- data.frame("flips"=seq(0,100,1), "Path" = barrierPath)
ggplot(pathData_barrier, aes(flips)) +
  geom_line(aes(y=Path, col="Path"), size=2) +
  labs(title="Single knock-out path", x="n", y="£")  


# Create a function to generate a simulated distribution of path (wealth) end-values.
payoff_distribution_barrier <- function(nPaths, nFlips, headsWins, tailsWins, probHeads,barrier) {
  w <- vector(length=nPaths)
  for(i in 1:nPaths){
    w[i] <- singlePath_barrier(nFlips, headsWins, tailsWins, probHeads,barrier)[nFlips+1]
  }
  return(w)
}

# Create a simulated distribution of path (wealth) end-values.
finalWealthDistribution_barrier <- payoff_distribution_barrier(50000,50,0.5,-0.5,0.5,barrierValue)


# Price a barrier knock-out option with strike K
K = 5
netPayoffs_barrier <- finalWealthDistribution_barrier - K
optionPayoffs_barrier <- replace(netPayoffs_barrier,
                                 netPayoffs_barrier <= 0,0)
cat("Simulation price of the knock-out option is", mean(optionPayoffs_barrier))



# Find the probability of a knock-out. 
n_knockOuts <- length(finalWealthDistribution_barrier[finalWealthDistribution_barrier <= barrierValue])
cat("Probability of a knock-out = ", n_knockOuts/50000)


#
# Reinterpret the model in terms of cash withdrawals from a bank.
#

# Bank cash 
cashHoldings <- 2
finalCashPosition <- payoff_distribution_barrier(50000,30,0.5,-0.5,0.5,-cashHoldings)

n_illiquids <- length(finalCashPosition[finalCashPosition <= -cashHoldings])
cat("Probability of a running out of cash = ", n_illiquids/50000)


#
# -------------------- Steps towards Black-Scholes -------------------
# -------------------- Using NEW NOTATION from this point ------------ 


# Start with the original singlePath() function with new notation.
singlePath_bs_1 <- function(S_start, nDays, S_up, S_down, probS_up){
  S_length = nDays+1
  S <- vector(length=S_length)
  S[1] <- S_start
  for(i in 2:S_length){
    trial <- runif(1)
    if(trial > 1 - probS_up){
      u <- S_up
    } else {
      u <- S_down
    }
    S[i] = S[i-1] + u
  }
  return(S)
}

# Now rule out negative values of S by using an exponential random variable.
singlePath_bs_2 <- function(S_start, nDays, S_up, S_down, probS_up){
  S_length = nDays+1
  S <- vector(length=S_length)
  S[1] <- S_start
  for(i in 2:S_length){
    trial <- runif(1)
    if(trial > 1 - probS_up){
      u <- S_up
    } else {
      u <- S_down
    }
    S[i] = exp(u)*S[i-1]   # This is new.
  }
  return(S)
}



# Adding some upward drift to S
singlePath_bs_3 <- function(S_start, nDays, S_up, S_down, probS_up,drift){
  S_length = nDays+1
  S <- vector(length=S_length)
  S[1] <- S_start
  for(i in 2:S_length){
    trial <- runif(1)
    if(trial > 1 - probS_up){
      u <- S_up
    } else {
      u <- S_down
    }
    S[i] = exp(drift) * exp(u)*S[i-1]  # This is new
  }
  return(S)
}



# Replacing the coin flips as the source of randomness.
singlePath_bs_4 <- function(S_start,nDays,drift){   # This is new
  S_length = nDays+1
  S <- vector(length=S_length)
  S[1] <- S_start
  set.seed(5)
  u <- rnorm(ndays+1) 
  for(i in 2:S_length){
    u <- rnorm(1)                         # ...so is this...
    S[i] = exp(drift) * exp(u[i])*S[i-1]  # ...and this.
  }
  return(S)
}


# Taking control of the volatility by adding v.
singlePath_bs_5 <- function(S_start,nDays,drift,v){
  S_length = nDays+1
  S <- vector(length=S_length)
  S <- S_start 
  for(i in 2:S_length){
    u <- rnorm(1)
    S = exp(drift) * exp(v*u)*S # This is new.
  }
  return(S)
}

# Function to create the paths' final values using singlePath_bs_5.

terminal_values <- function(nPaths, S_start, nDays,drift,v) {
  S <- vector(length=nPaths)
  for(i in 1:nPaths){
    S[i] <- singlePath_bs_5(S_start, nDays,drift,v)
  }
  return(S)
}


#
# Now price an option.
#


K <- 105
nDays <- 365
S_0 <- 100
mu <- 0.06
sigma <- 0.3
drift <- (mu-sigma^2/2)/365  
volatility <- sigma/sqrt(365)
nPaths <- 100000


terminal_S_values <- terminal_values(nPaths,S_0,nDays,drift,volatility)
upper <- floor(max(terminal_S_values)+5)
hist(terminal_S_values,  breaks=seq(0,upper,1), freq=FALSE,  col="cornflowerblue",
     main=c("Density chart of terminal S values."))
net_S_values <- terminal_S_values - K
option_payoffs <- replace(net_S_values,net_S_values < 0,0)
optionPrice <- exp(-mu)*mean(option_payoffs)

cat("Call option price with strike = ", K, ", is ",optionPrice)


#
# Black-Scholes price equation
#

BlackScholes_formula <- function(S_0,r,K,sigma,expiry) {
  T <- expiry
  d1 = (1/(sigma*sqrt(T))) * (log(S_0/K) + (r + (sigma^2)/2)*T)
  d2 = d1 - sigma * sqrt(T)
  
  C_0 = pnorm(d1,0,1)*S_0 - pnorm(d2,0,1) *  K * exp(-r*T)
  return(C_0)
}

# Set the expiry date (relative to time 0, in years) for the option
expiry <- 1

C_BSformula <- BlackScholes_formula(S_0,mu,K,sigma,expiry)

cat("Black-Scholes price = ",C_BSformula,"\n")

