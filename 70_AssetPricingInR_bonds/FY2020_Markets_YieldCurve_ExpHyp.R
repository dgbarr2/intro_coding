#
# FY2020_Markets_YieldCurve_ExpsHyp.R
#
# An introduction to the Expectations Hypothesis of the yield curve.
# 
# DGB May 2020.
#


rm(list = ls())
# setwd("C:/Users/[your number]/Documents/FY_Rcourse/myCodeAndData")

# Expected short rates as drivers of the yield curve.


yields_eh <- function(shortRates){
  ytilde <- vector(length=length(shortRates))
  logShortRates = log(1+shortRates)
  ytilde[1] <- logShortRates[1]
  for(i in 2:length(shortRates)){
    ytilde[i] = mean(logShortRates[1:i])
  }
  y <- exp(ytilde) - 1
  return(y)
}

expected_r_base <- c(rep(5,25))

expected_r_tighten_now <- c(rep(5,25))
expected_r_tighten_now[1] <- 8

expected_r_tighten_fut <- c(rep(5,25))
expected_r_tighten_fut[3] <- 8

expected_r_loosen_prog <- c(rep(5,25))
expected_r_loosen_prog[1:5] <- c(4,3,2,3,4)


mats = seq(1,25,1)

sim_yields_1 <- data.frame("base" = yields_eh(expected_r_base),
                           "tighten_now" = yields_eh(expected_r_tighten_now),
                           "tighten_fut" = yields_eh(expected_r_tighten_fut),
                           "loosen_prog" = yields_eh(expected_r_loosen_prog),
                           "Mat" = mats)

ggplot(sim_yields_1, aes(Mat)) + 
  coord_cartesian(xlim =c(0, 35), ylim = c(0, 10)) +
  geom_line(aes(y=base, col="Base"), size=1.2) +
  geom_line(aes(y=tighten_now, col="Tighten now"), size=1.2) +
  geom_line(aes(y=tighten_fut, col="Tighten future"), size=1.2) +
  geom_line(aes(y=loosen_prog, col="Loosen"), size=1.2) +
  scale_color_manual(values=c('red','blue','pink', 'green')) +
  theme(legend.position=c(.9,.75)) +
  labs(title="UK simulated yield curves", x="Maturity", y="% p.a.", colour="Key") 

