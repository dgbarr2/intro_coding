#
# FY2020_Markets_Yields_VarietiesAndPaths.R
#
# A couple of notes about convenient ways to work with yields.
# 
# DGB June 2020.
#


rm(list = ls())
# setwd("C:/Users/[your number]/Documents/FY_Rcourse/myCodeAndData")



# y and log(1+y) are almost equal if y is `small'.`
curve(x*1,from=-1,to=1,xname="x",lwd=4)
curve(log(1+x),from=-1,to=1,xname="x",add=TRUE,  col="red",lwd=4)
abline(v=0,col="blue")
abline(h=0,col="blue")


# Instantaneous rates, as opposed to annual rates.

finding_e <- function(max_n) {
  f <- vector(length = max_n)
  for (n in 1:max_n){
    f[n] <- (1 + 1/n)^n
  }
  return(f)
}




max_n <- 150

e_demo <- data.frame("n" = seq(1,max_n,1), "f" = finding_e(max_n))
ggplot(e_demo, aes(n)) + 
  coord_cartesian(ylim = c(2, 3)) +
  geom_line(aes(y=f, col="(1+1/n)^n"), size=.5,color="blue") +
  geom_hline(yintercept=2.718, linetype="dashed", color = "red") +
  labs(title="Converging on e", x="n", y="(1+1/n)^n")



finding_ePowers <- function(max_n,z) {
  f <- vector(length = max_n)
  for (n in 1:max_n){
    f[n] <- (1 + z/n)^n
  }
  return(f)
}

zValue = 0.5
ePowers_demo <- data.frame("n" = seq(1,n,1), "f" = finding_ePowers(n,zValue))
ggplot(ePowers_demo, aes(n)) + 
  coord_cartesian(ylim = c(1, 2)) +
  geom_line(aes(y=f, col="(1+r/n)^n"), size=1, color="blue") +
  geom_hline(yintercept=2.718^zValue, linetype="dashed", color = "red")+
  labs(title="Powers of e", x="n", y="(1+z/n)^n")

curve(40*exp(0.0916*x),from=0, to=10,lwd=4)
curve(47.24*exp(0.15*(x-5)),from=5, to=10,lwd=4, add=TRUE,col="red")
curve(91.39*exp(0.03*(x-7)),from=7, to=10,lwd=4, add=TRUE,col="darkgreen")
abline(v=5,col="red",lwd=2)
abline(v=7,col="darkgreen",lwd=2)

