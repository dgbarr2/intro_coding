#
# FY2020_MacroPru_riskAversion.R
#
# Use R to illustrate the basics of risk aversion using a 
#       simple utility function and a binomial distribution.
#
# DGB May 2020

rm(list = ls())  # This line clears R's memory of data so that we start from a clean slate
#    for this program with nothing hanging around from any programs we ran earlier.


library("ggplot2")
install.packages("crayon")  # Comment this out after use.
library("crayon")

# ------------------ Set the values of the main variables ------------------

#
# We get utility (u) from wealth (w).
# We set out with wealth of w0 and are offered a free
#    bet with a random outcome s: either up (su) or down (sd). 
#    su is positive and sd is negative.
# We will be told the variance of s, and will write functions
#    to calculate its up and down (i.e. win and lose) values.
# We do this so that we can relate a given variance of s to our
#    expected utility if we accept the bet.

w0 <- 3

# The probability of winning is pu, and of losing is pd.

pu <- 0.5
pd <- 1 - pu

# Our degree of risk aversion is represented by a parameter gamma.
#     Higher gamma means greated aversion to risk.
#     Risk neutrality gives us a gamma of zero.

gamma <- 2


# --------- Create some functions related to risk aversion --------

# Constant relative risk aversion (CRRA) utility.
utility <- function(x,g){
  if (g == 1){
    u <- log(x)
  }
  else {
    u <- (x**(1-g) - 1) / (1 - g)  # Try calculating this if g = 1.
  }
return(u)
}

# Print the initial level of utility given w0 and gamma.
cat(white$bold$underline$bgYellow("Utility of w0 = ", utility(w0,gamma), "\n"))

# Expected utility if we accept the bet.
expectedU <- function(w,s,p,g){
  Eu <- p*utility(w+s,g) + (1-p)*utility(w-s,g)
return(Eu)
}
cat(white$bold$underline$bgYellow("Exp Utility of the bet = ", expectedU(w0,4,pu,gamma), "\n"))

# ---------------- Create functions to calculate the value ----------------
# ----------------      of s and of expected utility       ----------------
# ----------------      for a given variance of s.         ----------------

# Calculate the implied value of s for a given variance of s.
# (This will be useful for the graphs we will be creating later.)

impliedS <- function(Vs,pu){
  is <- 0.5 * sqrt(Vs/(pu*(1-pu)))
return(is)
}
cat(white$bold$underline$bgYellow("For variance= 3 and pu = ", pu,
                         "the up payoff is ", impliedS(3,pu), "\n"))
# Calculate the expected utility for a given variance of s.
# (Also useful for the later graphs.)

Eu_given_v <- function(V,p,w,g){
  su <- impliedS(V,p)
  ExpU <- expectedU(w,su,p,g)
  return(ExpU)
}
cat(white$bold$underline$bgYellow("For variance= 3 and pu = ", pu,
                                  "expected utility is ", Eu_given_v(3,pu,w0,gamma), "\n"))
# ------------ Create UDFs to create vectors of variables which we  -------------------
# ------------           will use as data for some graphs.          -------------------

# Use a for loop to create a sequence of numbers.
createSequence <- function(start,increment,n){
  V_vector <- vector(length=n)
  V_vector[1] <- start
  for(i in 2:n){
    V_vector[i] <- V_vector[i-1] + increment
  }
  return(V_vector)
}
createSequence(1,1,5)



# Now create the same sequence using a simpler method.
# (But this doesn't teach you about for loops!)

seq(1,5, by=1)


# UDF to create a vector of values of utility for a range of wealth levels.
createUs <- function(w,g){
  U_vector <- utility(w,g)
  return(U_vector)
}



# UDF to create a vector of values of expected utilities.
createExpUs <- function(V,p,w,g){
  Eu_vector <- Eu_given_v(V,p,w,g)
  return(Eu_vector)
}



# UDF to create a vector of implied s values for a range of s variances.
createImpliedSs <- function(V,p){
  is_vector <- impliedS(V,p)
  return(is_vector)
}

# ----------------- Now we put some graphs together. ----------------

#
# Plot utility against w0 i.e. see what the utility function looks like. 
#

Wealths <- createSequence(1,.1,50)   # Create a vector of wealth levels
Utilities <- createUs(Wealths,4) # Create a vector of the utility these give.

df_Utilities <- data.frame("Wealth" =  Wealths, "Utility" = Utilities)
                                     # Put them in a dataframe called df_utilities.
ggplot(df_Utilities,aes(Wealth)) + 
  geom_line(aes(y=Utility),color="darkgreen",lwd=2)
                                     # Create the graph.

#
# Plot implied s against variance of s.
#
Variances <- createSequence(0,.1,50)
ImpliedSs <- createImpliedSs(Variances,pu)

df_ImpliedSs <- data.frame("Implied_s" =  ImpliedSs, "Variance" = Variances)

ggplot(df_ImpliedSs,aes(Variance)) + geom_line(aes(y=Implied_s),color="darkgreen",lwd=2)




#
#  Plot expected utility against the variance of s.
#

ExpectedUtilities_2 <- createExpUs(Variances,pu,w0,2)
ExpectedUtilities_5<- createExpUs(Variances,pu,w0,5)

df_EUtilities <- data.frame("VarS" = Variances,
                            "ExpU_2" = ExpectedUtilities_2,
                            "ExpU_5" = ExpectedUtilities_5)

ggplot(df_EUtilities,aes(VarS),lwd=2) + 
                      geom_line(aes(y=ExpU_2),color="red",lwd=2) + 
                      geom_line(aes(y=ExpU_5),color="blue",lwd=2)

# As variance goes up, expected utility goes down if gamma > 0.
# Try gamma = 0 (Do you have to change gamma at the top
#                of the programme of can you just sub 0 for 
#                gamma in this final section?)

ExpectedUtilities_minus2 <- createExpUs(Variances,pu,w0,-2)
ExpectedUtilities_0 <- createExpUs(Variances,pu,w0,0)

df_EUtilities <- data.frame("VarS" = Variances,
                            "ExpU_minus20" = ExpectedUtilities_minus2,
                            "ExpU_0" = ExpectedUtilities_0)

ggplot(df_EUtilities,aes(VarS),lwd=2) + 
  geom_line(aes(y=ExpU_minus20),color="red",lwd=2) + 
  geom_line(aes(y=ExpU_0),color="blue",lwd=2)

# Check the change in the shape of the utility function for gamma < 0
Wealths <- createSequence(1,.1,50)   
Utilities <- createUs(Wealths,-2) 

df_Utilities <- data.frame("Wealth" =  Wealths, "Utility" = Utilities)

ggplot(df_Utilities,aes(Wealth)) + geom_line(aes(y=Utility),
                                                 color="darkgreen",lwd=2)


