# Remove everything below here ---------------------------------------------------

# Plot the actual and fitted values

p_fit <- mod1$fit   # Create a new variable equal to the fitted values (this is not necessary but makes things a bit clearer.)
# The alternative is to add mod1$fit directly to the dataset in the next line.

mpdata$p_fit <- p_fit   # This adds a new column called p_fit OR replaces the data if the column already exists.
head(mpdata)

ggplot(mpdata, aes(date)) + geom_line(aes(y=p),colour="blue") + geom_line(aes(y=p_fit),colour="red")








# --- Add more variables
mod1 <- lm(p~m1+m3, data = mpdata)
summary(mod1)


# --- Add lagged variables


# Create some lagged series
library(dplyr)   # We need this package for lagging the data.


lag_p  = lag(mpdata$p,1)
lag_m1 = lag(mpdata$m1,1)
lag_m3 = lag(mpdata$m3,1)

# Add the lagged variables to the dataframe
mpdata$lag_p  <- lag_p
mpdata$lag_m1 <- lag_m1
mpdata$lag_m3 <- lag_m3

ggplot(mpdata, aes(date)) + geom_line(aes(y=p-lag_p),colour="blue") 

mod2 <- lm(p~lag_p, data = mpdata)
summary(mod2)

mod3 <- lm(p~lag_p+m1, data = mpdata)
summary(mod3)

# It fits the price level quite well, but what about fitting the inflation rate?

infl <- (mpdata$p - lag_p) / lag_p                # Quiz: Why do we need the 'mpdata$' attached to p but not to lag_p?
m1growth <- (mpdata$m1 - lag_m1) / lag_m1
lag_m1growth <- lag(m1growth,1)

mpdata$infl  <- infl
mpdata$m1growth  <- m1growth
mpdata$lag_m1growth  <- lag_m1growth


mod4 <- lm(infl~m1growth+lag_m1growth, data = mpdata)
summary(mod4)

# --- Restrict the sample period
mpdata_2 <- subset(mpdata, date > "1960-01-01" & date < "1985-01-01")
mod5 <- lm(infl~m1growth+lag_m1growth, data = mpdata_2)
summary(mod5)
mod6 <- lm(infl~lag_m1growth, data = mpdata_2)
summary(mod6)


# Plot actual and fitted values when the model includes lags of the dependent variable. i.e. p = a*lag_p + b*m.

mpdata_3 <- subset(mpdata, date >= "1960-02-01" & date <= "2019-10-01") # Creates a new df without the first row of the original. We do this because there is no fitted value from the model for the first period when we have a 1-period lag of the dependent variable included on the rhs..

p_fit <- mod3$fit   # We are using the fitted values from mod3 above.

mpdata_3$p_fit <- p_fit

ggplot(mpdata_3, aes(date)) + geom_line(aes(y=p),colour="blue") + geom_line(aes(y=p_fit),colour="red") + geom_line(aes(y=p-p_fit),colour="green")

# The green line here shows the differences between the actual and fitted values.

