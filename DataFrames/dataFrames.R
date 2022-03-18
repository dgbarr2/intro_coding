#
# doodle_dataFrames_1.R
#
# Create a data frame.
#
# DGB Jan 2020
#

rm(list = ls())

#
# Load (fake) data from a csv file
#

monPol_1 <- read.csv("monPol_1.csv", stringsAsFactors=FALSE)
monPol_1

# Print the first column of data i.e. the variable 'bank'. Note that since 'bank' is part of monPol_1 we have to tell R where to find it. 
# We put the dataframe name before the $ and the column name after it.
monPol_1$bank




#
# Create another dataframe using the columns from monPol_1 but you name the columms of the new dataframe yourself.
# Note that we are not loading any more data - we are using what we already have to create a new dataframe called monPol_2
# in which we choose to call the first column, 'Bank' and fill it the data 'monPol_1$bank'.
#

monPol_2 <- data.frame(Bank=monPol_1$bank,Policy_rate=monPol_1$rate,Inflation_rate=monPol_1$inflation,stringsAsFactors=FALSE)
monPol_2

#
# Retrieve some named columns and create variables with them
#

bank_rates_1 <- monPol_1$rate
bank_rates_1                    # Note that the variable bank_rates_1 has only data in it and no column name.

bank_rates_2 <- monPol_2$Policy_rate
bank_rates_2

bank_rates_3 <- monPol_2$rate    # This will fail because there is no column named 'rate' in monPol_2
bank_rates_3


#
# Add a column of ROW names to replace the ROW numbers in mon_Pol_1
#

rownames(monPol_1) <- c("UK_rn","Japan_rn","EU_rn","Australia_rn")   # 'rn' represents 'row name' but we can call the rows anything we like.
monPol_1           # The dataframe now has rows labeled with names rather than just numbers.

 stopped here 25 Mar 2020
#
# Get some variables that describe the data frame.
#

nrow(monPol_1)

ncol(monPol_1)

colnames(monPol_1)

colnames(monPol_2)

rownames(monPol_1)

monPol_1$bank    # Compare this with the rownames above. 

#
# Accessing the data in a data frame
#

	# By position
monPol_1[1,2]

	# By names
monPol_1["Japan_rn","inflation"]

	# By name and position
monPol_1[1,"rate"]

	# Complete row
monPol_1[1,]
monPol_1["EU_rn",]

	# Complete column
monPol_1[,1]
monPol_1[,"inflation"]

	# Getting more than 1 column
monPol_1[,c("bank","inflation")]

	# Getting more than 1 row - rows(2 to 4)
monPol_1[2:4,]

monPol_1["UK_rn":"EU_rn",]  # Fails - this works for rows and columns identified by numbers but not by names.





#
# Write a data frame to a csv file
#



write.csv(monPol_1,"C:/Users/320709/Documents/FY2019-20/FY2020_Rcourse/Rdoodling/exportedData.csv",row.names=FALSE)


#
# NEXT: do factor variables???
#












