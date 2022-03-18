#
# importingData_CSVfiles.R
# 
# Import (or 'load') data from a CSV file.
#
# DGB Mar 2020
#

# To execute a single line place the cursor on the line and hit <ctrl><enter>
# To execute a block of lines, highlight them and hit <ctrl><enter>
# To execute the whole file: <ctrl><shift><enter> 

# --------------- A small data set:

#   Load data from csv with the top row of the csv used as column headers.

monPolData <- read.csv("monPol_1.csv",header=TRUE)

#   Load data from csv with the top row of the csv used as the first row of the columns with column headers added by R (as V#).

monPolData_noHeader <- read.csv("monPol_1.csv",header=FALSE)


# Print the data in the console.

monPolData

# Print just 1 column

monPolData$bank

# Create a variable out of a column
r <- monPolData$rate

# Print the new variable
r


# Print just 1 row (you'll get the header too)


monPolData[1,]
monPolData[2,]

# Create a variable/data-frame from a single row.
uk_data <- monPolData[1,]
uk_data

# Print just one element 

monPolData[1,3]

# How different do things look if we don't make the first line of the CSV file into a header in R?

monPolData

monPolData_noHeader
# In the latter the first row of `data' is the old column headers - not what we want!
# This can be useful though if the first row of the original csv file does not include the variable names.


