# --------------- A larger data set (from Radziwill book)

# Load data from csv with variable names as headers from the top row of the csv.
mnmData <- read.csv("mnm-clean.csv",header=TRUE)
mnmData_noHeader <- read.csv("mnm-clean.csv",header=FALSE)

# Look at the data...

#       The whole dataset
mnmData

#       Just the first 6 rows
head(mnmData)

#       What difference does including a 'header' make?
head(mnmData_noHeader)


# Print one of the columns
mnmData$student