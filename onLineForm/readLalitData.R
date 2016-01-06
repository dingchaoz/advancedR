

# Read the whole data set line by line into variable data


# Remove all the blank rows in the raw data
rawdata <- rawdata[sapply(rawdata, nchar) > 0]

data <- read.table("data.txt",sep="\t")

dataNeW <-data[!data$V1=="deg"),]

# use melt and cast/dcast to transform the shape