### Advent of Code Day 1
#     Sonar Sweep
#     Available at https://adventofcode.com/
#
#     By Patrick Rogers, California Research Bureau
#       Dec 2021
#
#     Uses the following Packages
#       library1
#
#     Uses the following data
#       Day 1 - Sonar Sweep Data.csv

# Clear the workspace
remove(list=ls(all=TRUE))

# Inits
local.dir <- "C:\\Users\\patri\\Documents\\CRB Projects\\02. Training\\Advent of Code 2021"
setwd(local.dir)

# Load Packages

# Load Sourcefiles

# User Parameters

# Custom Functions ####
count_increases <- function(data, window=1){
  # Aggregate by window size
  for(i in window:length(data) - window + 1){
    data[i] <- sum(data[seq(from=i, by=1, length.out=window)])
  }
  # Trim
  data <- data[window:length(data) - window + 1]
  
  val <- data - c(data[1],data[1:length(data)-1])
  val <- sum(val>0)
  return(val)
}

# Function Test ####
depth <- c(199,200,208,210,200,207,240,269,260,263)
count_increases(data=depth, window=3)

# Puzzle Input ####
depth <- read.csv(file = "Day 1 - Sonar Sweep Data.csv", header=FALSE)
count_increases(data = depth[,1], window=1)
count_increases(data = depth[,1], window=3)

# Step 3 ####










