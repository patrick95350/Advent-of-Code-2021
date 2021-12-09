### Advent of Code Day 3
#     Binary Diagnostic
#     Available at https://adventofcode.com/
#
#     By Patrick Rogers, California Research Bureau
#       Dec 2021
#
#     Uses the following data
#       Day 3 - Binary Diagnostic.csv

# Clear the workspace
remove(list=ls(all=TRUE))

# Inits
local.dir <- "C:\\Users\\patri\\Documents\\CRB Projects\\02. Training\\Advent of Code 2021"
setwd(local.dir)

# Load Packages

# Load Sourcefiles

# User Parameters

# Custom Functions ####
calcPower <- function(input){
  gamma <- 0
  epsilon <- 0
  
  for(i in floor(log10(max(input))):0){
    gamma <- gamma + (10^i * (sum(input %/% 10^i) > (length(input)/2)))
    epsilon <- epsilon + (10^i * !(sum(input %/% 10^i) > (length(input)/2)))
    
    input <- input - ((input %/% 10^i) * 10^i)
  }
  
  val <- strtoi(as.character(gamma), base=2) * strtoi(as.character(epsilon), base=2)
  return(val)
}

calcOxy <- function(input){
  oxy <- 0
  for(i in floor(log10(max(input))):0){
    if(length(input)==1){
      oxy <- oxy + input
      break
    }
    frequent <- 10^i * (sum(input %/% 10^i) >= (length(input)/2))
    input <- input[input %/% 10^i == frequent %/% 10^i]
    input <- input - frequent
    oxy <- oxy + frequent
  }
  return(oxy)
}

calcCO2 <- function(input){
  co2 <- 0
  for(i in floor(log10(max(input))):0){
    if(length(input)==1){
      co2 <- co2 + input
      break
    }
    rare <- 10^i * (sum(input %/% 10^i) < (length(input)/2))
    input <- input[input %/% 10^i == rare %/% 10^i]
    input <- input - rare
    co2 <- co2 + rare
  }
  return(co2)
}
  
# Function Test ####
test <- c(00100, 11110, 10110, 10111, 10101, 01111, 00111, 11100, 10000, 11001, 00010, 01010)

calcPower(test)
calcOxy(test)
calcCO2(test)
strtoi(as.character(calcOxy(test)), base=2) * strtoi(as.character(calcCO2(test)), base=2)

# Puzzle Input - Part 1 ####
digits <- read.csv("Day 3 - Binary Diagnostics Data.csv", header = FALSE)

calcPower(digits[,1])

# Puzzle Input - Part 2 ####
strtoi(as.character(calcOxy(digits[,1])), base=2) * strtoi(as.character(calcCO2(digits[,1])), base=2)




