### Advent of Code Day 2
#     Dive
#     Available at https://adventofcode.com/
#
#     By Patrick Rogers, California Research Bureau
#       Dec 2021
#
#     Uses the following data
#       Day 2 - Dive Data.csv

# Clear the workspace
remove(list=ls(all=TRUE))

# Inits
local.dir <- "C:\\Users\\patri\\Documents\\CRB Projects\\02. Training\\Advent of Code 2021"
setwd(local.dir)

# Load Packages

# Load Sourcefiles

# User Parameters
pos_depth <- c(pos=0, depth=0, aim=0)

# Custom Functions ####
nav <- function(call, depth, pos, aim, x){
  switch(call,
         forward = list(depth, pos + x, aim),
         up = list(depth - x, pos, aim),
         down = list(depth + x, pos, aim))
}

nav2 <- function(call, depth, pos, aim, x){
  switch(call,
         forward = list(depth + (aim * x), pos + x, aim),
         up = list(depth, pos, aim - x),
         down = list(depth, pos, aim + x))
}
  
# Function Test ####
test <- c("forward 5", "down 5", "forward 8", "up 3", "down 8", "forward 2")

pos_depth <- list(pos=0, depth=0, aim=0)
for(i in strsplit(test, split=" ")){
  pos_depth = do.call("nav", args=c(call=i[1], pos_depth, x=as.numeric(i[2])))
}
print(pos_depth)

pos_depth <- list(pos=0, depth=0, aim=0)
for(i in strsplit(test, split=" ")){
  pos_depth = do.call("nav2", args=c(call=i[1], pos_depth, x=as.numeric(i[2])))
}
print(pos_depth)

# Puzzle Input - Part 1 ####
nav_commands <- read.csv(file = "Day 2 - Dive Data.csv", header = FALSE)

pos_depth <- list(pos=0, depth=0, aim=0)
for(i in strsplit(nav_commands[,1], split=" ")){
  pos_depth = do.call("nav", args=c(call=i[1], pos_depth, x=as.numeric(i[2])))
}

print(pos_depth[[1]] * pos_depth[[2]])

# Puzzle Input - Part 2 ####
pos_depth <- list(pos=0, depth=0, aim=0)
for(i in strsplit(nav_commands[,1], split=" ")){
  pos_depth = do.call("nav2", args=c(call=i[1], pos_depth, x=as.numeric(i[2])))
}

print(pos_depth[[1]] * pos_depth[[2]])




