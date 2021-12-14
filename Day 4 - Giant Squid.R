### Advent of Code Day 4
#     Giant Squid
#     Available at https://adventofcode.com/
#
#     By Patrick Rogers, California Research Bureau
#       Dec 2021
#
#     Uses the following data
#       Day 4 - Giant Squid.csv

# Clear the workspace
remove(list=ls(all=TRUE))

# Inits
local.dir <- "C:\\Users\\patri\\Documents\\CRB Projects\\02. Training\\Advent of Code 2021"
setwd(local.dir)

# Load Packages

# Load Sourcefiles

# User Parameters

# Custom Functions ####
buildBoards <- function(data, size){
  data <- gsub("^ ", "", data)
  data <- gsub("  +", " ", data)
  val <- vector("list", length = length(data)/5)
  for(i in 1:length(val)){
    start <- ((i - 1) * 5) + 1
    end <- start + size - 1
    #print(start:end)
    val[[i]] <- do.call("rbind", strsplit(data[start:end], " "))
    #class(val[[i]]) <- "numeric"
  }
  return(val)
}

stampBoard <- function(board, call){
  # Helper function for runBingo
  board <- gsub(paste0("^", call, "$"), "-1", board)
  return(board)
}

numericBoard <- function(board){
  # Helper function for checkWinner
  class(board) <- "numeric"
  return(board)
}

checkWinner <- function(boards){
  # Helper function for runBingo
  boards <- lapply(boards, numericBoard)
  boardRows <- lapply(boards, rowSums)
  boardCols <- lapply(boards, colSums)
  val <- list(winner = length(unique(c(grep(-5, boardRows), grep(-5, boardCols))))>0,
              board = unique(c(grep(-5, boardRows), grep(-5, boardCols))))
  return(val)
}

runBingo <- function(calls, boards){
  outcome <- checkWinner(boards)
  
  while(!outcome$winner){
    boards <- lapply(boards, function(x) stampBoard(board=x, call=calls[1]))
    outcome <- checkWinner(boards)
    
    if(!outcome$winner){
      calls <- calls[-1]
      if(length(calls)==0){
        print("board cannot win")
        break
      }
    } else {
      winner <- boards[[outcome$board[1]]] # Sometimes more than one board will win on the same run, pick the lowest index
      
      winner <- numericBoard(gsub("^-1$", "0", winner))
      score <- sum(colSums(winner)) * calls[1]
    }
  }
  
  return(list(board=outcome$board,
              score=score,
              calls=calls))
}

# Function Test ####
# Part 1
calls <- as.numeric(read.csv("Day 4 - Giant Squid Test.csv", header = FALSE,
                             nrows = 1)[1,])
boards <- read.csv("Day 4 - Giant Squid Test.csv", header = FALSE, skip=1)
boards <- buildBoards(boards[,1], 5)

runBingo(calls, boards)

# Part 2
while(length(boards)>1){
  outcome <- runBingo(calls, boards)
  #calls <- outcome$calls[-1]
  boards <- boards[-outcome$board]
}

runBingo(calls, boards)

# Puzzle Input - Part 1 ####
calls <- as.numeric(read.csv("Day 4 - Giant Squid.csv", header = FALSE,
                 nrows = 1)[1,])

boards <- read.csv("Day 4 - Giant Squid.csv", header = FALSE, skip=1)
boards <- buildBoards(boards[,1], 5)

runBingo(calls, boards)

# Puzzle Input - Part 2 ####
while(length(boards)>1){
  outcome <- runBingo(calls, boards)
  boards <- boards[-outcome$board]
}

runBingo(calls, boards)



