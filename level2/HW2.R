# Name: Yating Tian
# Email: ytian83@wisc.edu

rm(list = ls())

# Implement Connect Four in the same manner that we
# implemented tic-tac-toe in lecture. Start by implementing
# the helper functions, below, and testing them by running
#   source("HW2test.R")
# Then write code for the game itself.
#
# We'll test your code by running
#   source("HW2.R")
# We might also play Connect Four, read your code, and do other tests.

# Returns TRUE if vector v (of character strings) contains
# (at least) four in a row of player (character string). e.g.
#   four.in.a.row("X", c("O","X","X","X","X","O"))
# is TRUE, while
#   four.in.a.row("O", c("O","X","X","X","X","O"))
# is FALSE.
four.in.a.row = function(player, v, debug=FALSE) {
  if (debug) {
    cat(sep="", "four.in.a.row(player=", player, ", v=", v, ")\n")
  }
  if (length(v)<4){
    return(F)
  }else{
    for(i in 1:length(v)){
      if(length(v)-i<3){
        break
      }
      if (v[i]!=player){
        next
      }else{
        if ((v[i]==v[i+1]) & (v[i]==v[i+2]) &(v[i]==v[i+3])){
          return(T)
        }
      }
    }
  }
  return(FALSE) # correct this return() statement
}

# Returns TRUE if (matrix) board (of character strings)
# contains at least four in a row of (string) player, who
# just played in position (r, c). (Here "r" means "row" and
# "c" means "column").
#
# Hint: this function should call four.in.a.row() four times, once
# each for the current row, column, diagonal, and reverse diagonal.
won = function(player, board, r, c, debug=FALSE) {
  if (debug) {
    cat(sep="", "won(player=", player, ", board=\n")
    print(board)
    cat(sep="", ", r=", r, ", c=", c, ")\n")
  }
  return(four.in.a.row(player, board[r, ]) | 
           four.in.a.row(player, board[, c]) | 
           four.in.a.row(player, board[row(board) - col(board) == r - c]) | 
           four.in.a.row(player, board[row(board) + col(board) == r + c]))
  
  return(FALSE) # correct this return() statement
}

# Returns largest index of an empty position in column col
# of (matrix) board. If there is no such empty position in
# board, return value is NULL.
largest.empty.row = function(board, col, debug=FALSE) {
  if (debug) {
    cat(sep="", "largest.empty.row(board=\n")
    print(board)
    cat(sep="", ", col=", col, ")\n")
  }
  line=board[,col]
  index=0
  for(i in line){
    if (i=="E"){
      index=index+1
    }
  }
  if (index==0){
    return(NULL)
  }else{
    return(index)
  }
}

source("HW2test.R") # Run tests on the functions above.

nrow=7
ncol=6
par(pty = "s")
x = rep(1:nrow, each = ncol)
y = rep(1:ncol, times = nrow)
symbols(x,y, squares = rep(1, 42),inches = FALSE,xlim = c(0, 8), ylim = c(7,0))

board = matrix(rep("E", 42), nrow = 6, ncol = 7)
player = "X"
for (i in 1:42) {
  if (player == "X") { #human player
    repeat {
      index = identify(x, y, n=1, plot = FALSE)
      col = x[index]
      row = y[largest.empty.row(board, col)]
      if (length(row)==0){
        text(x=4, y=7, labels=" click empty column.")
      }
      else if (board[row, col] == "E") {
        break
      }
    }
  } else { # computer player
    index = sample(x = which(c(board) == "E"), size = 1)
    col = x[index]
    row = y[largest.empty.row(board, col)]
  }
  board[row, col] = player
  text(col, row, player)
  cat(sep="", "i=", i, ", player=", player, ", index=", index,
      ", row=", row, ", col=", col, ", board:", "\n")
  print(board)
  if (won(player, board, row, col)) {
    text(x=2, y=1/7, labels=paste(player, " won!"), col="red")
    break
  }
  player = ifelse(player == "X", "O", "X")
}


# Hint: this program is modeled on the tic-tac-toe program we did in
# lecture, so studying the latter program is worthwhile.

# Note that a user click in a column indicates that a checker should
# go to that column's lowest empty row (unless the column is full).

# Note that you should implement a computer player. At the least, it
# should choose randomly from among the non-full columns. (Feel free
# to do much more!)
