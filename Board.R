boardsize <- 3
winningSeries <- 3

iInitBoard <- function(){
  return(matrix(0, ncol = boardsize, nrow = boardsize))
}

checkForVictory <- function(board){
  
}

checkSubMatrix <- function(board){
  secondDiagonal <- function(m){
    return(diag(apply(m, 2, rev)))
  }
   maxSequence = max(
              # check first row
              sum(board[1,]),
              # check first column
              sum(board[,1]),
              # check diagonal
              sum(diag(board)),
              # check other diagonal
              sum(secondDiagonal(board))
             )
   if(abs(maxSequence) == winningSeries){
     return(maxSequence/winningSeries)
   }
   else{
     return(0)
   }
         
}