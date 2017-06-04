boardsize <- 3
winningSeries <- 3

initBoard <- function(){
  return(matrix(0, ncol = boardsize, nrow = boardsize))
}

checkForVictory <- function(board){
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
  
  getSubMatrix <- function(board, uppery, leftx){
    return(board[c(leftx:(leftx+winningSeries-1)), c(uppery:(uppery+winningSeries-1))])
  }
  
  corners = expand.grid(c(1:(boardsize - winningSeries + 1)), c(1:(boardsize - winningSeries + 1)))
  res = apply(corners, 1, function(i){
    checkSubMatrix(getSubMatrix(board, i[1], i[1]))
  })
  
  # TODO: check the remaining right columns and the bottom rows
  
      
}

