
winningSeries <- 3

initBoard <- function(){
  return(matrix(0, ncol = boardsize, nrow = boardsize))
}

checkForVictory <- function(board){
  boardsize <- dim(board)[1]
  checkSubMatrix <- function(board){
    secondDiagonal <- function(m){
      return(diag(apply(m, 2, rev)))
    }
    sequences = c(
      # check first row
      sum(board[1,]),
      # check first column
      sum(board[,1]),
      # check diagonal
      sum(diag(board)),
      # check other diagonal
      sum(secondDiagonal(board))
    )
    if(max(sequences) == winningSeries){
      return(1)
    }
    else if(min(sequences) == (-winningSeries)){
      return(-1)
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
    checkSubMatrix(getSubMatrix(board, i[1], i[2]))
  })
  return(sum(res))
  
  # TODO: check the remaining right columns and the bottom rows
  
      
}

