
initBoard <- function(){
  return(matrix(0, ncol = boardSize, nrow = boardSize))
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
  
  evaluator <- function(i){
    checkSubMatrix(getSubMatrix(board, i[1], i[2]))
  }
  
  winningSeriesConverter <- function(val){
    if(val == winningSeries){
      return(1)
    }
    else if(val == -(winningSeries)){
      return(-1)
    }
    else{
      return(0)
    }
  }
  rightevaluator <- function(i){
     val = sum(board[c(i[1]:(i[1] + winningSeries - 1)), i[2]])
     return(winningSeriesConverter(val))
  }
  
  bottomevaluator <- function(i){
    val = sum(board[i[1], c(i[2]: (i[2] + winningSeries - 1))])
    return(winningSeriesConverter(val))
  }
  # evalute inner boards
  corners = expand.grid(c(1:(boardsize - winningSeries + 1)), c(1:(boardsize - winningSeries + 1)))
  res = apply(corners, 1, evaluator)
  rightcorners = expand.grid(c(1:(boardsize - winningSeries + 1)),c((boardsize - winningSeries + 2): boardsize))
  rightres = apply(rightcorners, 1, rightevaluator)
  bottomcorners =   expand.grid(c((boardsize - winningSeries + 2): boardsize), c(1:(boardsize - winningSeries + 1)))
  bottomres = apply(bottomcorners, 1, bottomevaluator)
  return(sum(res, rightres, bottomres))
}

