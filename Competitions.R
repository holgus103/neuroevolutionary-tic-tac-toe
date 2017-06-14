isAnyFieldEmpty <- function(board){
  return(length(which(board == 0)))
}

randomizeSide <- function(){
  return(sample(c(-1, 1), 1))
}


#In any competition if firstSide wins the result is 1, if the second one - the result is 2
game_AIvsRandom <- function(network){
  aiPlayer <- randomizeSide()
  sideOnTheMove <- randomizeSide()
  board <- initBoard()
  gameResult <- 0
  while(gameResult == 0 && isAnyFieldEmpty(board)){
    if(sideOnTheMove == aiPlayer){
      board <- makeMove(board, network, sideOnTheMove)
    }
    else{
      randomMove <- sample(which(board==0), 1)
      board[[randomMove]] <- sideOnTheMove
    }
    
    gameResult <- checkForVictory(board)
    sideOnTheMove <- sideOnTheMove * -1
  }
  if(aiPlayer == gameResult)
    return(1)
  else if(gameResult == 0)
    return(0)
  else
    return(-1)
}

game_AIvsAI <- function(population1, population2){
  population1Side <- randomizeSide()
  sideOnTheMove <- randomizeSide()
  board <- initBoard()
  gameResult <- 0
  
  while(gameResult == 0 && isAnyFieldEmpty(board)){
    if(sideOnTheMove == population1Side){
      board <- makeMove(board, population1, sideOnTheMove)
    }
    else{
      board <- makeMove(board, population2, sideOnTheMove)
    }
    
    gameResult <- checkForVictory(board)
    sideOnTheMove <- sideOnTheMove * -1
  }
  
  if(gameResult == population1Side) return(1)
  else return(0)
}

game_AIvsAI_revange <- function(population1, population2){
  population1Side <- 1
  sideOnTheMove <- 1
  board <- initBoard()
  gameResult <- 0
  
  while(gameResult == 0 && isAnyFieldEmpty(board)){
    if(sideOnTheMove == population1Side){
      board <- makeMove(board, population1, sideOnTheMove)
    }
    else{
      board <- makeMove(board, population2, sideOnTheMove)
    }
    
    gameResult <- checkForVictory(board)
    sideOnTheMove <- sideOnTheMove * -1
  }
  side <- -1
  board <- initBoard()
  revangeResult <- 0
  
  while(revangeResult == 0 && isAnyFieldEmpty(board)){
    if(sideOnTheMove == population1Side){
      board <- makeMove(board, population1, sideOnTheMove)
    }
    else{
      board <- makeMove(board, population2, sideOnTheMove)
    }
    
    revangeResult <- checkForVictory(board)
    sideOnTheMove <- sideOnTheMove * -1
  }
  
  if(gameResult + revangeResult > 0) return(1)
  else return(0)
}




