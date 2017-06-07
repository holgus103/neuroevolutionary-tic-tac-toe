isAnyFieldEmpty <- function(board){
  return(length(which(board)))
}

randomizeSide <- function(){
  return(sample(c(-1, 1), 1))
}

game_AIvsRandom <- function(network){
  aiPlayer <- RandomizeSide
  sideOnTheMove <- RandomizeSide
  
  board <- initBoard()
  gameResult <- 0
  
  while(gameResult == 0 && isAnyFieldEmpty()){
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
}