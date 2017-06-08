

generateIndividual <- function(){
  weightsCount <- 3 * boardSize * boardSize * hiddenNeurons
  network <- matrix(runif(weightsCount, min = 0, max = 1), ncol = hiddenNeurons)
  network <- makeSumsEqual1(network, boardSize)
  return(network)
}

makeSumsEqual1 <- function(network, boardSize){
  network[1:(boardSize * boardSize * 2),] <-
    apply(network[1:(boardSize * boardSize * 2),], 2, function(c){
      s = sum(c)
      sapply(c, function(e){
        return(e/s)
      })
    })
  
  network[(boardSize * boardSize * 2 + 1):(boardSize * boardSize * 3),] <-
    t(apply(network[(boardSize * boardSize * 2 + 1) : (boardSize * boardSize * 3),], 1, function(r){
      s <- sum(r)
      sapply(r, function(e){
        return(e/s)
      })
    })
  )
  return(network)
}

calculate <- function(board, network){
  size <- boardSize
  boardVector = as.array(board)
  input = c(sapply(boardVector, function(s){
      length(which(s == 1))
    }), 
    sapply(boardVector, function(s){
      length(which(s == -1))
    })
  )
  hLayerInput = input %*% network[1:(2 * size * size),]
  return(network[(2 * size * size + 1):(3 * size * size),] %*% t(hLayerInput))
}

chooseBestMove <-function(board, network, sideOnTheMove){
  if(sideOnTheMove == -1){
    board <- sapply(board, function(x) -x)
  }
  result <- calculate(board, network)
  result[board != 0] = -Inf
  return(which.max(result))
}

makeMove <- function(board, network, sideOnTheMove){
  bestField <- chooseBestMove(board, network, sideOnTheMove)
  board[[bestField]] = sideOnTheMove
  return(board)
}

startAlgorithm <- function(){
  populationA <- getPopulation()
  populationB <- getPopulation()
  for(i in 1:epochs){
    victoriesCountA <- competitionBetweenPopulations(populationA, populationB)
    victoriesCountB <- competitionBetweenPopulations(populationB, populationA)
    populationA <- evolveNextGeneration(populationA, victoriesCountA)
    populationB <- evolveNextGeneration(populationB, victoriesCountB)
    print(paste("Iteration", i, "completed"))
  }
  print("Finished")
  victoriesCountA <- competitionBetweenPopulations(populationA, populationB)
  victoriesCountB <- competitionBetweenPopulations(populationB, populationA)
  winnerA <- which.max(victoriesCountA)
  winnerB <- which.max(victoriesCountB)
  competitionWinner <- populationA[winnerA]
  if(victoriesCountB[winnerB] > victoriesCountA[winnerA]){
    competitionWinner <- populationB[winnerB]
  }
  competitionWinner <- competitionWinner[[1]]
  gameWithRandom <- sapply(1:testGames, function(x){game_AIvsRandom(competitionWinner)})
  print("Testing evolved algorithm against a random player")
  print(gameWithRandom)
  print("Results")
  wins = length(gameWithRandom[gameWithRandom > 0])
  res = as.matrix(c(wins, length(gameWithRandom) - wins, length(gameWithRandom)))
  rownames(res) <-c("Wins", "Draws or Loses", "Total")
  print(res)
}






