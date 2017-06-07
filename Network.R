games <- 100
epochs <- 500

generateIndividual <- function(){
  weightsCount <- 3 * boardSize * boardSize * hiddenNeurons
  network <- matrix(runif(weightsCount, min = 0, max = 1), ncol = hiddenNeurons)
  network <- makeSumsEqual1(network, boardSize)
  return(network)
}

makeSumsEqual1 <- function(network, boardSize){
  network[1:(boardSize * boardSize * 2),] <-
    apply(i[1:(boardSize * boardSize * 2),], 2, function(c){
      s = sum(c)
      sapply(c, function(e){
        return(e/s)
      })
    })
  
  network[(boardSize * boardSize * 2 + 1):(boardSize * boardSize * 3),] <-
    apply(network[(boardSize * boardSize * 2 + 1) : (boardSize * boardSize * 3),], 1, function(r){
      s <- sum(r)
      sapply(r, function(e){
        return(e/s)
      })
    })
  return(network)
}

calculate <- function(board, network){
  size <- dim(board)[1]
  input = apply(as.vector(board))
  hLayerInput = input %*% network[1:(2 * size * size),]
  output <- hLayerInput %*% t(network[(2 * size * size + 1):(3 * size * size),1])
  return(output)
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
  for(i in 1:hiddenNeuronsCount){
    victoriesCountA <- competitionBetweenPopulations(populationA, populationB)
    victoriesCountB <- competitionBetweenPopulations(populationB, populationA)
    populationA <- evolveNextGeneration(populationA, victoriesCountA)
    populationB <- evolveNextGeneration(populationB, victoriesCountB)
  }
  victoriesCountA <- competitionBetweenPopulations(populationA, populationB)
  victoriesCountB <- competitionBetweenPopulations(populationB, populationA)
  winnerA <- which.max(victoriesCountA)
  winnerB <- which.max(victoriesCountB)
  competitionWinner <- winnerA
  if(victoriesCountB[winnerB] > victoriesCountA[winnerA]){
    competitionWinner <- winnerB
  }
  competitionWinner <- competitionWinner[[1]]
  
  gameWithRandom <- sapply(1:testGamesCount, function(x){game_AIvsRandom(competitionWinner)})
  print("Testing evolved algorithm against a random player")
  return(gameWithRandom)
  
}

GenerateResultsTable <- function(results){
  winsCount = length(results[results=="1"])
  drawsCount = length(results[results=="0"])
  losesCount = length(results[results=="-1"])
  totalGamesCount <- winsCount + losesCount + drawsCount
  result <- as.matrix(c(winsCount, drawsCount, losesCount, totalGamesCount))
  rownames(result) <- c("Wins", "Draws", "Loses", "Total")
  return(t(result))
}






