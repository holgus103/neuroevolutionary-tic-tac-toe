

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

calculate_c <- cmpfun(calculate)

chooseBestMove <-function(board, network, sideOnTheMove){
  if(sideOnTheMove == -1){
    board <- sapply(board, function(x) -x)
  }
  result <- calculate_c(board, network)
  result[board != 0] = -Inf
  return(which.max(result))
}

makeMove <- function(board, network, sideOnTheMove){
  bestField <- chooseBestMove(board, network, sideOnTheMove)
  board[[bestField]] = sideOnTheMove
  return(board)
}

startAlgorithm <- function(iterations){
  populationA <- getPopulation()
  populationB <- getPopulation()
  it <- iterations[which.max(iterations)]
  for(i in 1:it){
    victoriesCountA <- competitionBetweenPopulations(populationA, populationB)
    victoriesCountB <- competitionBetweenPopulations(populationB, populationA)
    populationA <- evolveNextGeneration(populationA, victoriesCountA)
    populationB <- evolveNextGeneration(populationB, victoriesCountB)
    if(length(which(iterations == i)) > 0){
      print(paste("Iteration", i, "completed"))
      evaluate(populationA, populationB, victoriesCountA, victoriesCountB)
    }
  }
  print("Finished")
}

evaluate <- function(populationA, populationB, victoriesCountA, victoriesCountB){
  victoriesCountA <- competitionBetweenPopulations(populationA, populationB)
  victoriesCountB <- competitionBetweenPopulations(populationB, populationA)
  winnerA <- which.max(victoriesCountA)
  winnerB <- which.max(victoriesCountB)
  competitionWinner <- populationA[winnerA]
  if(victoriesCountB[winnerB] > victoriesCountA[winnerA]){
    competitionWinner <- populationB[winnerB]
  }
  competitionWinner <- competitionWinner[[1]]
  gameWithRandom <- parSapply(cl, 1:testGames, function(x){game_AIvsRandom(competitionWinner)})
  wins = length(gameWithRandom[gameWithRandom > 0])
  loses = length(gameWithRandom[gameWithRandom < 0])
  res = as.matrix(c(wins, length(gameWithRandom) - wins - loses, loses, length(gameWithRandom)))
  rownames(res) <-c("Wins", "Draws","Loses", "Total")
  print(paste("population", populationSize, "neurons", hiddenNeurons, "mutation", mutationProbabilty))
  print(res)
  return(0)
}






