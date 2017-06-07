#Setting up necessaryParameteres
crossoverVersion <-1
populationSize <- 50
mutationProbabilty <- 0.5
gamesForIndividual <- 1

getPopulation <- function(){
  lapply(1:populationSize, function(i)generateIndividual())
}

competitionBetweenPopulations <- function(populationA, populationB)
{
  startGame <- function(selectedIndividual, opponents){
    sum(sapply(opponents, function(x)game_AIvsAI_revange(selectedIndividual, x)))
  }
  
  results <- sapply(populationA, function(i) startGame(i, sample(populationB, gamesForIndividual)))
  minResult <- min(results)
  sapply(results, function(x)(x - minResult))
}

evolveNextGeneration <-function(population, victoriesCount){
  newGeneration <- list()
  newGeneration[[1]] <- population[[which.max(victoriesCount)]]
  
  probability <- sapply(victoriesCount, FUN = countProbability, sum(victoriesCount))
  for(i in 2:populationSize){
    drawn <- sample(population, 2, FALSE, probability)
    if(crossoverVersion == 1){
      newGeneration[[i]] <- mutation(crossover(drawn[[1]], drawn[[2]]))
    }
    else{
      newGeneration[[i]] <- mutation(drawn[[1]], drawn[[2]])
    }
  }
  return(newGeneration)
}

countPropability <- function(x, sumOfElements){
  (x+1)/(sumOfElements + populationSize)
}


crossover <- function(networkA, networkB){
  boardSize<-board.size
  test<-networkA
  bool <- c(TRUE, FALSE)
  test[1,] <- sample(bool, hiddenNeuronsCount, TRUE)
  test[1:(2*boardSize*boardSize),] <- apply(test[1:(2*boardSize*boardSize),], 2, function(col){ sapply(col, function(y)col[1]) })
  test[(2*boardSize*boardSize + 1):(3*boardSize*boardSize),1] <- sample(bool, boardSize*boardSize, TRUE)
  test[(2*boardSize*boardSize + 1):(3*boardSize*boardSize),] <- t(apply(test[(2*boardSize*boardSize + 1):(3*boardSize*boardSize),], 1, function(row){ sapply(row, function(y)row[1]) }))
  ifelse(test, networkA, networkB)
}

# Creates a new individual with neurons from hidden layer from individualA
# or individualB (randomly choosed) each copied neuron stays with his input
# and output connection weights
crossover2 <- function(networkA, networkB){
  test<-networkA
  bool <- c(TRUE, FALSE)
  test[1,] <- sample(bool, hiddenNeuronsCount, TRUE)
  test <- apply(test, 2, function(col){ sapply(col, function(y)col[1]) })
  crossoverResult <- ifelse(test, networkA, networkB)
  NormalizeConnections(crossoverResult)
}

mutation <- function(network){
  randomNumber <- runif(1, min = 0, max = 1)
  if(mutationProbabilty > randomNumber){
    column <- sample(1:hiddenNeurons)
    row <- sample(1: 3 * boardSize * boardSize)
    network[row, column] <- runif(1, min = 0, max = 1)
    network <- makeSumsEqual1(network, boardSize)
  }
  return(network)
}
