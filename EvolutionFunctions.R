

getPopulation <- function(){
  lapply(1:populationSize, function(i){
    generateIndividual()
    })
}

competitionBetweenPopulations <- function(populationA, populationB)
{
  startGame <- function(selectedIndividual, opponents){
    sum(sapply(opponents, function(x)game_AIvsAI_revange(selectedIndividual, x)))
  }
  popA <<- populationA
  popB <<- populationB
  return(parSapply(cl, populationA, function(i) startGame(i, sample(populationB, gamesForIndividual))))
}

evolveNextGeneration <-function(population, victoriesCount){
  newGeneration <- list()
  #print(victoriesCount[which.max(victoriesCount)])
  newGeneration[[1]] <- population[[which.max(victoriesCount)]]
  
  generateNewIndividual <- function(probability, population){
    drawn <- sample(population, 2, FALSE, probability)
    return(mutation(crossover(drawn[[1]], drawn[[2]])))
  }v
  probability <- sapply(victoriesCount, countPropability, sum(victoriesCount))
  probs <<- probability
  victories <<- victoriesCount
  return(parLapply(cl, 1:populationSize, function(x){generateNewIndividual(probability, population)}))
}

countPropability <- function(x, sumOfElements){
  (x)/(sumOfElements)
}


crossover <- function(networkA, networkB){
  test<-networkA
  bool <- c(TRUE, FALSE)
  test[1,] <- sample(bool, hiddenNeurons, TRUE)
  test[1:(2*boardSize*boardSize),] <- apply(test[1:(2*boardSize*boardSize),], 2, function(col){ sapply(col, function(y)col[1]) })
  test[(2*boardSize*boardSize + 1):(3*boardSize*boardSize),1] <- sample(bool, boardSize*boardSize, TRUE)
  test[(2*boardSize*boardSize + 1):(3*boardSize*boardSize),] <- t(apply(test[(2*boardSize*boardSize + 1):(3*boardSize*boardSize),], 1, function(row){ sapply(row, function(y)row[1]) }))
  ifelse(test, networkA, networkB)
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
