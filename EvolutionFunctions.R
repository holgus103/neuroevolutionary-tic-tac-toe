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
  play <- function(selectedIndividual, opponents){
    sum(sapply(opponents, function(x))
  }
  
  results <- sapply((populationA, function(i) Play(i, sample(populationB, gamesForIndividual))))
  minResult <- min(results)
  sapply(results, function(x)(x - minResult))
}


mutation <- function(individual, ){
  randomNumber <- runif(1, min = 0, max = 1)
  if(mutationProbabilty > randomNumber){
    column <- sample(1:hiddenNeurons)
    row <- sample(1: 3 * boardSize * boardSize)
    individual[row, column] <- runif(1, min = 0, max = 1)
    individual <- makeSumsEqual1(individual, boardSize)
  }
  return(individual)
}
