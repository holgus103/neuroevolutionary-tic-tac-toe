test <- function(){
  #sink("res", append=TRUE)
  testGames <- 1000
  epochs <- 3
  iterations = c(1,2,3)
  winningSeries <- 4
  boardSize <- 5
  hiddenNeurons <- 15
  populationSize <- 55
  mutationProbabilty <- 0.05
  gamesForIndividual <- 10
  startAlgorithm(iterations)
  mutationProbabilty <- 0.2
  startAlgorithm(iterations)
  mutationProbabilty <- 0.5
  startAlgorithm(iterations)
  
  mutationProbabilty <- 0.2
  hiddenNeurons <- 10
  startAlgorithm(iterations)
  hiddenNeurons <- 15
  startAlgorithm(iterations)
  hiddenNeurons <- 20
  startAlgorithm(iterations)
  
  hiddenNeurons <- 15
  populationSize <- 20
  startAlgorithm(iterations)
  populationSize <- 50
  startAlgorithm(iterations)
  populationSize <- 100
  startAlgorithm(iterations)
}