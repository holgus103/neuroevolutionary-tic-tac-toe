
  sink("res", append=TRUE)
  testGames <- 1000
  iterations = seq(10, 1000, 10)
  winningSeries <- 3
  boardSize <- 3
  hiddenNeurons <- 15
  populationSize <- 50
  mutationProbabilty <- 0.05
  gamesForIndividual <- 10
  #startAlgorithm(iterations)
  mutationProbabilty <- 0.2
  startAlgorithm(iterations)
  mutationProbabilty <- 0.5
  startAlgorithm(iterations)
  
  mutationProbabilty <- 0.
  hiddenNeurons <- 10
  startAlgorithm(iterations)
  hiddenNeurons <- 20
  startAlgorithm(iterations)
  
  hiddenNeurons <- 15
  populationSize <- 20
  startAlgorithm(iterations)
  populationSize <- 100
  startAlgorithm(iterations)