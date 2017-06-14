
  sink("res", append=TRUE)
  testGames <- 1000
  epochs <- 200
  iterations = c(50,100,150,200)
  winningSeries <- 4
  boardSize <- 5
  hiddenNeurons <- 15
  populationSize <- 50
  mutationProbabilty <- 0.05
  gamesForIndividual <- 10
  #startAlgorithm(iterations)
  mutationProbabilty <- 0.2
  #startAlgorithm(iterations)
  mutationProbabilty <- 0.5
  #startAlgorithm(iterations)
  
  mutationProbabilty <- 0.2
  hiddenNeurons <- 10
  startAlgorithm(iterations)
  hiddenNeurons <- 20
  startAlgorithm(iterations)
  
  hiddenNeurons <- 15
  populationSize <- 20
  startAlgorithm(iterations)
  populationSize <- 100
  startAlgorithm(iterations)