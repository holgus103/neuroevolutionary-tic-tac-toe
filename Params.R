testGames <- 1000
#hiddenNeurons <- 10
#populationSize <- 30
#mutationProbabilty <- 0.5
gamesForIndividual <- 10
winningSeries <- 3
boardSize <- 3
hiddenNeurons <- 15
populationSize <- 50
mutationProbabilty <- 0.05
gamesForIndividual <- 25
victories <- 0
probs <- 0
dims = NULL
library(parallel)
library(compiler)
cl <- makeCluster(5, type = "PSOCK")
clusterEvalQ(cl, library(parallel))

clusterExport(cl, c("gamesForIndividual",
                    "game_AIvsAI_revange", 
                    "initBoard", 
                    "boardSize", 
                    "isAnyFieldEmpty",
                    "chooseBestMove",
                    "makeMove",
                    "calculate_c",
                    "checkForVictory",
                    "winningSeries", 
                    "game_AIvsRandom",
                    "randomizeSide",
                    "mutation",
                    "mutationProbabilty",
                    "hiddenNeurons",
                    "crossover",
                    "makeSumsEqual1"
                    ))

