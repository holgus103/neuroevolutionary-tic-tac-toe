testGames <- 1000
epochs <- 200
hiddenNeurons <- 10
populationSize <- 30
mutationProbabilty <- 0.5
gamesForIndividual <- 10
winningSeries <- 3
boardSize <- 3
library(parallel)
library(compiler)
cl <- makeCluster(getOption("cl.cores", 12))
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
                    "randomizeSide"
                    ))