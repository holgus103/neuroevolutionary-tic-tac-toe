hiddenNeurons <- 10
games <- 100
epochs <- 500

generateIndividual <- function(){
  weightsCount <- 3 * boardSize * boardSize * hiddenNeurons
  i <- matrix(runif(weightsCount, min = 0, max = 1), ncol = hiddenNeurons)
  
  i[1:(boardSize * boardSize * 2),] <-
    apply(i[1:(boardSize * boardSize * 2),], 2, function(c){
      s = sum(c)
      sapply(c, function(e){
        return(e/s)
      })
    })
  
  i[(boardSize * boardSize * 2 + 1):(boardSize * boardSize * 3),] <-
    apply(i[(boardSize * boardSize * 2 + 1) : (boardSize * boardSize * 3),], 1, function(r){
      s <- sum(r)
      sapply(r, function(e){
        return(e/s)
      })
    })
  return(i)
}

calculate <- function(board, i){
  size <- dim(board)[1]
  input = apply(as.vector(board))
  hLayerInput = input %*% i[1:(2 * size * size),]
  output <- hLayerInput %*% t(i[(2 * size * size + 1):(3 * size * size),1])
  return(output)
}