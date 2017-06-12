populationTest <- function(populationCount){
  res = c()
  for(val in  1:length(populationCount)){
    populationSize <- populationCount[val]
    res[val] <- startAlgorithm()
  }
  return(res)
}

res = list()
for(i in 1:10){
  res[[i]] = populationTest(c(10, 25, 50, 100, 200))
}

#print(res)
m = sapply(res,unlist)
apply(m, 1, mean)


