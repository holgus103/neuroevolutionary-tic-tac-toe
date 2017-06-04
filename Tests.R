testForValue <- function(actual, expected, testname){
  res = "failed"
  if(actual == expected){
    res = "passed"
  }
  else{
    print(paste(actual, "instead of", expected))
  }
  print(paste(testname, res))
}

basicRowDetectionTest <- function(){
  m = matrix(c(1, 1, 1,
              -1,-1, 1,
              -1,-1, 0), nrow = 3, ncol = 3);
  testForValue(checkForVictory(m), 1, "basicRowDetectionTest")
}

basicColumnDetectionTest <- function(){
  m = matrix(c(1,-1, 0,
               1, 0,-1,
               1,-1, 0), nrow = 3, ncol = 3);
  testForValue(checkForVictory(m), 1, "basicColumnDetectionTest")
}

basicDiagonalDetectionTest <- function(){
  m = matrix(c(1,-1, 0,
               0, 1,-1,
               0,-1, 1), nrow = 3, ncol = 3);
  testForValue(checkForVictory(m), 1, "basicDiagonalDetectionTest")
}

basicSecondDiagonalDetectionTest <- function(){
  m = matrix(c(0,-1, 1,
               0, 1,-1,
               1,-1, 0), nrow = 3, ncol = 3);
  testForValue(checkForVictory(m), 1, "basicSecondDiagonalDetectionTest")
}

nothignInterestingTest <- function(){
  m = matrix(c(0,-1, 1,
               0, 1,-1,
              -1, 1, 0), nrow = 3, ncol = 3);
  testForValue(checkForVictory(m), 0, "nothignInterestingTest")
}

oppositeWinnerTest <- function(){
  m = matrix(c(-1, 1, 1,
               -1,-0, 1,
               -1, 0, 0), nrow = 3, ncol = 3);
  testForValue(checkForVictory(m), -1, "oppositeWinnerTest")
}

advancedCheckTest <- function(){
  m = matrix(c(0,-1,-1,-1,
               1, 0, 1, 0, 
               0, 1, 0, 0,
               0, 0, 0, 0), nrow = 4, ncol = 4)
  testForValue(checkForVictory(m), -1, "advancedCheckTest")
}

basicColumnDetectionTest()
basicRowDetectionTest()
basicDiagonalDetectionTest()
basicSecondDiagonalDetectionTest()
nothignInterestingTest()
oppositeWinnerTest()