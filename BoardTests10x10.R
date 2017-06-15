#For boardSize==10 and winningSeries>=5

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
  m = matrix(c(1, 1, 1, 1, 1, 0, 0, 0, 0, 0,
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow = 10, ncol = 10);
  testForValue(checkForVictory(m), 1, "basicRowDetectionTest")
}

basicColumnDetectionTest <- function(){
  m = matrix(c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
               1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
               1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
               1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
               1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow = 10, ncol = 10);
  testForValue(checkForVictory(m), 1, "basicColumnDetectionTest")
}

basicDiagonalDetectionTest <- function(){
  m = matrix(c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
               0, 1, 0, 0, 0, 0, 0, 0, 0, 0,
               0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
               0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
               0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow = 10, ncol = 10);
  testForValue(checkForVictory(m), 1, "basicDiagonalDetectionTest")
}

basicSecondDiagonalDetectionTest <- function(){
  m = matrix(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
               0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
               0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
               0, 0, 0, 0, 0, 0, 1, 0, 0, 0,
               0, 0, 0, 0, 0, 1, 0, 0, 0, 0,
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow = 10, ncol = 10);
  testForValue(checkForVictory(m), 1, "basicSecondDiagonalDetectionTest")
}

nothignInterestingTest <- function(){
  m = matrix(c(0, 1, 1,-1, 0, 0, 1, 0, 0, 0,
               0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
               0, 0, 0, 1,-1, 0, 0, 0, 0, 0,
               0, 0, 0, 1, 0, 0, 1, 0, 0, 0,
               0, 0,-1, 0, 0, 0, 0, 0, 0, 0,
               0, 1, 0, 0, 0, 0, 1, 0, 0, 0,
               1, 1,-1,-1,-1,-1, 1, 0, 0, 0,
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow = 10, ncol = 10);
  testForValue(checkForVictory(m), 0, "nothignInterestingTest")
}

oppositeWinnerTest <- function(){
  m = matrix(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
               0,-1, 0, 0, 0, 0, 0, 0, 0, 0,
               0, 0,-1, 0, 0, 0, 0, 0, 0, 0,
               0, 0, 0,-1, 0, 0, 0, 0, 0, 0,
               0, 0, 0, 0,-1, 0, 0, 0, 0, 0,
               0, 0, 0, 0, 0,-1, 0, 0, 0, 0,
               0, 0, 0, 0, 0,-1, 0, 0, 0, 0,
               0, 0, 0, 0, 0,-1, 0, 0, 0, 0), nrow = 10, ncol = 10);
  testForValue(checkForVictory(m), -1, "oppositeWinnerTest")
}

advancedCheckTest1 <- function(){
  m = matrix(c(1,-1, 1,-1, 1,-1, 1,-1, 0, 0,
               1,-1, 1,-1, 1,-1, 1,-1, 0, 0,
               1,-1, 1,-1, 1,-1, 1,-1, 0, 0,
               1,-1, 1,-1, 1,-1, 1,-1, 0, 0,
               0, 1,-1, 1,-1, 1,-1, 1, 0, 0,
               1, 1,-1, 1,-1, 1,-1, 1, 0, 0,
               1, 1,-1, 1,-1, 1,-1, 1, 0, 0,
               1, 1,-1, 1,-1, 1,-1, 1, 0, 0,
               0, 0, 0, 0, 0,-1, 0, 0, 0, 0,
               0, 0, 0, 0, 0,-1, 0, 0, 0, 0), nrow = 10, ncol = 10);
  testForValue(checkForVictory(m), 0, "advancedCheckTest1")
}

advancedCheckTest2 <- function(){
  m = matrix(c(1,-1, 1,-1, 1,-1, 1,-1, 0, 0,
               1,-1, 1,-1, 1,-1, 1,-1, 0, 0,
               1,-1, 1,-1, 1,-1, 1,-1, 0, 0,
               1,-1, 1,-1, 1,-1, 1,-1, 0, 0,
               0, 1,-1, 1,-1, 1,-1, 1, 0, 0,
               1, 1,-1, 1, 1, 1,-1, 1, 0, 0,
               1, 1,-1, 1,-1, 1,-1, 1, 0, 0,
               1, 1, 1, 1,-1, 1,-1, 1, 0, 0,
               0, 0, 0, 0, 0,-1, 0, 0, 0, 0,
               0, 0, 0, 0, 0,-1, 0, 0, 0, 0), nrow = 10, ncol = 10);
  testForValue(checkForVictory(m), 1, "advancedCheckTest2")
}

advancedCheckTest3 <- function(){
  m = matrix(c(1,-1, 1,-1, 1,-1, 1,-1, 0, 0,
               1,-1, 1,-1, 1,-1, 1,-1, 0, 0,
               1,-1, 1,-1, 1,-1, 1,-1, 0, 0,
               1,-1, 1,-1,-1,-1, 1,-1, 0, 0,
               0, 1,-1, 1,-1, 1,-1, 1, 0, 0,
               1, 1,-1, 1,-1, 1,-1, 1, 0, 0,
               1, 1,-1, 1,-1, 1,-1, 1, 0, 0,
               1, 1,-1, 1,-1, 1,-1, 1, 0, 0,
               0, 0, 0, 0, 0,-1, 0, 0, 0, 0,
               0, 0, 0, 0, 0,-1, 0, 0, 0, 0), nrow = 10, ncol = 10);
  testForValue(checkForVictory(m), -1, "advancedCheckTest3")
}

rightEvaluationTest <- function(){
  m = matrix(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
               0, 0, 0, 0, 0, 0, 0, 0, 0,-1,
               0, 0, 0, 0, 0, 0, 0, 0, 0,-1,
               0, 0, 0, 0, 0, 0, 0, 0, 0,-1,
               0, 0, 0, 0, 0, 0, 0, 0, 0,-1,
               0, 0, 0, 0, 0, 0, 0, 0, 0,-1,
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
               0, 0, 0, 0, 0,-1, 0, 0, 0, 0,
               0, 0, 0, 0, 0,-1, 0, 0, 0, 0), nrow = 10, ncol = 10);
  testForValue(checkForVictory(m), -1, "rightEvaluationTest")
}

bottomEvaluationTest <- function(){
  m = matrix(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
               0, 0,-1,-1,-1,-1,-1, 0, 0, 0), nrow = 10, ncol = 10);
  testForValue(checkForVictory(m), -1, "bottomEvaluationTest")
}

basicColumnDetectionTest()
basicRowDetectionTest()
basicDiagonalDetectionTest()
basicSecondDiagonalDetectionTest()
nothignInterestingTest()
oppositeWinnerTest()
advancedCheckTest1()
advancedCheckTest2()
advancedCheckTest3()
rightEvaluationTest()
bottomEvaluationTest()