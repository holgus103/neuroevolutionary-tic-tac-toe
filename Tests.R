testForValue <- function(expected, actual, testname){
  res = "failed"
  if(actual == expected){
    res = "passed"
  }
  print(paste(testname, res))
}

basicRowDetectionTest <- function(){
  m = matrix(c(1, 1, 1, -1, -1, 1, -1, -1, 0), nrow = 3, ncol = 3);
  testForValue(checkSubMatrix(m), 1, "basicRowDetectionTest")
}

basicColumnDetectionTest <- function(){
  m = matrix(c(1, -1, 0, 1, 0, -1, 1, -1, 0), nrow = 3, ncol = 3);
  testForValue(checkSubMatrix(m), 1, "basicColumnDetectionTest")
}

basicDiagonalDetectionTest <- function(){
  m = matrix(c(1, -1, 0, 0, 1, -1, 0, -1, 1), nrow = 3, ncol = 3);
  testForValue(checkSubMatrix(m), 1, "basicDiagonalDetectionTest")
}

basicSecondDiagonalDetectionTest <- function(){
  m = matrix(c(0, -1, 1, 0, 1, -1, 1, -1, 0), nrow = 3, ncol = 3);
  testForValue(checkSubMatrix(m), 1, "basicSecondDiagonalDetectionTest")
}


basicColumnDetectionTest()
basicRowDetectionTest()
basicDiagonalDetectionTest()
basicSecondDiagonalDetectionTest()