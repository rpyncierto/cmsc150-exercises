# implementation of Gaussian Elimination
GaussianElimination <- function(matrix) {
  # get number of rows
  nrow <- nrow(matrix)
  # get number of columns
  ncol <- ncol(matrix)
  # implement forward elimination
  for (i in 1:(nrow-1)) {
    # get the pivot element of the matrix
    pivot <- max(abs(matrix[i:nrow, i]))
    # get the row index of the pivot element
    pivotrow <- which(abs(matrix[i:nrow, i]) == pivot) + i - 1
    # check if pivot element is present in multiple rows
    if (length(pivotrow) > 1) {
      # get the index of the first instance of the pivot element
      pivotrow <- pivotrow[1]
    }
    # check if the pivot element is equal to 0,
    # if yes, print "No unique solution exists"
    if (matrix[pivotrow, i] == 0) {
      print("No unique solution exists")
      return(NA)
    }
    # swap the pivot row with the current row
    temp <- matrix[i, ]
    matrix[i, ] <- matrix[pivotrow, ]
    matrix[pivotrow, ] <- temp
    # perform row operations to eliminate the elements
    # below the pivot element
    for (j in (i+1):nrow) {
      # find the multiplier
      multiplier <- matrix[j, i] / matrix[i, i]
      # subtract the multiplier times the current row from the normal row
      matrix[j, ] <- matrix[j, ] - multiplier * matrix[i, ]
    }
  }
  # create a vector of zeros to store the solution
  solutionSet <- rep(0, nrow)
  # implement backward substitution
  for (i in nrow:1) {
    # get the solution for the current row
    solutionSet[i] <- matrix[i, ncol] / matrix[i, i]
    # substitute the solution in the remaining equations
    for (j in 1:(i-1)) {
      matrix[j, ncol] <- matrix[j, ncol] - matrix[j, i] * solutionSet[i]
    }
  }
  return(solutionSet)
}

# implementation of Gauss-Jordan Elimination
GaussJordanElimination <- function(matrix) {
  # get number of rows
  nrow <- nrow(matrix)
  # get number of column
  ncol <- ncol(matrix)
  for (i in 1:nrow) {
    if (i != nrow) {
      # get the pivot element of the matrix
      pivot <- max(abs(matrix[i:nrow, i]))
      # get the row index of the pivot element
      pivotrow <- which(abs(matrix[i:nrow, i]) == pivot) + i - 1
      # check if pivot element is present in multiple rows
      if (length(pivotrow) > 1) {
        # get the index of the first instance of the pivot element
        pivotrow <- pivotrow[1]
      }
      # check if the pivot element is equal to 0,
      # if yes, print "No unique solution exists"
      if (matrix[pivotrow, i] == 0) {
        print("No unique solution exists")
        # stop the execution of the function and return
        return(NA)
      }
      # swap the pivot row with the current row
      temp <- matrix[i, ]
      matrix[i, ] <- matrix[pivotrow, ]
      matrix[pivotrow, ] <- temp
    }
    # perform row operations to normalize
    matrix[i, ] <- matrix[i, ] / matrix[i, i]
    for (j in 1:nrow) {
      if (j != i) {
        # find the multiplier
        multiplier <- matrix[j, i]
        # subtract the multiplier times the current row
        # from the normal row
        matrix[j, ] <- matrix[j, ] - multiplier * matrix[i, ]
      }
    }
  }
  # create a vector of zeros to store the solution
  solutionSet <- rep(0, nrow)
  # iterate RHS of the augmented coefficient matrix and 
  # store the values to the solution vector
  for (i in 1:nrow) {
    solutionSet[i] <- matrix[i, ncol]
  }
  return(solutionSet)
}

# implementation of polynomial regression
PolynomialRegression <- function(x, y, degree) {
  # check if the integer input is less than one
  # if yes, return NA
  if (degree < 1) {
    return(NA)
  }
  # check if the length of x and y are equal
  # if not, return NA
  if (length(x) != length(y)) {
    return(NA)
  }
  # get the number of data points
  n <- length(x)
  # create a matrix of zeros
  matrix <- matrix(0, nrow = degree + 1, ncol = degree + 2)
  # create a vector of zeros
  values <- rep(0, degree * 2)
  # fill the vector with values to be inserted in the matrix
  for (i in 1:(degree * 2)) {
    values[i] <- sum(x ^ i)
  }
  # create a vector of zeros (RHS)
  RHS <- rep(0, ncol(matrix))
  # fill the vector with values to be inserted in the matrix (RHS)
  for (i in 1:ncol(matrix)) {
    RHS[i] <- sum(x ^ (i - 1) * y)
  }
  # store the values in the matrix
  for (i in 1:nrow(matrix)) {
    for (j in 1:ncol(matrix)) {
      if (i == 1 && j == 1) { # [1, 1] element is the number of data points
        matrix[i, j] <- n
      } else if (j == ncol(matrix)) { # RHS
        matrix[i, j] <- RHS[i]
      } else {
        matrix[i, j] <- values[i + j - 2]
      }
    }
  }
  # ==========================================================================
  # get the solution set/coefficient after the elimination process
  # uncomment depends on the elimination method
  
  # using GaussianElimination
  # coefficients <- GaussianElimination(matrix)
  
  # using GaussJordanElimination
  coefficients <- GaussJordanElimination(matrix)
  # ==========================================================================
  # a string to store the function
  functionString <- "function (x) "
  # iterate the solution set and create the function
  for (i in 1:length(coefficients)) {
    if (i < length(coefficients)) {
      functionString <- paste(functionString, coefficients[i], " * x^", i - 1, " + ", sep = "")
    } else {
      functionString <- paste(functionString, coefficients[i], " * x^", i - 1, sep = "")
    }
    
  }
  # convert the string to a function to evaluate
  funct <- eval(parse(text = functionString))
  print(coefficients)
  cat("\n")
  # returns the function in which when run, can compute the value of the regression polynomial with the given x
  return(funct)
}

# test case
# x = c(0,1,2,3,4,5) #independent variable
# y = c(2.1,7.7,13.6,27.2,40.9,61.1)#dependent variable
# result <- PolynomialRegression(x, y, 2)
# print(result)
# cat("\n")
# result(2.5)
