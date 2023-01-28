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

poly.qsi <- function(data, x) {
  n <- length(data[[1]])-1 # of intervals or data points - 1
  equations <- list() # a list to store the equations
  function_string = "function ("
  
  # getting the equation of the first endpoint (condition 2)
  func <- paste(function_string, "a1, b1, c1) ", sep = "")
  eqtn <- paste(data[[1]][1]^2, " * a1 + ", data[[1]][1], " * b1 + 1 * c1 + -", data[[2]][1], sep = "")
  equations <- append(equations, eval(parse(text = paste(func, eqtn, sep = ""))))
  
  # getting the equation of the internal knots (condition 1)
  for (i in 2:n) {
    func <- paste(function_string, "a", i-1, ", b", i-1, ", c", i-1, ") ", sep = "")
    eqtn <- paste(data[[1]][i]^2, " * a", i-1, " + ", data[[1]][i], " * b", i-1, " + 1 * c", i-1, " + -", data[[2]][i], sep = "")
    equations <- append(equations, eval(parse(text = paste(func, eqtn, sep = ""))))
    func <- paste(function_string, "a", i, ", b", i, ", c", i, ") ", sep = "")
    eqtn <- paste(data[[1]][i]^2, " * a", i, " + ", data[[1]][i], " * b", i, " + 1 * c", i, " + -", data[[2]][i], sep = "")
    equations <- append(equations, eval(parse(text = paste(func, eqtn, sep = ""))))
  }
  
  # getting the equation at the last endpoint (condition 2)
  func <- paste(function_string, "a", n, ", b", n, ", c", n, ") ", sep = "")
  eqtn <- paste(data[[1]][n+1]^2, " * a", n, " + ", data[[1]][n+1], " * b", n, " + 1 * c", n, " + -", data[[2]][n+1], sep = "")
  equations <- append(equations, eval(parse(text = paste(func, eqtn, sep = ""))))
  
  # getting the equation where we equal first derivative at the interior knots (condition 3)
  for (i in 2:n) {
    func <- paste(function_string, "a", i-1, ", a", i, ", b", i-1, ", b", i, ") ", sep = "")
    eqtn <- paste(data[[1]][i]*2, " * a", i-1, " + ", 1, " * b", i-1, " + -", data[[1]][i]*2, " * a", i, " + -1 * b", i, " + 0",sep = "")
    equations <- append(equations, eval(parse(text = paste(func, eqtn, sep = ""))))
  }
  variables = list() # a list to store the variables
  for (i in 1:length(equations)) {
    variables <- append(variables, names(formals(equations[[i]])))
  }
  variables <- unique(variables) # get the unique variables
  for (i in 1:length(variables)) { # remove a1 since it is assumed as 0 (condition 4)
    if (variables[[i]] == "a1") {
      variables <- variables[-i]
      break
    }
  }
  col_names <- c(unlist(variables), "RHS") # add RHS as column name
  aug_coeff_matrix <- matrix(0, nrow = length(equations), ncol = length(col_names), dimnames = list(1:length(equations), col_names)) # create a matrix
  for (i in 1:length(equations)) {
    equation_vector <- unlist(strsplit(deparse(equations[[i]])[2], "\\ \\+ ")) # split the equations
    for (j in 1:length(equation_vector)) {
      term <- unlist(strsplit(equation_vector[j], "\\ \\* ")) # split to terms
      if (length(term) == 1) {
        aug_coeff_matrix[i, col_names == "RHS"] <- eval(parse(text = term[1])) * (-1) # add to RHS
      } else {
        if (!term[2] %in% col_names) { # in case of a1 is the variable, since we assume that a1 = 0 (condition 4)
          next
        } else {
          aug_coeff_matrix[i, term[2]] <- eval(parse(text = term[1])) # add the coefficients to their respective place in matrix
        }
      }
    }
  }
  solution <- GaussJordanElimination(aug_coeff_matrix) # solution set returned after Gauss-Jordan Elimination
  solution <- c(0, solution) # assuming that the second derivative is 0 at the first point
  qsi.fxns <- list() # a list to store the quadratic functions of each interval
  j = 1
  for (i in 1:n) { # creation of quadratic functions for the interval
    func <- paste(function_string, "x) ", sep = "")
    eqtn <- paste(solution[j], " * x^2 + ", solution[j+1], " * x + ", solution[j+2], sep = "")
    qsi.fxns <- append(qsi.fxns, eval(parse(text = paste(func, eqtn, sep = ""))))
    j <- j+3
  }
  if (x < data[[1]][1] || x > data[[1]][length(data[[1]])]) { # check if x is within the range of the data points
    y = NA
  } else {
    j = 1
    for (i in 1:n) {
      if (x >= data[[1]][j] && x <= data[[1]][j+1]) { # check what interval is x included
        y = eval(parse(text = paste("qsi.fxns[[", i, "]](x)", sep = ""))) # evaluate
    }
    j = j + 1
    }
  }
  info = list( # a labeled list containing the quadratic functions of each interval as well as the predicted value of f(x)
    qsi.fxns = qsi.fxns,
    y = y
  )
  return (info)
}

# # TEST CASES
# x = c(3.0,4.5,7.0,9.0)
# y = c(2.5,1.0,2.5,0.5)
# data1 = list(x,y)
# 
# poly.qsi(data1,5)$y #0.66
# 
# poly.qsi(data1,8)$y #3.1
# 
# poly.qsi(data1,8.5)$y #2.2
# 
# poly.qsi(data1,20)$y #NA
# 
# poly.qsi(data1,3.0)$y #2.5, the given data
# 
# # ------------------------------------
#   
# x = c(3.0,4.5,7.0,9.0,11.0,13.0)
# y = c(2.5,1.0,2.5,0.5,4.0,4.6)
# data2 = list(x,y)
# 
# poly.qsi(data2,10)$y #-0.72
# 
# poly.qsi(data2,12)$y # 8