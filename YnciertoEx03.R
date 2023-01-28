AugCoeffMatrix <- function(system) {
  # get variables involved in the system of mathematical equations
  variables <- list() # create an empty list
  for (i in 1:length(system)) {
      variables[[i]] <- names(as.list(formals(system[[i]]))) # get variables from the argument of the mathematical functions and store it in the list earlier
  }
  variables <- unique(variables) # get unique variables (to avoid redundancy and to check if all the equations have the same number of unknown variables)
  
  # check if each equation has the same number of variables
  if (length(variables) != 1) {
      return(NA) # means that the mathematical equations does not have the same number of unknown variables
  }
  
  # create a matrix with the variables and the RHS as column names and number of mathematical equations as number of rows
  colNames <- c(unlist(variables), "RHS") # concatenate the variables from earlier and the RHS
  augcoeffmatrix <- matrix(0, nrow = length(variables[[1]]), ncol = length(variables[[1]])+1, dimnames = list(c(1:length(variables[[1]])), colNames)) # create a matrix initialized with 0 with the variables and the RHS as column names, and number of equation as number of rows

  # fill the matrix with the coefficients
  for (i in 1:length(system)) {
      systemVector <- unlist(strsplit(deparse(system[[i]])[2], "\\ \\+ ")) # convert the mathematical function to a string and split it by the " + " sign (including the spaces before and after the delimiter)
      for (j in 1:length(systemVector)) { 
          term <- unlist(strsplit(systemVector[j], "\\ \\* ")) # split the terms by the " * " sign (including the spaces before and after the delimiter)
          coordinate <- eval(parse(text = gsub("x", "", term[2]))) # get the position of the term in the matrix by the subscript of the variable x
          coefficients <- eval(parse(text = term[1])) # get the coefficient of the term
          if (is.na(coordinate)) { 
            augcoeffmatrix[i, length(system)+1] <- (-1)*coefficients # if the coordinates is NA, it is in the RHS, multiply the coefficient by -1 since it will be transpose to the right side of the mathematical equation
          } else {
            augcoeffmatrix[i, coordinate] <- coefficients # if the coordinates is not NA, it is in the matrix
          }
      }
  }
  print(variables)
  print(augcoeffmatrix)
}

# TEST CASES
E1 <- function (x1, x2, x3) 0.3 * x1 + -0.2 * x2 + 10 * x3 + -71.4
E2 <- function (x1, x2, x3) 3 * x1 + -0.2 * x3 + -0.1 * x2 + -7.85;
E3 <- function (x1, x2, x3) 0.1 * x1 + 7 * x2 + -0.3 * x3 + 19.3;
system1 <- list(E1, E2, E3)

E4 <- function (x1, x2, x3, x4) 1 * x1 + 0 * x2 + 2 * x3 + 3 * x4 + -90;
E5 <- function (x1, x2, x3, x4) -1 * x1 + 2 * x3 + 0 * x2 + 3 * x4 + 90;
E6 <- function (x1, x2, x3, x4) 1 * x1 + 0 * x2 + 2 * x3 + 3 * x4 + -90;
E7 <- function (x1, x2, x3, x4) -1 * x1 + 2 * x3 + 0 * x2 + 3 * x4 + 90;
system2 <- list(E4, E5, E6, E7)
system3 <- list(E4, E5, E6, E3)

AugCoeffMatrix(system1)
AugCoeffMatrix(system2)
AugCoeffMatrix(system3)