# import augmented coefficient matrix script from exer 3
AugCoeffMatrix <- function(system) {
    # get variables involved in the system of mathematical equations
    variables <- list()
    for (i in 1:length(system)) {
        # get variables from the argument of the mathematical functions and
        # store it in the list earlier
        variables[[i]] <- names(as.list(formals(system[[i]])))
    }
    # get unique variables (to avoid redundancy and to check if all
    # the equations have the same number of unknown variables)
    variables <- unique(variables)

    # check if each equation has the same number of variables
    if (length(variables) != 1) {
        # means that the mathematical equations does not have
        # the same number of unknown variables
        return(NA) 
    }
    # create a matrix with the variables and the RHS as column names and
    # number of mathematical equations as number of rows
    # concatenate the variables from earlier and the RHS
    colNames <- c(unlist(variables), "RHS")
    # create a matrix initialized with 0 with the variables and
    # the RHS as column names, and number of equation as number of rows
    augcoeffmatrix <- matrix(0, nrow = length(variables[[1]]), ncol = length(variables[[1]])+1, 
    dimnames = list(c(1:length(variables[[1]])), colNames))
    # get the equations from the functions
    systemVector <- paste(lapply(system, body), sep = " ")
    # iterate through the system of equations
    for (i in 1:length(system)) {
        # split the equation by the " + " sign
        # (including the spaces before and after the delimiter)
        Vector <- unlist(strsplit(systemVector[i], "\\ \\+ "))
        # iterate through the terms
        for (j in 1:length(Vector)) { 
            # split the term by the " * " sign
            # (including the spaces before and after the delimiter)
            term <- unlist(strsplit(Vector[j], "\\ \\* "))
            # get the position of the term in the matrix by the subscript of
            # the variable x
            coordinate <- eval(parse(text = gsub("x", "", term[2])))
            # get the coefficient of the term
            coefficients <- eval(parse(text = term[1]))
            if (is.na(coordinate)) {
                # if the coordinates is NA, it is in the RHS, multiply the
                # coefficient by -1 since it will be transpose to the right
                # side of the mathematical equation
                augcoeffmatrix[i, length(system)+1] <- (-1) *coefficients
            } else {
                # if the coordinates is not NA, it is in the matrix
                augcoeffmatrix[i, coordinate] <- coefficients
            }
        }
        
    }
    return(augcoeffmatrix)
}

# get the variables in the system of linear equations
GetVariables <- function(system) {
    # create an empty list
    variables <- list()
    # get variables from the argument of the mathematical functions
    # and store it in the list earlier
    for (i in 1:length(system)) {
        variables[[i]] <- names(as.list(formals(system[[i]])))
    }
    # convert list to vector
    variables <- unlist(variables)
    # remove duplicates
    variables <- unique(variables)
    # return the variables
    return(variables)
}


# implementation of Gaussian Elimination
GaussianElimination <- function(system) {
    # get the augmented coefficient matrix of the system of linear equations
    augcoeffmatrix <- AugCoeffMatrix(system)
    # get the variables in the system of linear equations
    variables <- GetVariables(system)
    # get number of rows
    nrow <- nrow(augcoeffmatrix)
    # get number of columns
    ncol <- ncol(augcoeffmatrix)
    # implement forward elimination
    for (i in 1:(nrow-1)) {
        # get the pivot element of the matrix
        pivot <- max(abs(augcoeffmatrix[i:nrow, i]))
        # get the row index of the pivot element
        pivotrow <- which(abs(augcoeffmatrix[i:nrow, i]) == pivot) + i - 1
        # check if pivot element is present in multiple rows
        if (length(pivotrow) > 1) {
            # get the index of the first instance of the pivot element
            pivotrow <- pivotrow[1]
        }
        # check if the pivot element is equal to 0,
        # if yes, print "No unique solution exists"
        if (augcoeffmatrix[pivotrow, i] == 0) {
            print("No unique solution exists")
            return(NA)
        }
        # swap the pivot row with the current row
        temp <- augcoeffmatrix[i, ]
        augcoeffmatrix[i, ] <- augcoeffmatrix[pivotrow, ]
        augcoeffmatrix[pivotrow, ] <- temp
        # perform row operations to eliminate the elements
        # below the pivot element
        for (j in (i+1):nrow) {
            # find the multiplier
            multiplier <- augcoeffmatrix[j, i] / augcoeffmatrix[i, i]
            # subtract the multiplier times the current row from the normal row
            augcoeffmatrix[j, ] <- augcoeffmatrix[j, ] - multiplier * augcoeffmatrix[i, ]
        }
    }
    # create a vector of zeros to store the solution
    solutionSet <- rep(0, length(GetVariables(system)))
    # matrix before backward substitution
    matrix <- augcoeffmatrix
    # implement backward substitution
    for (i in nrow:1) {
        # get the solution for the current row
        solutionSet[i] <- augcoeffmatrix[i, ncol] / augcoeffmatrix[i, i]
        # substitute the solution in the remaining equations
        for (j in 1:(i-1)) {
            augcoeffmatrix[j, ncol] <- augcoeffmatrix[j, ncol] - augcoeffmatrix[j, i] * solutionSet[i]
        }
    }
    print(solutionSet)
    print(variables)
    print(matrix)
}

# implementation of Gauss-Jordan Elimination
GaussJordanElimination <- function(system) {
    # get the augmented coefficient matrix of the system of linear equations
    augcoeffmatrix <- AugCoeffMatrix(system)
    # get the variables in the system of linear equations
    variables <- GetVariables(system)
    # get number of rows
    nrow <- nrow(augcoeffmatrix)
    # get number of column
    ncol <- ncol(augcoeffmatrix)
    for (i in 1:nrow) {
        if (i != nrow) {
            # get the pivot element of the matrix
            pivot <- max(abs(augcoeffmatrix[i:nrow, i]))
            # get the row index of the pivot element
            pivotrow <- which(abs(augcoeffmatrix[i:nrow, i]) == pivot) + i - 1
            # check if pivot element is present in multiple rows
            if (length(pivotrow) > 1) {
                # get the index of the first instance of the pivot element
                pivotrow <- pivotrow[1]
            }
            # check if the pivot element is equal to 0,
            # if yes, print "No unique solution exists"
            if (augcoeffmatrix[pivotrow, i] == 0) {
                print("No unique solution exists")
                # stop the execution of the function and return
                return(NA)
            }
            # swap the pivot row with the current row
            temp <- augcoeffmatrix[i, ]
            augcoeffmatrix[i, ] <- augcoeffmatrix[pivotrow, ]
            augcoeffmatrix[pivotrow, ] <- temp
        }
        # perform row operations to normalize
        augcoeffmatrix[i, ] <- augcoeffmatrix[i, ] / augcoeffmatrix[i, i]
        for (j in 1:nrow) {
            if (j != i) {
                # find the multiplier
                multiplier <- augcoeffmatrix[j, i]
                # subtract the multiplier times the current row
                # from the normal row
                augcoeffmatrix[j, ] <- augcoeffmatrix[j, ] - multiplier * augcoeffmatrix[i, ]
            }
        }
    }
    # create a vector of zeros to store the solution
    solutionSet <- rep(0, length(GetVariables(system)))
    # iterate RHS of the augmented coefficient matrix and 
    # store the values to the solution vector
    for (i in 1:nrow) {
        solutionSet[i] <- augcoeffmatrix[i, ncol]
    }
    print(solutionSet)
    print(variables)
    print(augcoeffmatrix)
}

# # TEST CASES
# E1 <- function (x1, x2, x3) 0.1 * x1 + 7 * x2 + -0.3 * x3 + 19.3;
# E2 <- function (x1, x2, x3) 3 * x1 + -0.1 * x2 + -0.2 * x3 + -7.85;
# E3 <- function (x1, x2, x3) 0.3 * x1 + -0.2 * x2 + 10 * x3 + -71.4;
# system1 = list(E1,E2,E3)

# #Solution Set of system1
# #[1]  3.0 -2.5  7.0

# F1 <- function(x1, x2, x3, x4, x5) 1 * x1 + 1 * x2 + 1 * x3 + 1 * x4 + 1 * x5 + -500;
# F2 <- function(x1, x2, x3, x4, x5) 0.1 * x1 + 0.4 * x2 + 0.6 * x3 + 0.5 * x4 + 0.8 * x5 + -225;
# F3 <- function(x1, x2, x3, x4, x5) 1 * x1 + -2 * x2 + 0 * x3 + 0 * x4 + 0 * x5 + 0;
# F4 <- function(x1, x2, x3, x4, x5) 0 * x1 + 0 * x2 + -3 * x3 + 0 * x4 + 1 * x5 + 0;
# F5 <- function(x1, x2, x3, x4, x5) 0 * x1 + -1 * x2 + -1 * x3 + 1 * x4 + 0 * x5 + 0;
# system2 <- list(F1, F2, F3, F4, F5)

# #Solution Set of system2
# #[1] 147.05882  73.52941  41.17647 114.70588 123.52941

# # Using Gauss Elimination
# print("---Gaussian Elimination---")
# GaussianElimination(system1)
# GaussianElimination(system2)
# print("---Gauss-Jordan Elimination---")
# GaussJordanElimination(system1)
# GaussJordanElimination(system2)