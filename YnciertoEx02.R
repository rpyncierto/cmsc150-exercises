# Reymond P. Yncierto
# CMSC 150 X2L
# Exercise 2: This program implements various matrices operations using a matrix as the primary object to work on.

# This function determines if a matrix is a square matrix
# requirement: matrix
# result: returns TRUE if matrix is a square matrix, otherwise FALSE
SquareMatrix <- function(mat) { 
  if (is.matrix(mat)) { # if mat is a matrix
    if (nrow(mat) == ncol(mat)) { # if mat is a square matrix (has equal number of rows and columns)
      return(TRUE)
    }
  }
  return(FALSE)
}

# This function solves the minor of matrix with respect to i and j
# requirement: matrix, i and j
# result: returns the minor of matrix with respect to i and j
MatrixMinor <- function(mat, i, j) {
  if (SquareMatrix(mat)) { # if mat is a square matrix
    minor <- mat[-i, -j] # remove the ith row and jth column
    return(minor)
  }
  return(NULL) # mat is not a square matrix
}

# This function solves the cofactor of the matrix with respect to i and j
# requirement: matrix, i and j
# result: returns the cofactor of matrix with respect to i and j
MatrixCofactor <- function(mat, i, j) {
  if (SquareMatrix(mat)) { # if mat is a square matrix
    minor <- MatrixMinor(mat, i, j) # solves the minor of mat with respect to i and j
    cofactor <- (-1)^(i+j) * det(minor) # solves the cofactor of mat with respect to i and j
    return(cofactor)
  }
  return(NULL) # mat is not a square matrix
}

# This function solves the adjoint of the matrix
# requirement: matrix
# result: returns the adjoint of the matrix if matrix is a square matrix, otherwise NA
MatrixAdjoint <- function(mat) {
  if (SquareMatrix(mat)) { # if mat is a square matrix
    adjoint <- matrix(0, nrow(mat), ncol(mat)) # generate a matrix that is identical to mat
    for (i in 1:nrow(mat)) { # loop through the rows of mat
      for (j in 1:ncol(mat)) { # loop through the columns of mat
        adjoint[i, j] <- MatrixCofactor(mat, i, j) # solves the cofactor of mat with respect to i and j
      }
    }
    return(adjoint)
  }
  return(NA) # mat is not a square matrix
}

# This function solves the inverse of the matrix
# requirement: matrix
# result: returns the inverse of the matrix if the matrix is a square matrix, otherwise NA
MatrixInverse <- function(mat) {
  if (SquareMatrix(mat)) { # if mat is a square matrix
    adjoint <- t(MatrixAdjoint(mat)) # get the adjoint of mat
    inverse <- adjoint / det(mat) # get the inverse of mat
    return(inverse)
  }
  return(NA) # mat is not a square matrix
}

mat1 = matrix(c(2, 0, 2, -3, -1, -4, 4, 1, 3), nrow=3, ncol=3)
mat2 = matrix(c(2,6,3,1,-8,9,0,2,4,5,1,-2,-5,2,4,-6), nrow=4, ncol=4)
mat3 = matrix(1:8,nrow=2,ncol=4)

MatrixCofactor(mat1, 1, 2)
SquareMatrix(mat3)
MatrixInverse(mat1)
MatrixInverse(mat2)
MatrixInverse(mat3)

# Word Problem
# mat4 = matrix(c(3, 5, 1, -7, -2, -4, 5, 2, 0), nrow=3, ncol=3)
# MatrixInverse(mat4)