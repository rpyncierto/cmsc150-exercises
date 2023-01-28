simplex <- function(tableau, isMax, problem) {
  col_names <- c("s1","s2","s3","s4","s5","s6","s7","s8","x1","x2","x3","x4","x5","x6","x7","x8","x9","x10","x11","x12","x13","x14", "s15", "Z", "Solution")
  if (problem) {
    colnames(tableau) <- col_names # hard coded column names for problem inputs
  } else if (isMax) {
    colnames(tableau) <- c(paste0("x", 1:(ncol(tableau)-(nrow(tableau)+1))), paste0("S", 1:(nrow(tableau)-1)), "Z", "Solution") # maximization column names
  } else {
    colnames(tableau) <- c(paste0("S", 1:(nrow(tableau)-1)), paste0("x", 1:(ncol(tableau)-(nrow(tableau)+1))), "Z", "Solution") # minimization column names
  }
  solutions <- list() # to store the basic solutions
  
  while (min(tableau[nrow(tableau),]) < 0) { # as long as the minimum in the last row is less then 0, then this will iterate
    
    pivot_col <- which(tableau[nrow(tableau), 1:ncol(tableau)-1] < 0) # get the columns with negative values excluding the last column (solution column)
    pivot_col <- pivot_col[which.max(abs(tableau[nrow(tableau), pivot_col]))] # get the column with the highest magnitude number among the columns with negative values
    
    pivot_row <- which(tableau[1:(nrow(tableau)-1), pivot_col] > 0) # get the rows with positive values excluding the last row
    pivot_row <- pivot_row[which.min(tableau[pivot_row, ncol(tableau)] / tableau[pivot_row, pivot_col])] # get the row with the smallest ratio
    
    if (length(pivot_row) == 0) { # the iteration cannot go on because there is no possible positive test ratio thus, no solution is feasible
      return ("No feasible solution")
    }
    
    pivot_element <- tableau[pivot_row, pivot_col] # pivot element
    
    tableau[pivot_row, ] <- tableau[pivot_row, ] / pivot_element # normalize the pivot row
    
    for (i in 1:nrow(tableau)) { # elimination of other rows
      if (i != pivot_row) { # if i == pivot row skip since it is already normalize
        multiplier <- tableau[i, pivot_col]
        tableau[i, ] <- tableau[i, ] - multiplier * tableau[pivot_row, ]
      }
    } 
    
    solution <- c() # to store the solutions on every iteration
    
    if (isMax) { # if maximization
      for (i in 1:(ncol(tableau)-1)) { # iterate the columns
        if (sum(tableau[1:nrow(tableau), i]) == 1) { 
          solution <- c(solution, tableau[which(tableau[1:(nrow(tableau)), i] == 1), ncol(tableau)]) # store the solution value of the row to the solution
        } else {
          solution <- c(solution, 0) # store 0 as solution
        }
      }
    } else { # if minimization
      solution <- tableau[nrow(tableau), 1:ncol(tableau)]
    }
    solutions <- c(solutions, list(solution)) # append solution as a list in the solutions list
  }
  
  solutions <- do.call(rbind, solutions) # row bind the solutions
  
  column_names <- colnames(tableau) # use the column names of the tableau to the column names of the basic solutions
  colnames(solutions) <- column_names[1:(ncol(solutions))]
  
  info <- list( # labeled list
    final.tableau = tableau,
    basic.solution = solutions,
    opt.val = as.numeric(tableau[nrow(tableau), ncol(tableau)])
  )
  
  if(problem) { # if true will include the shipping number per cell in matrix form
    opt_sol <- solutions[nrow(solutions),] # get the optimal solution
    shipping <- c() # vector to store the solution
    for (i in 9:(length(opt_sol)-2)) { # hard coded way of assigning the optimal solutions to the warehouses and plants
      shipping <- c(shipping, opt_sol[i])
    }
    shipping <- matrix(shipping, nrow = 3, byrow = TRUE, dimnames = list(c("DEN", "PHO", "DAL"), c("SAC", "SL", "ALB", "CHI", "NYC"))) # making it as matrix
    info <- c(info, list(shipping.num = shipping)) # add shipping.num to the info labeled list
  }
  return (info)
}

# ========================================================================================================================================================== #
# below is a function used to generate the initial tableau of the test cases used                                                                            #
#                                                                                                                                                            #
# THIS IS NOT A GENERAL FUNCTION for all test cases, thus only used to generate the initial tableau of the test cases used                                   #
#                                                                                                                                                            #
# this DOES NOT GUARANTEE THAT THIS WILL WORK ON ALL TESTCASES just solely used to help in testing different test cases to avoid hardcoding of the tableau   #
#                                                                                                                                                            #
# IF USED and the return values do not match PLEASE input the initial tableau that you have directly on the function above                                   #
#                                                                                                                                                            #
# REMEMBER: THIS IS NOT YET TESTED FOR ACCURACY, only used to setup the initial tableau of the test cases used                                               #
#                                                                                                                                                            #
# if there are inquiry about the implementation below, kindly contact the author                                                                             #
#                                                                                                                                                            #
# @Reymond P. Yncierto                                                                                                                                       #
# rpyncierto@up.edu.ph                                                                                                                                       #
# ========================================================================================================================================================== #


createTableau <- function(matrix, isMax, problem) {
  if(!isMax) { # minimization
    if (problem) { # problem
      matrix[1:3, 1:(ncol(matrix)-1)] <-  matrix[1:3, 1:(ncol(matrix)-1)] * (-1) # multiply some constraints to -1 to fully transform it into minimization
      matrix[4:(nrow(matrix)-1), ncol(matrix)] <- matrix[4:(nrow(matrix)-1), ncol(matrix)] * (-1)
      matrix <- t(matrix) # transpose the matrix
    } else {
      matrix <- t(matrix) # transpose the matrix
      matrix[nrow(matrix),] <- matrix[nrow(matrix),] * (-1) # multiply the last row to -1
    }
  } else {
    matrix[nrow(matrix),] <- matrix[nrow(matrix),] * (-1) # multiply the last row to -1
  }
  Solution <- matrix[, ncol(matrix)] # get the last column
  matrix <- matrix[, 1:(ncol(matrix) - 1)] # remove the last column
  matrix <- cbind(matrix, diag(nrow(matrix))) # apply slack variables
  matrix <- cbind(matrix, Solution) # include again the last column
  simplex(matrix, isMax, problem)# call the simplex method function
}

# ================================================================================================================================================== #
# The function above is used to generate the initial tableau of the test cases below, transportation problem are hard coded without considering the  #
# inequalities as it is already hard coded in the implementation above                                                                               #
#                                                                                                                                                    #
# The above function works mostly with generic maximization/minimization, but does not guarantee its accuracy                                        #
# ================================================================================================================================================== #

# @example1
# max Z = 150x1 + 175x2
# subject to
# 7x1 + 11x2 <= 77
# 10x1 + 8x2 <= 80
# x1 <= 9
# x2 <= 6
# x1, x2 >= 0
# the last element is the right hand side of the objective function not Z, therefore we put 0 to it
m <- matrix(c(7, 11, 77, 10, 8, 80, 1, 0, 9, 0, 1, 6, 150, 175, 0), nrow = 5, byrow = TRUE)
# createTableau(m, TRUE, FALSE)

# @example2
# min Z = 14x1 + 20x2
# subject to
# x1 + 2x2 >= 4
# 7x1 + 6x2 >= 20
# x1, x2 >= 0
# the last element is the right hand side of the objective function not Z, therefore we put 0 to it
n <- matrix(c(1, 2, 4, 7, 6, 20, 14, 20, 0), nrow = 3, byrow = TRUE)
# createTableau(n, FALSE, FALSE)

# @ TEST CASES
# TEST CASE 1
# Min Z = 10x1 + 8x2 + 6x3 + 5x4 + 4x5 + 6y1 + 5y2 + 4y3 + 3y4 + 6y5 + 3z1 + 4z2 + 5z3 + 5z4 + 9z5
# subject to
# x1 + x2 + x3 + x4 + x5 <= 310
# y1 + y2 + y3 + y4 + y5 <= 260
# z1 + z2 + z3 + z4 + z5 <= 280
# x1 + y1 + z1 >= 180
# x2 + y2 + z2 >= 80
# x3 + y3 + z3 >= 200
# x4 + y4 + z4 >= 160
# x5 + y5 + z5 >= 220
# the last element is the right hand side of the objective function not Z, therefore we put 0 to it
o <- matrix(c(1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 310, 
              0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 260, 
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 280, 
              1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 180, 
              0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 80, 
              0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 200, 
              0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 160, 
              0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 220, 
             10, 8, 6, 5, 4, 6, 5, 4, 3, 6, 3, 4, 5, 5, 9, 0), nrow = 9, byrow = TRUE)
# createTableau(o, FALSE, TRUE)

# TEST CASE 2
# Min Z = 5x1 + 6x2 + 7x3 + 8x4 + 9x5 + 6y1 + 7y2 + 8y3 + 9y4 + 10y5 + 3z1 + 5z2 + 7z3 + 11z4 + 13z5
# subject to
# x1 + x2 + x3 + x4 + x5 <= 200
# y1 + y2 + y3 + y4 + y5 <= 200
# z1 + z2 + z3 + z4 + z5 <= 200
# x1 + y1 + z1 >= 100
# x2 + y2 + z2 >= 100
# x3 + y3 + z3 >= 100
# x4 + y4 + z4 >= 100
# x5 + y5 + z5 >= 100
# the last element is the right hand side of the objective function not Z, therefore we put 0 to it
p <- matrix(c(1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 200, 
              0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 200, 
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 200, 
              1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 100, 
              0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 100, 
              0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 100, 
              0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 100, 
              0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 100, 
              5, 6, 7, 8, 9, 6, 7, 8, 9,10, 3, 5, 7,11,13, 0), nrow = 9, byrow = TRUE)
# createTableau(p, FALSE, TRUE)

# TEST CASE 3
# Min Z = 30x1 + 29x2 + 31x3 + 35x4 + 33x5 + 26y1 + 24y2 + 23y3 + 25y4 + 27y5 + 11z1 + 13z2 + 15z3 + 20z4 + 17z5
# subject to
# x1 + x2 + x3 + x4 + x5 <= 1400
# y1 + y2 + y3 + y4 + y5 <= 400
# z1 + z2 + z3 + z4 + z5 <= 200
# x1 + y1 + z1 >= 431
# x2 + y2 + z2 >= 332
# x3 + y3 + z3 >= 350
# x4 + y4 + z4 >= 450
# x5 + y5 + z5 >= 400
# the last element is the right hand side of the objective function not Z, therefore we put 0 to it
q <- matrix(c(1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1400, 
              0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 400, 
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 200, 
              1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 431, 
              0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 332, 
              0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 350, 
              0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 450, 
              0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 400, 
             30,29,31,35,33,26,24,23,25,27,11,13,15,20,17, 0), nrow = 9, byrow = TRUE)
# createTableau(q, FALSE, TRUE)

# TEST CASE 4
# Min Z = 5x1 + 5x2 + 5x3 + 5x4 + 5x5 + 5y1 + 5y2 + 5y3 + 5y4 + 5y5 + 5z1 + 5z2 + 5z3 + 5z4 + 5z5
# subject to
# x1 + x2 + x3 + x4 + x5 <= 100
# y1 + y2 + y3 + y4 + y5 <= 100
# z1 + z2 + z3 + z4 + z5 <= 100
# x1 + y1 + z1 >= 20
# x2 + y2 + z2 >= 20
# x3 + y3 + z3 >= 20
# x4 + y4 + z4 >= 20
# x5 + y5 + z5 >= 20
# the last element is the right hand side of the objective function not Z, therefore we put 0 to it
r <- matrix(c(1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 100, 
              0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 100, 
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 100, 
              1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 20, 
              0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 20, 
              0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 20, 
              0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 20, 
              0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 20, 
              5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 0), nrow = 9, byrow = TRUE)
# createTableau(r, FALSE, TRUE)

# TEST CASE 5
# Min Z = 30x1 + 29x2 + 31x3 + 35x4 + 33x5 + 26y1 + 24y2 + 23y3 + 25y4 + 27y5 + 11z1 + 13z2 + 15z3 + 20z4 + 17z5
# subject to
# x1 + x2 + x3 + x4 + x5 <= 50
# y1 + y2 + y3 + y4 + y5 <= 50
# z1 + z2 + z3 + z4 + z5 <= 50
# x1 + y1 + z1 >= 20
# x2 + y2 + z2 >= 25
# x3 + y3 + z3 >= 90
# x4 + y4 + z4 >= 60
# x5 + y5 + z5 >= 70
# the last element is the right hand side of the objective function not Z, therefore we put 0 to it
s <- matrix(c(1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 50, 
              0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 50, 
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 50, 
              1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 20, 
              0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 25, 
              0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 90, 
              0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 60, 
              0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 70, 
              30,29,31,35,33,26,24,23,25,27,11,13,15,20,17, 0), nrow = 9, byrow = TRUE)
# createTableau(s, FALSE, TRUE)
