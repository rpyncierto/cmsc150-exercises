# Reymond P. Yncierto
# CMSC 150 X2L
# Exercise B: This program joins two unsorted numerical vectors and sorts the resulting vector.

# This function sorts a vector (bubble sort)
# requirement: vector
# result: returns the sorted vector
sort <- function(vector) {
  for (i in 1:(length(vector) -1)) { # loop through the vector
    for (j in 1:(length(vector) -1)) { # loop through the vector
      if (vector[j] > vector[j+1]) { # if the current element is greater than the next element
        # swap the position of both elements
        temp <- vector[j]
        vector[j] <- vector[j+1]
        vector[j+1] <- temp
      } else { 
        next # skip to the next iteration
      }
    }
  }
  return (vector) # returns the sorted vector
}

# This function merges two vectors
# requirement: two vectors
# result: returns the merged vector
merge <- function(vector1, vector2) {
  newVector <- c(vector1, vector2) # concatenates the two vectors 
  return (newVector) # returns the resulting vector
}

sort(merge(c(3,5,10,6), c(4,1,15,2,7))) # function calls with two vectors as the merge function's actual parameters, and the merge function's return value as the sort function's parameter