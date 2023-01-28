# implementation of polynomial interpolation using lagrange interpolation
poly.lagrange <- function(data) {
  # get the number of data points
  n <- length(data[[1]])
  # create a vector of zeros to store the polynomial bases
  raw_polybases <- rep(0, n)
  # create a list to store the polybases
  polybases <- list()
  # create a string to store the function
  function_string <- "function (x) "
  # iterate n times, where n is the number of data points
  for (i in 1:n) {
    # create a string to store the numerator of the polybases
    numerator <- ""
    # create a string to store the denominator of the polybases
    denominator <- ""
    # create a string that will add open parenthesis to the equation to ensure the correct order of operations
    funct <- "("
    # iterate n times
    for (j in 1:n) {
      # check if i == j, if yes, proceed to the next iteration since i and j must not be equal
      if (i == j) {
        next
      } else {
        # creates the string polybases
        if ((i == n && j == n-1) || (j >= n)){
          # store the numerator of the polybases
          numerator <- paste(numerator, "(x - ", data[[1]][j], ")", sep = "")
          # store the denominator of the polybases
          denominator <- paste(denominator, "(", data[[1]][i], " - ", data[[1]][j], ")", sep = "")
        } else {
          # store the numerator of the polybases
          numerator <- paste(numerator, "(x - ", data[[1]][j], ")", " * ", sep = "")
          # store the denominator of the polybases
          denominator <- paste(denominator, "(", data[[1]][i], " - ", data[[1]][j], ")", " * ", sep = "")
        }
      }
    }
    # join the numerator and denominator of the polybases
    raw_polybases[i] <- paste(funct, numerator, ")", "/", funct, denominator, ")", sep = "")
    # convert the polybases string into a function
    polybases <- append(polybases, eval(parse(text= paste(function_string, raw_polybases[i], sep = ""))))
  }
  # a string to store interpolating polynomial
  polystring <- "function (x) "
  # iterate length of polybases times
  for (i in 1:length(polybases)) {
    # combine the polybases into a single function
    if (i < length(polybases)) {
      polystring <- paste(polystring, data[[2]][i], " * ", raw_polybases[i], " + ", sep = "")
    } else {
      polystring <- paste(polystring, data[[2]][i], " * ", raw_polybases[i], sep = "")
    }
  }
  # convert the polystring into a function
  polyfunc <- eval(parse(text=polystring))
  # store the lists in a list to return all of it
  info = list(
    polybases = polybases,
    polystring = polystring,
    polyfunc = polyfunc
  )
  # return the list
  return(info)
}

# TEST CASE
#  x = c(1, 3, 6, 9)
#  y = log(x)
#  data = list(x, y)
#  result = poly.lagrange(data)

#  print(result)

# access the polybases function in the list
#  for (i in 1:length(result$polybases)) {
#    print(result$polybases[[i]](4.5))
#  }
# access the polyfunc function in the list
# result$polyfunc(4.5)