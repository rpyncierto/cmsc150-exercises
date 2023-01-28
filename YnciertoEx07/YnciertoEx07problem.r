# import
source("YnciertoEx07.r")

# Problem Solving
x = c(1995, 2000, 2005, 2010, 2015)
y = c(68349452, 75505061, 82079348, 87940171, 93440274)
data = list(x, y)

# import the poly.lagrange function
result = poly.lagrange(data)

# polyfunc result
print(result$polyfunc)
# approximation of the function at x = 2004, or the population in 2004
print(result$polyfunc(2004))