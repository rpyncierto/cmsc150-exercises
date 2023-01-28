# import statement
source("YnciertoEx04.r")

# Word Problem #2

# x1 = (50 + 30 + x2 + x4)/4
    # 4x1 - x2 - x4 - 80

# x2 = (30 + x1 + x5 + x3)/4
    # 4x2 - x1 - x3 - x5 - 30

# x3 = (50 + 30 + x2 + x6)/4
    # 4x3 - x2 - x6 - 80

# x4 = (50 + x7 + x5 + x1)/4
    # 4x4 - x1 - x5 - x7 - 50

# x5 = (x2 + x4 + x8 + x6)/4
    # 4x5 -x2 - x4 - x6 - x8

# x6 = (50 + x3 + x5 + x9)/4
    # 4x6 -x3 - x5 - x9 - 50

# x7 = (50 + 70 + x8 + x4)/4
    # 4x7 - x4 - x8 - 120

# x8 = (70 + x9 + x5 + x7)/4
    # 4x8 - x5 - x7 -x9 - 70

# x9 = (70 + 50 + x6 + x8)/4
    # 4x9 - x6 - x8 - 120


E1 <- function (x1, x2, x3, x4, x5, x6, x7, x8, x9) 4 * x1 + -1 * x2 + 0 * x3 + -1 * x4 + 0 * x5 + 0 * x6 + 0 * x7 + 0 * x8 + 0 * x9 + -80;

E2 <- function (x1, x2, x3, x4, x5, x6, x7, x8, x9) 4 * x2 + -1 * x1 + -1 * x3 + 0 * x4 + -1 * x5 + 0 * x6 + 0 * x7 + 0 * x8 + 0 * x9 + -30;

E3 <- function (x1, x2, x3, x4, x5, x6, x7, x8, x9) 4 * x3 + 0 * x1 + -1 * x2 + 0 * x4 + 0 * x5 + -1 * x6 + 0 * x7 + 0 * x8 + 0 * x9 + -80;

E4 <- function (x1, x2, x3, x4, x5, x6, x7, x8, x9) 4 * x4 + -1 * x1 + 0 * x2 + 0 * x3 + -1 * x5 + 0 * x6 + -1 * x7 + 0 * x8 + 0 * x9 + -50;

E5 <- function (x1, x2, x3, x4, x5, x6, x7, x8, x9) 4 * x5 + 0 * x1 + -1 * x2 + 0 * x3 + -1 * x4 + -1 * x6 + 0 * x7 + -1 * x8 + 0 * x9 + 0;

E6 <- function (x1, x2, x3, x4, x5, x6, x7, x8, x9) 4 * x6 + 0 * x1 + 0 * x2 + -1 * x3 + 0 * x4 + -1 * x5 + 0 * x7 + 0 * x8 + -1 * x9 + -50;

E7 <- function (x1, x2, x3, x4, x5, x6, x7, x8, x9) 4 * x7 + 0 * x1 + 0 * x2 + 0 * x3 + -1 * x4 + 0 * x5 + 0 * x6 + -1 * x8 + 0 * x9 + -120;

E8 <- function (x1, x2, x3, x4, x5, x6, x7, x8, x9) 4 * x8 + 0 * x1 + 0 * x2 + 0 * x3 + 0 * x4 + -1 * x5 + 0 * x6 + -1 * x7 + -1 * x9 + -70;

E9 <- function (x1, x2, x3, x4, x5, x6, x7, x8, x9) 4 * x9 + 0 * x1 + 0 * x2 + 0 * x3 + 0 * x4 + 0 * x5 + -1 * x6 + 0 * x7 + -1 * x8 + -120;

system3 <- list(E1, E2, E3, E4, E5, E6, E7, E8, E9);


GaussianElimination(system3)
GaussJordanElimination(system3)