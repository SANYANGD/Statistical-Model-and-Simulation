# Created by: 代春洋
# Created on: 2020/12/23

p <- 0.5

f_1_2 <- function(u1, u2, e1, e2, x2) {
  return(rnorm(1, mean = (u1 + p * (e1 / e2) * (x2 - u2)), (1 - p * p) * e1 * e1))
}

f_2_1 <- function(u1, u2, e1, e2, x1) {
  return(rnorm(1, mean = (u2 + p * (e2 / e1) * (x1 - u1)), (1 - p * p) * e2 * e2))
}

G <- function(u1, u2, e1, e2) {
  n <- 5000
  list1 <- vector(length = n)
  list2 <- vector(length = n)
  x2 <- u2
  for (i in 1:n) {
    x1 <- f_1_2(u1, u2, e1, e2, x2)
    x2 <- f_2_1(u1, u2, e1, e2, x1)
    list1[i] <- x1
    list2[i] <- x2
  }
  return(list(a = list1, b = list2))
}

x_r <- G(0, 2, 1, 0.5)$a
x_r
y_r <- G(0, 2, 1, 0.5)$b
y_r

