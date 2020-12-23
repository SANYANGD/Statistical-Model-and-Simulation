# Created by: 代春洋
# Created on: 2020/12/23

fx1x2 <- function(u1, u2, e1, e2, p, x2) {
  return(rnorm(1, mean = (u1 + p * (e1 / e2) * (x2 - u2)), (1 - p * p) * e1 * e1))
}

fx2x1 <- function(u1, u2, e1, e2, p, x1) {
  return(rnorm(1, mean = (u2 + p * (e2 / e1) * (x1 - u1)), (1 - p * p) * e2 * e2))
}

gibbs <- function(u1, u2, e1, e2, p) {
  num <- 5000
  x1List <- vector(length = num)
  x2List <- vector(length = num)
  x2 <- u2
  for (i in 1:num) {
    x1 <- fx1x2(u1, u2, e1, e2, p, x2)
    x2 <- fx2x1(u1, u2, e1, e2, p, x1)
    x1List[i] <- x1
    x2List[i] <- x2
  }
  return(list(one = x1List, two = x2List))
}

x_res <- gibbs(0, 2, 1, 0.5, 0.75)$one
y_res <- gibbs(0, 2, 1, 0.5, 0.75)$two

