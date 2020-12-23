
# Created by: 代春洋
# Created on: 2020/12/16

rayleigh <- function(x, s) {
  if (x < 0)
    return(0)
  stopifnot(s > 0)
  return((x / s^2) * exp(-x^2 / (2 * s^2)))
}

n <- 10000; s <- 4; result <- 0
x <- numeric(n)
x[1] <- rchisq(1, df <- 1)
u <- runif(n)

for (i in 2:n) {
  y <- rchisq(1, df <- x[i - 1])
  num <- rayleigh(y, s) * dchisq(x[i - 1], df = y)
  d <- rayleigh(x[i - 1], s) * dchisq(y, df = x[i - 1])
  if (u[i] <= num / d)
    x[i] <- y
  else {
    x[i] <- x[i - 1]
    result <- result + 1
  }
}
result  # 4131