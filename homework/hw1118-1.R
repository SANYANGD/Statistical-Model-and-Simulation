# Title     : 对偶变量法
# Objective : TODO
# Created by: 代春洋
# Created on: 2020/11/18

func1 <- function(n) {
  a <- runif(10000)
  m <- n / sqrt(2 * pi) * exp(-(n * a)^2 / 2)
  # x <- ave(m) * 1 + 0.5
  return(var(m))
}
x <- seq(0.1, 2.5, length = 10)
r <- c()
for (i in 1:10) {
  r <- c(r, func1(x[i]))
}
a <- ave(r)[1]
a

func2 <- function(n) {
  u1 <- runif(5000)
  u2 <- 1 - u1

  m <- n / sqrt(2 * pi) * exp(-(n * u1)^2 / 2)
  m2 <- n / sqrt(2 * pi) * exp(-(n * u2)^2 / 2)
  m <- (m + m2) / 2
  # mm <- ave(m) * 1 + 0.5
  return(var(m))
}
x <- seq(0.1, 2.5, length = 10)
r2 <- c()
for (i in 1:10) {
  r2 <- c(r2, func2(x[i]))
}
a2 <- ave(r2)[1]
a2

r / r2

