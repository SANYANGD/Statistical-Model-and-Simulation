# Title     : TODO
# Objective : TODO
# Created by: 代春洋
# Created on: 2020/12/2


#需要先对数据进行排序

data <- sort(c(71, 198, 139, 145, 21, 31, 47, 122, 146, 109))

F <- function(x) {
  return(1 - exp(-x / 100))
}

D <- function(data, n) {
  t <- c()
  for (j in 1:n) {
    t <- c(t, j / n - (F(data[j])))
    t <- c(t, (F(data[j])) - (j - 1) / n)
  }
  return(max(t))

}

D(data, 10)