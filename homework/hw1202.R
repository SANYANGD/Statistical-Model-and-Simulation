# Title     : TODO
# Objective : TODO
# Created by: 代春洋
# Created on: 2020/12/2

#P值计算方法: 首先求出D值d之后，计算D>d的概率，因此需要求出D的分布函数，就可以按照F求出多个10值之后得到D1，D2，D3......，计算其中大于d的比例

data <- sort(c(71, 198, 139, 145, 21, 31, 47, 122, 146, 109))

d <- c()

D <- function(data, n) {
  t <- c()
  for (j in 1:n) {
    t <- c(t, j / n - data[j])
    t <- c(t, (data[j] - (j - 1) / n))
  }
  return(max(t))
}

for (i in 1:10000) {
  r <- runif(10)
  d <- c(d, D(sort(r), 10))
}

P <- length(d[d > 0.2637835]) / 10000
P
