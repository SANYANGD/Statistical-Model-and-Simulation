# Title     : TODO
# Objective : TODO
# Created by: SCDX
# Created on: 2020/10/21

#泊松分布抽样

cost = 0
all = c()
for (i in 1:10000) {
  sum = 0
  for (j in 1:12) {
    n = rpois(1, 5)
    money = runif(n, min = 2000, max = 10000)
    sum = sum + sum(money)
  }
  all = c(all, sum)
}
cost = sort(all)[9901] / 10000
cost



