# Title     : TODO
# Objective : TODO
# Created by: SCDX
# Created on: 2020/10/21

#泊松分布抽样
countcost = function() {
  cost = c()
  for (i in c(1, 2000)) {
    sum = 0
    for (t in c(1, 12)) {
      n = rpois(1, 5)
      money = runif(n, min = 2000, max = 10000)
      sum = sum + sum(money)
    }
    mean = sum
    all = c(cost, mean)
  }
  cost = mean(all) * 0.99
  return(cost)
}

huifei = c()
all = c()
#遍历2000名用户
for (i in 1:2000)
{
  #一个用户一年需要的金钱
  sum = 0
  for (t in 1:12)
  {
    cishu = rpois(1, 5)
    money = runif(cishu, min = 2000, max = 10000)
    sum = sum + sum(money)
  }
  all = c(all, sum)
}
huifei = mean(all) * 0.99