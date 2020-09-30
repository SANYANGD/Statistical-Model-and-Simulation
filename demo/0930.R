# Title     : TODO
# Objective : TODO
# Created by: SCDX
# Created on: 2020/9/30

# 复合法
select = function() {
  u1 = runif(1)
  u2 = runif(1)
  x = 0
  if (u1 < 0.5) {
    x = floor(10 * u2) + 1
  }else {
    x = floor(5 * u2) + 6
  }
  return(x)
}

array = c()
for (i in 1:10000) {
  array[i] = select()
}
table(array) / 10000


# 指数分布抽样
# lamda=0.5
curve(0.5 * exp(-0.5 * x), 0, 10, ylim = c(0, 1), col = 'green')
par(new = T)
curve(1 - exp(-0.5 * x), 0, 10, ylim = c(0, 1))
par(new = T)
hist(-2 * log(runif(100000)), freq = F, xlim = c(0, 10), ylim = c(0, 1), breaks = 100)