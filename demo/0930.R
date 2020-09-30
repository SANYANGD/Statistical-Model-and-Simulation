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


#
u1 = runif(100000)
u2 = runif(100000)
k = 2 * sqrt(pi)
y = (-3 / 2) * log(u1)
c = (3^1.5) / (2 * pi * exp(1))^0.5
accept = y[u2 <= k * ((y^0.5) * exp(-y) / c * (2 / 3) * exp(-2 * y / 3))]
hist(accept, freq = F, xlim = c(0, 10), breaks = seq(0, 100, 0.02))
par(new = T)
plot(10 * u1, ((10 * u1)^0.5) * exp(-u1 * 10) * 2 / sqrt(pi))
