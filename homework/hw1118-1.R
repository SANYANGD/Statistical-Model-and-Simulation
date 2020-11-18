# Title     : TODO
# Objective : TODO
# Created by: 代春洋
# Created on: 2020/11/18

#直接抽
g = function(n) {
  u1 = runif(10000)
  s = n / sqrt(2 * pi) * exp(-(n * u1)^2 / 2)
  ss = ave(s) * 1 + 0.5
  return(var(s))
}

x = seq(0.1, 2.5, length = 10)
res = c()
for (i in 1:10) {
  res = c(res, g(x[i]))
}
ave1 = ave(res)[1]
ave1

#互补抽
g2 = function(n) {
  u1 = runif(5000)
  u2 = 1 - u1

  s = n / sqrt(2 * pi) * exp(-(n * u1)^2 / 2)
  s2 = n / sqrt(2 * pi) * exp(-(n * u2)^2 / 2)
  s = (s + s2) / 2
  ss = ave(s) * 1 + 0.5
  return(var(s))
}

x = seq(0.1, 2.5, length = 10)
res2 = c()
for (i in 1:10) {
  res2 = c(res2, g2(x[i]))
}
ave2 = ave(res2)
ave2
res / res2
ave1 / ave2