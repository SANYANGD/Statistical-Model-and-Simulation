# Title     : TODO
# Objective : TODO
# Created by:
# Created on: 2020/9/23

chouqv = function(n) {
  s = 0
  chou = sample(1:n, n, replace = F, prob = rep(1 / n, n))
  for (i in 1:n) {
    if (i == chou[i]) {
      s = s + 1
    }
  }
  return (s)
}

a = c()
for (i in 1:10000){
  a[i] = chouqv(100)
}
mean_a = mean(a)
mean_a #期望
var_a = var(a)
var_a #方差

