# Title     : TODO
# Objective : TODO
# Created by: SCDX
# Created on: 2020/9/23


# 4点分布抽样
chouqv = function(n) {
  x = c()
  for (i in 1:n) {
    u = runif(1)
    if (u < 0.2) {
      x[i] = 1
    }else if (u < 0.35) {
      x[i] = 2
    }else if (u < 0.6) {
      x[i] = 3
    }else {
      x[i] = 4
    }
  }
  return(x)
}

num = chouqv(1000)
table(num) / 1000


# 随机排列的生成(10数)
a = c(1:10)
b = c()
for (i in 1:10) {
  x = floor(length(a) * runif(1)) + 1
  b[i] = a[x]
  a = a[-x]
}
b
## sample法
c = sample(1:10, 10, replace = F, prob = rep(1 / 10, 10))
c


