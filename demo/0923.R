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


# 几何随机变量抽样
toushaizi = function() {
  shaizi = sample(1:6, 100, replace = T, prob = rep(1 / 6, 6))
  a = c(1:6)
  for (i in 1:100) {
    if (shaizi[i] == 1) {
      a = a[-1]
    }else if (shaizi[i] == 2) {
      a = a[-2]
    }else if (shaizi[i] == 3) {
      a = a[-3]
    }else if (shaizi[i] == 4) {
      a = a[-4]
    }else if (shaizi[i] == 5) {
      a = a[-5]
    }else if (shaizi[i] == 6) {
      a = a[-6]
    }
    if (length(a) == 0) {
      return(i)
    }
  }
}
tsz = c()
for (i in 1:10000){
  tsz[i] = toushaizi()
}
hist(tsz)
tsz_mean = mean(tsz)
tsz_mean
tsz_var = var(tsz)
tsz_var

