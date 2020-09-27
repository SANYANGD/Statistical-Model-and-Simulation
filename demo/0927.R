# Title     : TODO
# Objective : TODO
# Created by: SCDX
# Created on: 2020/9/27

# 泊松分布抽样
possion1 = function (lamda){
  u = runif(1)
  i = 0
  p = exp(-lamda)
  F = p
  while(F <= u){
    p = lamda*p/(i + 1)
    F = F + p
    i = i + 1
  }
  return(i)
}
ps = c()
for(i in 1:10000){
  ps[i] = possion1(10)
}

#lamda = 4.3
#x = 1:10
#y = dpois(x, lamda)
#plot(x, y, type = "p")

plot(table(ps)/10000)
par(new = TRUE)
plot(x, y)
