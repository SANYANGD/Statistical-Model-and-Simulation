# Title     : 仓储模型
# Objective : 仓储模型
# Created by: 代春洋
# Created on: 2020/11/4


#题目当中给出的信息变量
cusLamda <- 8
purchaseRto <- c(0.7, 0.7, 0.08, 0.02)
buyPrice <- 5
storePrice <- 0.5
transPrice <- 10
transDelay <- 1
salePrice <- 12
Ss <- c(10, 120)


Arrival <- function(lamda, g, s) {
  t <- s
  while (1) {
    t <- t - log(runif(1)) / lamda
    if (runif(1) <= g(t) / lamda) {
      break
    }
  }
  return(t)
}

g <- function(x) {
  return(2 * x)
}


numG <- function() {
  numG <- 0
  random <- runif(1)
  if (random < purchaseRto[1]) {
    numG <- 1
  }
  else if (random < purchaseRto[1] + purchaseRto[2]) {
    numG <- 2
  }
  else if (random < 1 - purchaseRto[4]) {
    numG <- 3
  }
  else if (random <= 1) {
    numG <- 4
  }
  return(numG)
}


countpro <- function() {
  #计数变量
  t <- 0   #时间变量
  C <- 0  #t时刻订购话费的总和
  H <- 0   # 商品库存花费总和
  R <- 0   #t时刻收入总量
  SS <- c(120, 0) #SS(x,y)x当前产品库存，y当前商品的订户订货量

  t0 <- Arrival(cusLamda, g, t) #下一个顾客到达的时间
  t1 <- Inf   #下一批订单到达的时间

  while (1) {
    if (t0 < t1) {
      D <- numG()
      H <- H + (t0 - t) * SS[1] * storePrice
      w <- min(D, SS[1])
      R <- R + w * salePrice
      SS[1] <- SS[1] - w
      t <- t0
      t0 <- Arrival(cusLamda, g, t)
      if (SS[1] < Ss[1] && SS[2] == 0) {
        SS[2] <- Ss[2] - SS[1]
        t1 <- t + transDelay
      }
    }
    else if (t0 >= t1) {
      H <- H + (t1 - t) * SS[1] * storePrice
      t <- t1
      C <- C + buyPrice * SS[2] + transPrice
      SS[1] <- SS[1] + SS[2]
      SS[2] <- 0
      t1 <- Inf
    }
    if (t > 30) {
      break
    }
  }
  return(R - H - C)
}

Sum <- 0
for (i in 1:1000) {
  Sum <- Sum + countpro()
}
result <- Sum / 1000
result

