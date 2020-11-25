# Title     : 分层抽样 重要抽样
# Objective : TODO
# Created by: 代春洋
# Created on: 11/25/2020

# 分层抽样
U1 <- runif(2500,min = 0,max = 0.25)
U2 <- runif(2500,min=0.25,max=0.5)
U3 <- runif(2500,min=0.5,max=0.75)
U4 <- runif(2500,min = 0.75,max=1)
T1 <- exp(-U1)/(1+(U1)^2)
T2 <- exp(-U2)/(1+(U2)^2)
T3 <- exp(-U3)/(1+(U3)^2)
T4 <- exp(-U4)/(1+(U4)^2)
mean <- 1/4*(mean(T1)+mean(T2)+mean(T3)+mean(T4))
var <- (1/4*(var(T1)+var(T2)+var(T3)+var(T4)))/4
U <- runif(10000)
T <- exp(-U)/(1+(U)^2)
mean_crude <- mean(T)
var_crude <- var(T)
mean # 分层抽样均值
var # 分层抽样方差
mean_crude # 正常抽样均值
var_crude # 正常抽样方差
100*(var_crude-var)/var_crude # 优化百分比


# 重要抽样
r <- c()
for(i in 1:10000) {
  u <- rbinom(prob=0.8,n=1,size=20)
  if(U1>=16) {
    r <- c(r,(1/6)^(u)*3^20)
  }
  else {
    r <- c(r,0)
  }
}
mean(r) # 无偏估计量
