# Title     : 控制变量法
# Objective :
# Created by: 代春洋
# Created on: 2020/11/18

u <- runif(100000)

m1 <- exp(-u) / (1 + u^2)
m2 <- m1 - cov(exp(-0.5) / (1 + u^2),
              exp(-u) / (1 + u^2)) / var(exp(-0.5) / (1 + u^2)) * (exp(-0.5) / (1 + u^2) - mean(exp(-0.5) / (1 + u^2)))

c(mean(m1), mean(m2))
c(var(m1), var(m2))
(var(m1) - var(m2)) / var(m1)
