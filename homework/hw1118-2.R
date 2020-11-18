# Title     : TODO
# Objective : TODO
# Created by: SCDX
# Created on: 2020/11/18

U = runif(100000)
T1 = exp(-U) / (1 + U^2)
T2 = T1 - cov(exp(-0.5) / (1 + U^2), exp(-U) / (1 + U^2)) / var(exp(-0.5) / (1 + U^2)) * (exp(-0.5) / (1 + U^2) - mean(exp(-0.5) / (1 + U^2)))
c(mean(T1), mean(T2))
c(var(T1), var(T2))
(var(T1) - var(T2)) / var(T1)
