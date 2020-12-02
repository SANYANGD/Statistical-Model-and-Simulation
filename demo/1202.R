# Title     : TODO
# Objective : TODO
# Created by: SCDX
# Created on: 2020/12/2

data = c(20,7,23,17,12,21)
mean = 100/6
df = 6-1
T = sum((data-mean)^2)/mean
p = 1-pchisq(T,df)
