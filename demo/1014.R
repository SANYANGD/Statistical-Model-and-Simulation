# Title     : TODO
# Objective : TODO
# Created by: SCDX
# Created on: 2020/10/14

## 1/B(a,b) x^(a-1) (1-x)^(b-1)
#beta <- function (a,b){
#  curve((factorial(a+b-1)/factorial(a-1)*factorial(b-1))*x^exp(a-1)*(1-x)^exp(b-1), from = 0, to = 1)
#}
#beta(5,2)

u1=runif(100000)
u2=runif(100000)
accept=u1[u2<=(256*u1*(1-u1)^3)/27]
plot(u1,20*u1*(1-u1)^3)
par(new=T)
hist(accept,breaks = seq(0,1,0.002),freq = F)



