# Title     : TODO
# Objective : TODO
# Created by: SCDX
# Created on: 2020/10/14

# test04
# f(x)= 1
# c=1
u1=runif(100000)
u2=runif(100000)
accept=u1[u2<=u1^0]
plot(u1,u1^0)
par(new=T)
hist(accept,breaks = seq(0,1,0.002),freq = F)
c = length(accept)/(2*length(u1))
c

# test05
# f(x)=30*(x^2-2*x^3+x^4) 0=<x=<1
# c=15/8
u1=runif(100000)
u2=runif(100000)
accept=u1[u2<=16*(u1^2-2*u1^3+u1^4)]
plot(u1,30*(u1^2-2*u1^3+u1^4))
par(new=T)
hist(accept,breaks = seq(0,1,0.002),freq = F)
c = length(accept)/(2*length(u1))
c