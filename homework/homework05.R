# Title     : TODO
# Objective : TODO
# Created by: SCDX
# Created on: 2020/9/30


## f(x) = (e*x) / (e-1) , 0 =< x =<1
#hist(log((exp(1)-1)*runif(100000)+1), freq = F, xlim = c(0, 1), ylim=c(0,1.6), breaks = 50)
#par(new = T)
#curve((exp(x) - 1) / (exp(1) - 1), xlim = c(0, 1), ylim=c(0,1.6), col = "red")
#par(new = T)
#curve(exp(x) / (exp(1) - 1), ylim=c(0,1.6), col = "green")


#
u1 = runif(100000)
u2 = runif(100000)
y = (-2) * log(u1) + 5
c = 5 / 3
accept = y[u2 <= (y * exp(-y) / (exp(-5) * 6)) / (c * (1 / 2) * exp(-(y - 5) / 2))]
hist(accept, breaks = seq(0, 100, 0.01), xlim = c(5, 15), freq = F)
par(new = T)
curve(x * exp(-x) / (exp(-5) * 6), xlim = c(5, 15), col = "green")
par(new = T)
curve((1 / 2) * exp(-(x - 5) / 2), xlim = c(5, 15), col = "red")


