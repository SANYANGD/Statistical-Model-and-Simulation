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
curve((exp(-x) * x) / 6 * (exp(-5)), xlim = c(5, 10), col = "red")
par(new = T)
curve(2 * exp(-2 * x), xlim = c(5, 10), col = "green")
par(new = T)


