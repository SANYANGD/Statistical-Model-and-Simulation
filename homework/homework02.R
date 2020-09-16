# Title     : TODO
# Objective : TODO
# Created by: chunyang
# Created on: 9/16/2020

# 求微积分
u = runif(1000000)
value1 = mean((1-u^2)^(3/2))
value1
value2 = mean(4 * exp( (4*u-2) + ( (4*u-2)^2 )) )
value2
value3 = mean( (1/u-1) * (1+(1/u-1)^2)^-2 * 1/u^2 )
value3
value4 = mean(exp( -(1/u-1)^2 ) * 1/u^2)
value4