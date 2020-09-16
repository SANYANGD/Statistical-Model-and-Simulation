# Title     : TODO
# Objective : TODO
# Created by: chunyang
# Created on: 9/16/2020

# 随机投点法计算y=x^2在（0，1）的积分
area = function (n){
    x = c()
    y = c()
    num = 0
    for(i in 1:n){
        xtemp = runif(1)
        ytemp = runif(1)
        if (ytemp < xtemp * xtemp){
            x[i] = xtemp
            y[i] = ytemp
          num = num +1
        }
    }
    plot(x,y)
    return (num)
}
num = area(10000)
print(num/10000)