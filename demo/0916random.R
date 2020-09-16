# Title     : TODO
# Objective : TODO
# Created by: chunyang
# Created on: 9/16/2020

# 线性同余法随机数
randomA = function (m,a,b,c,raw1,raw2){
    point = c(raw1, raw2)
    for (i in 3:50000){
        point[i] = (point[i-2]*a+point[i-1]+b) %% m
    }
    return (point/m)
}
point = randomA(1000000, 2, 4, 4, 1, 64 )
# print(point)
hist(point) # 画图
