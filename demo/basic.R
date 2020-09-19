# Title     : TODO
# Objective : TODO
# Created by: chunyang
# Created on: 9/9/2020

f = c(0,1)
for(i in 3:20){
  f[i] = f[i-2] + f[i-1]
}
print(f)

a = c(12,213,4,436,764,231,3257,34,4573,2232,2)
print( a[order(a)] )
print( sum(a) )
print( sort(a[which(a<=100)]) )

vector=c(1, 2, 3, 4, 5, 6)
matrix(vector, 2, 3)

# 逆矩阵
A = matrix(c(1, 3, 2, 4), 2, 2)
solve(A)

# 函数
new.function = function(a,b) {
    for(i in 1:a) {
       b <- i^2
       print(b)
    }
 }
# 调用函数，并传递参数
new.function(6,3)