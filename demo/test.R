# Title     : TODO
# Objective : TODO
# Created by: chunyang
# Created on: 9/9/2020

f = c(0,1)
for(i in 3:20){
  f[i] = f[i-2] + f[i-1]
}
print(f)