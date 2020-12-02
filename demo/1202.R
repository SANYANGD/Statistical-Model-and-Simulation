# Title     : TODO
# Objective : TODO
# Created by: SCDX
# Created on: 2020/12/2

data=c(20,7,23,17,12,21)
mean=100/6
df=6-1
T=sum((data-mean)^2)/mean
p=1-pchisq(T,df)


# m: m个人做这个实验
# T：根据题目当中的分布算出来的K方
exam=function (m,T)
  {
  # T0 假设骰子是均匀情况下的分布
  T0=0;mean=100/6
  for(i in 1:m){
    Y=sample(1:6,100,replace = T)
    N=c(length(Y[Y=1]),length(Y[Y=2]),length(Y[Y=3]),length(Y[Y=4]),length(Y[Y=5]),length(Y[Y=6]))
    T0[i]=sum((N-mean)^2/mean)
  }
    length(T0[T0>=T])/m
}
exam(100000,T)