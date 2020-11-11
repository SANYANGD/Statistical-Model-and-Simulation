# Title     : TODO
# Objective : TODO
# Created by: SCDX
# Created on: 2020/11/11

exam1 = function(d){
  X = rnorm(100)
  k = 100
  while(sd(X)/sqrt(k)>=d){
    k = k+1
    X[k]=rnorm(1)
  }
  return(k-1)
}

exam2=function(d){
  X=rnorm(100)
  k=100
  ss=0
  xbar=X[1]
  for(j in 1:(k-1)){
    xbar[j+1]=xbar[j]+(X[j+1]-xbar[j])/(j+1)
    ss[j+1]=(1-1/j)*ss[j]+(j+1)*(xbar[j+1]-xbar[j])^2
  }
  while(1){
    k=k+1
    X[k]=rnorm(1)
    xbar[k]=xbar[k-1]+(X[k]-xbar[k-1])/k
    ss[k]=(1-1/(k-1))*ss[k-1]+k*(xbar[k]-xbar[k-1])^2
    if(sqrt(ss[k])/sqrt(k)<d){
      break
    }
  }
  list(xbar=mean(X),N=k,Var=ss[k])
}


system.time(exam1(0.01))
system.time(exam2(0.01))
