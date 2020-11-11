# Title     : TODO
# Objective : TODO
# Created by: 代春洋2018141493004
# Created on: 2020/11/11

g = function(x) { return(2 * x) }

Arrival = function(lamda, g, s) {
  #g(t)表示强度函数（随时间变化）
  #lamda表示g(t)的最大值
  #该子程序用来产生时刻s后第一个顾客的到达时间
  t = s;
  while (1) {
    t = t - log(runif(1)) / lamda
    if (runif(1) <= g(t) / lamda) { break }
  }
  return(t)
}

MG1 = function(Arrival, T, mu) {
  #假设服务时间Y服从参数为mu的指数分布
  t = 0; Na = 0; Nd = 0; n = 0;
  A = c(); D = c(); N = c();
  tA = Arrival(lamda, g, t); tD = Inf
  while (1) {
    if (tA <= tD & tA <= T) {
      t = tA; Na = Na + 1; n = n + 1;
      tA = Arrival(lamda, g, t);
      if (n == 1) {
        Y = -log(runif(1)) / mu;
        tD = t + Y;
      }
      A[Na] = t
    }else if (tD <= tA & tD <= T) {
      t = tD; n = n - 1; Nd = Nd + 1;
      if (n == 0) { tD = Inf }
      else { Y = -log(runif(1)) / mu; tD = t + Y }
      D[Nd] = t; N[Nd] = n
    }else if (tA > T & tD > T) { break }
  }
  while (1) {
    if (n > 0) {
      t = tD; n = n - 1; Nd = Nd + 1;
      if (n > 0) { Y = -log(runif(1)) / mu; tD = t + Y }
      D[Nd] = t; N[Nd] = n
    }else if (n <= 0) { break }
  }
  #Tp = max(t - T, 0)
  Tp = t - T
  list(w = mean(D - A), gohome = Tp, L = mean(N))
  return(Tp)
}


lamda = 1.2; k = 100; T = 12; mu = 2; a = 0; b = 0
X = c()

exam1 = function(d) {
  for (i in 1:100) {
    X[i] = MG1(Arrival, T, mu)
  }
  k = 100
  ss = 0
  xbar = X[1]
  for (j in 1:(k - 1)) {
    xbar[j + 1] = xbar[j] + (X[j + 1] - xbar[j]) / (j + 1)
    ss[j + 1] = (1 - 1 / j) * ss[j] + (j + 1) * (xbar[j + 1] - xbar[j])^2
  }
  while (1) {
    k = k + 1
    X[k] = MG1(Arrival, T, mu)
    xbar[k] = xbar[k - 1] + (X[k] - xbar[k - 1]) / k
    ss[k] = (1 - 1 / (k - 1)) * ss[k - 1] + k * (xbar[k] - xbar[k - 1])^2
    if (sqrt(ss[k]) / sqrt(k) < d) {
      break
    }
  }
  plot(c(1:k),X)
  list(xbar = mean(X), N = k, Var = ss[k])
}
system.time(exam1(0.01))
a = exam1(0.01)
a$xbar+12#最后一个顾客离开系统的平均时间  12.36237
a$N#抽样次数  21007

exam2 = function(d) {
  for (i in 1:100) {
    X[i] = MG1(Arrival, T, mu)
    if (X[i] >= 0.5) { X[i] = 1 }
    else { X[i] = 0 }
  }
  k = 100
  ss = 0
  xbar = X[1]
  for (j in 1:(k - 1)) {
    xbar[j + 1] = xbar[j] + (X[j + 1] - xbar[j]) / (j + 1)
    ss[j + 1] = (1 - 1 / j) * ss[j] + (j + 1) * (xbar[j + 1] - xbar[j])^2
  }
  while (1) {
    k = k + 1
    X[k] = MG1(Arrival, T, mu)
    if (X[k] >= 0.5) { X[k] = 1 }
    else { X[k] = 0 }
    xbar[k] = xbar[k - 1] + (X[k] - xbar[k - 1]) / k
    ss[k] = (1 - 1 / (k - 1)) * ss[k - 1] + k * (xbar[k] - xbar[k - 1])^2
    if (sqrt(ss[k]) / sqrt(k) < d) {
      break
    }
  }
  list(xbar = mean(X), N = k, Var = ss[k])
}
system.time(exam2(0.01))
b = exam2(0.01)
b$xbar#到8：30仍有顾客在系统中的概率  0.3954849
b$N#抽样次数  2392