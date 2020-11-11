# Title     : TODO
# Objective : TODO
# Created by: 单服务员模型
# Created on: 2020/11/11


# 某理发店只有1名理发师为顾客服务，按照先到先服务的原则对顾客进行服务，
# 假设该理发店每天的工作时间为上午8点到下午8点，在此段时间内的顾客都必须提供服务，而下午8点之后不再接受新顾客。
# 假设顾客以参数为λ=1.2人/小时的泊松过程进入该店，且每位顾客的理发时间服从指数分布，平均需要30分钟。
# 试估计顾客在理发店的平均时间、该理发师的平均加班时间，以及平均队长。


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

g = function(x) { return(2 * x) }

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
  Tp = max(t - T, 0)
  list(w = mean(D - A), gohome = Tp, L = mean(N))
}

lamda = 1.2; T = 12; mu = 2
result = data.frame(w = 0, gohome = 0, L = 0)
for (i in 1:1000) {
  tmp = MG1(Arrival, T, mu)
  add = c(tmp$w, tmp$gohome, tmp$L)
  result[i,] = add
}
