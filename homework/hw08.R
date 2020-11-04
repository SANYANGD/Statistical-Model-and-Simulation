# Title     : TODO
# Objective : TODO
# Created by: SCDX
# Created on: 2020/11/4

#ts <- function(lambda, g, s) {
#  t <- s
#  u <- runif(1)
#  t <- t - log(u) / lambda
#  u1 <- runif(1)
#  while (u1 > g(t) / lambda) {
#    u <- runif(1)
#    t <- t - log(u) / lambda
#    u1 <- runif(1)
#  }
#  t
#}


g <- function(x) {
  return(2 * x)
}

ts <- function(lamda, g, s) {
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


two_severs_parallel_queue <- function(T, f, lambda, lambda_1, lambda_2) {
  #变量的初始化
  t <- 0
  N_A <- 0
  c_1 <- 0
  c_2 <- 0
  SS <- c(0, 0, 0)
  t_1 <- Inf
  t_2 <- Inf
  t_A <- ts(lambda, f, t)
  #IDtime记录每个顾客到达系统和离开系统的时间
  IDtime <- NULL
  #对顾客进入系统时间小于或者等于T的过程进行模拟。
  while (t_A <= T) {
    #当前时刻当下一个顾客进入时间小于或者等于在两个服务台的服务时间的系统模拟
    if (t_A <= t_1 & t_A <= t_2) {

      t <- t_A
      N_A <- N_A + 1
      t_A <- ts(lambda, f, t)
      IDtime <- cbind(IDtime, c(t, 0))

      if (SS[1] == 0) {
        SS <- SS + c(1, N_A, 0)
        t_1 <- t + rexp(1, lambda_1)
      }

      else if (SS[1] == 1) {
        if (SS[3] == 0) {
          SS <- SS + c(1, 0, N_A)
          t_2 <- t + rexp(1, lambda_2)
        }else {
          SS <- SS + c(1, N_A, 0)
          t_1 <- t + rexp(1, lambda_1)
        }
      }

      else {
        SS <- SS + c(1, 0, 0)
      }

    }

      #当前时间第一个服务台完成服务的时间小于下一个顾客进入系统的时间以及第二个服务台#的服务时间的系统模拟。
    else if (t_1 < t_A & t_1 <= t_2) {
      t <- t_1
      c_1 <- c_1 + 1
      IDtime[, SS[2]] <- IDtime[, SS[2]] + c(0, t)

      if (SS[1] == 1) {
        SS <- c(0, 0, 0)
        t_1 <- Inf
      }

      else if (SS[1] == 2) {
        SS <- SS - c(1, SS[2], 0)
        t_1 <- Inf
      }

      else {
        SS[1] <- SS[1] - 1
        SS[2] <- max(SS[2], SS[3]) + 1
        t_1 <- t + rexp(1, lambda_1)
      }

    }
      #当前时间第二个服务台完成服务的时间小于下一个顾客进入系统的时间以及第一个服务台#服务时间的系统模拟。
    else {
      t <- t_2
      c_2 <- c_2 + 1
      IDtime[, SS[3]] <- IDtime[, SS[3]] + c(0, t)

      if (SS[1] == 1) {
        SS <- c(0, 0, 0)
        t_2 <- Inf
      }

      else if (SS[1] == 2) {
        SS <- SS - c(1, 0, SS[3])
        t_2 <- Inf
      }

      else {
        SS[1] <- SS[1] - 1
        SS[3] <- max(SS[2], SS[3]) + 1
        t_2 <- t + rexp(1, lambda_2)
      }

    }

  }


  #T时刻后对系统过程进行模拟。

  while (SS[1] > 0) {

    #当前时间第一个服务台完成服务的时间小于第二个服务台的服务时间的系统模拟。
    if (t_1 <= t_2) {
      t <- t_1
      c_1 <- c_1 + 1
      IDtime[, SS[2]] <- IDtime[, SS[2]] + c(0, t)

      if (SS[1] == 1) {
        SS <- c(0, 0, 0)
        t_1 <- Inf
      }

      else if (SS[1] == 2) {
        SS <- SS - c(1, SS[2], 0)
        t_1 <- Inf
      }

      else {
        SS[1] <- SS[1] - 1
        SS[2] <- max(SS[2], SS[3]) + 1
        t_1 <- t + rexp(1, lambda_1)
      }
    }

      #当前时间第二个服务台完成服务的时间小于第一个服务台的服务时间的系统模拟。
    else {
      t <- t_2
      c_2 <- c_2 + 1
      IDtime[, SS[3]] <- IDtime[, SS[3]] + c(0, t)

      if (SS[1] == 1) {
        SS <- c(0, 0, 0)
        t_2 <- Inf
      }

      else if (SS[1] == 2) {
        SS <- SS - c(1, 0, SS[3])
        t_2 <- Inf
      }

      else {
        SS[1] <- SS[1] - 1
        SS[3] <- max(SS[2], SS[3]) + 1
        t_2 <- t + rexp(1, lambda_2)
      }
    }

  }

  #In_depart_time为每个每个顾客进入和离开的时间
  #sever_1_num为第一个服务台服务的人数
  #sever_2_num为第二个服务台服务的人数
  #systym_num为来到系统的总人数
  #mean_time为平均每人在系统中逗留的时间

  x <- mean(IDtime[2,] - IDtime[1,])
  mylist <- list(In_depart_time = IDtime, sever_1_num = c_1, sever_2_num = c_2, systym_num = N_A, mean_time = x)
  return(mylist)
}

#对并联的双服务台的排队系统模拟n次后得到的一个人在系统中逗留的平均时间
mean_time_of_systym <- function(n, T, f, lambda, lambda_1, lambda_2) {
  s <- 0
  n <- 0
  for (i in 1:10) {
    x <- two_severs_parallel_queue(T, f, lambda, lambda_1, lambda_2)
    meantime <- x$mean_time
    num <- x$systym_num
    s <- meantime * num + s
    n <- n + num
  }
  return(s / n)
}

T = 10
f <- function(t) 3 + 4 / (1 + t)
lambda <- 4
lambda_1 <- 4
lambda_2 <- 3
n <- 100
mean_time <- mean_time_of_systym(n, T, f, lambda, lambda_1, lambda_2)
mean_time