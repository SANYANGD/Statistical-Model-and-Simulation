# Title     : TODO
# Objective : TODO
# Created by:
# Created on: 2020/9/27

x = 1:10
fx = table(sample(x, 50, replace = T)) / 50
p = as.numeric(fx)
max = max(p)
u1 = floor(10 * runif(100000)) + 1
barplot(table(u1))
u2 = runif(100000)
accept = u1[u2 <= p[u1] / max]
length(accept) / 200000 #效率
plot(x, p, ylim = c(0, max))
par(new = TRUE)
plot(table(accept) / length(accept), ylim = c(0, max))

