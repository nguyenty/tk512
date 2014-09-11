#2.1
install.packages("binhf")
library(binhf)
mu <- rep(c(2.64, 2.28, 1.82, 1.40), each = 3)
px_y <- mu
p0_y <- rep(mean(mu), 12)



sst <- (norm(px_y, 0)^2 - norm(p0_y, 0)^2)
sst
mse <- 0.3675
df1 <- 3
df2 <- 8
ftest <- (sst/df1)/(mse)
1 - pf(ftest, df1, df2)

# 2.3 

inv.s.lambda <- 60/(sqrt(3/2) +2* sqrt(11/36) + 2* sqrt(11/36) + 3*sqrt(11/54))
 
n1 <- sqrt(3/2)*inv.s.lambda
n2 <- n3 <- sqrt(11/36)*inv.s.lambda
n4 <- sqrt(11/54)*inv.s.lambda

round(c(n1, n2, n3, n4)) + 1 
t(round(c(n1, n2, n3, n4)) +1)%*%c(1, 2, 2, 3)


# 2.4

library(plyr)

pf(q, df1, df2, ncp, lower.tail = TRUE, log.p = FALSE)
df1 <- 3
df2 <- 8
alpha <- 0.1
ncp <- c(6, 1/3, 17/3)
q <- qf(1-alpha, df1, df2)

design_power <- laply(1:length(ncp), function(i) pf(q,df1, df2, ncp[i]))
design_power

