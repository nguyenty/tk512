#1.3 
tau <- c(2.657, 2.725, 2.861, 2.867, 2.890)
tau_bar <- mean(tau)
b <- 10
t <- 5
sst <- b*sum((tau - tau_bar)^2)
mse <- 0.04
ftest <- (sst/(t-1))/mse

ftest

pvalue <- 1 - pf(ftest, df1 = t-1, df2 = b*t - b - t +1)
pvalue
