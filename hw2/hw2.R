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

#1.4 
#(a) 
sigma_crd <- 0.06
N <- b*t
alpha <- 0.1

sigma_cbd <- sigma_crd*qt(1-alpha/2, N-t)/qt(1-alpha/2, N-b-t+1)

#(b)
tau <- c(0.1, 0.1, 0, 0, 0)

q_tau <- b*sum((tau - mean(tau))^2)
sigma_crd
ncp_crd <- q_tau/sigma_crd^2

power_crd <- 1 - pf(qf(1-alpha, df1 = t -1, df2 = N - t), df1 = t-1,
                    df2 = N-t, ncp = ncp_crd)
power_crd


power_cbd <- function(sigma){
  1 - pf(qf(1-alpha, df1 = t -1, df2 = N - t - b+1), df1 = t-1,
         df2 = N-t - b +1, ncp = q_tau/sigma^2) - power_crd
}

uniroot(power_cbd, interval = c(0.000001, 0.1))
