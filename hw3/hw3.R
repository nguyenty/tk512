#1####
mse <- 400
t <- 4
diet <- c("C1", "R5", "R10", "T")

mean_trt <- c(158, 240, 282, 279)

mean_overall <- mean(mean_trt)

ls_trt <- mean_trt - mean_overall

# 6 contrast for pair comparisons
 C <- matrix(c(1, -1, 0, 0, 
               1, 0, -1, 0, 
               1, 0, 0, -1, 
               0, 1, -1, 0, 
               0, 1, 0, -1, 
               0, 0, 1, -1
               ), byrow = T, 
             nrow = 6)

C

#  least square estimate of the constrast

Ct <- as.vector(C%*%ls_trt)

# their corresponding variance
se_Ct <- unlist(lapply(1:nrow(C), function(i) sqrt(mse * sum(C[i,]^2)/t)))

# test statistics 
t_test <- abs(Ct/se_Ct)

# df 
df<- (t-1)*(t-2)

# pvalues for each test - two sided test

pvalues <- unlist(lapply(1:length(t_test), function(i){
  2*(1 - pt(t_test[i], df))
}))

pvalues


#2 ####

# This belongs to Version 2 of the replicate LSD
r <- 2 # number of replicates

df2 <-  r*t^2-1 - (r-1) - (t-1) - (t-1) - r*(t-1) # Df for the error is 

# test statistics are the same, only degree of freedom change 
pvalues2 <- unlist(lapply(1:length(t_test), function(i){
  2*(1 - pt(t_test[i], df2))
}))

pvalues2

# 4####

xx <- matrix(c(1, 2, 1, 5, 
               10, 11, 12, 13), 
             byrow = T, nrow = 2)
t(xx)
