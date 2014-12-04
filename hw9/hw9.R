# model matrix#####
library(plyr)
set.seed(1)
D <- matrix(c(-1, 1, 1, -1, 1, 1, 1, -1, -1, -1, 
              -1, -1, -1, 1, -1, 1, 1, -1, 1, 1, 
              1, -1, -1, -1, 1, -1, 1, 1, -1, 1, 
              1, 1, -1, -1, -1, 1, -1, 1, 1, -1, 
              -1, 1, 1, 1, -1, -1, -1, 1, -1, 1, 
              1, -1, 1, 1, 1, -1, -1, -1, 1, -1), ncol = 10, byrow = T)
X <- cbind(rep(1, 6), D)
intercept <- 2
m <- c(1, 2, 5, 10)
s.b <- c(1, 2, 5, 10)
sim.func <- function(m, s.b){
  main.effect <- rep(0, 10)
  nonzero.effect<- sample(1:10, m)
  main.effect[nonzero.effect] <- rnorm(m, 0, s.b)
  noise <- rnorm(6, 0, 1)
  mean.vector <- c(intercept, main.effect)
  y <- X%*%(mean.vector) + noise
  Rsquare <- laply(2:11, function(i) summary(lm(y ~X[,i]))$r.squared)
  ans1 <- as.numeric(which.max(Rsquare) %in%nonzero.effect)
  ans2 <- as.numeric(which.max(Rsquare) %in%which.max(abs(nonzero.effect)))
  return(c(ans1=ans1, ans2 = ans2))
}

result.func <- function(m,s.b){
  out <- laply(1:1000, function(i)sim.func(m,s.b))
  res <- apply(out, 2, mean)
  return(res)
}

par <- matrix(c(1,1, 
                1, 2, 
                1,3,
                1,4,
                2,1,
                2,2,
                2,3,
                2,4,
                3,1,
                3,2,
                3,3,
                3,4,
                4,1,
                4,2,
                4,3,
                4,4), ncol = 2, byrow = T)
final.result <- laply(1:16, function(i)result.func(m[par[i,1]],s.b[par[i,2]]))
final.result2 <- cbind(m = rep(m, each = 4), sigma.beta = rep(s.b, 4), final.result)
final.result2
