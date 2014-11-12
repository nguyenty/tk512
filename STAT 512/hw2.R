#------------------ Problem 2
Cp <- matrix(c(-1,1,0,0,0, 0,-1,1,0,0, 0,0,-1,1,0 ,0,0,0,-1,1),byrow=T,nrow=4)

X <- t(Cp)%*%Cp

ni <- sqrt(diag(X))/sum(sqrt(diag(X))) * 50

#------------------ Problem 3

ni <- c(7,12,12,12,7)
mus <- c(10,11,12,12,12)
sigma <- 2
ne <- 10
N <- 50

qf(1-0.05,4,45)
# [1] 2.578739

muh1 <- 1/N * sum(ne*(mus))

# For all n_i = 10
lambda1 <- sum(ne* (mus-muh1)^2/sigma^2)
lambda1
#[1] 8

#Power of the F-test
1-pf(qf(1-0.05,4,45),4,45,ncp =lambda1)
#[1] 0.5540384

#(b) for the "optimal" sample sizes
muh2 <- 1/N * sum(ni * (mus))

lambda2 <- sum(ni * (mus-muh2)^2/sigma^2 )
lambda2
#[1] 6.62

# Power of the F-test for optimal sample sizes.
1-pf(qf(1-0.05,4,45),4,45,ncp =lambda2)
# [1] 0.4677505

x <- seq(0,15,by = 0.01)
plot(x,df(x,4,45),type="l")
lines(x,df(x,4,45,ncp = lambda1),lty=2,col=2)
lines(x,df(x,4,45,ncp = lambda2),lty=2,col=4)

#(c)
mus
np <-  (N * mus[i]/2 -sum(mus[-i]*ni[-i])/mus[i]
#---------------------Problem 8--------------------

a <- c(27.3, 34.6, 31.8, 35.4)
b <- c(41.9, 36.8, 38.2, 38.4)
c <- c(36.5, 39.2, 35.1, 34.7)
tr <- as.factor(rep(1:3,each=4))

#(a)
y <- c(a,b,c)

yl <- lm(y~tr)
summary(yl)
anova(yl)

# anova(yl)
#Analysis of Variance Table

#Response: y
#          Df Sum Sq Mean Sq F value  Pr(>F)  
#tr         2 87.620  43.810  5.9112 0.02295 *
#Residuals  9 66.702   7.411                  


#(b)
Confidence I


width <- 5
Sp <- 7.411 #((length(a)-1) * var(a) + (length(b)-1) * var(b) + )/((length(a)-1) + (length(b)-1))
n0 <- 8 * (qnorm(1-0.05/2) * sqrt(Sp)/width)^2
n <- 8 * (qt(1-0.05/2,2*(n0-1))*sqrt(Sp)/width)^2