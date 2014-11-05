
## data
dat <-                  matrix(c(2, 150, 8, 7.54, 6.66, 
                              30, 100, 40, 12.65, 15.96, 
                              30, 150, 8, 12.46, 12.62, 
                              2, 100, 40, 10.95, 17.68, 
                              30, 100, 8, 15.12, 17.48, 
                              30, 150, 40, 14.96, 14.96, 
                              2, 150, 40, 8.03, 8.84, 
                              2, 100, 8, 14.68, 15.18), nrow = 8, byrow = T)

dat
dat[dat[,1]==2,1] <- -1
dat[dat[,1]==30,1] <- 1

dat[dat[,2]==100,2] <- -1
dat[dat[,2]==150,2] <- 1

dat[dat[,3]==8,3] <- -1
dat[dat[,3]==40,3] <- 1
dat
# d <- as.data.frame(rbind(dat[,-5], dat[, -4]))
# d
X <- model.matrix(~V1*V2*V3, data = as.data.frame(dat))
dim(X)
colnames(X)
block <-NULL 
block[1:8] <- as.numeric(X[,8]==1)+1
block[9:16] <- as.numeric(X[,8]==1) + 3
block <- as.factor(block)
block

dat.block <- (cbind(rbind(X, X), block, y= c(dat[,4], dat[,5])))
rownames(dat.block) <- 1:16
dat.block <- as.data.frame(dat.block)
dat.block$block <- as.factor(dat.block$block)
anova1 <- anova(lm(y ~block, data = dat.block))
ssb<- anova1[1,2]
sum(anova1[,2])
ssb
## factorial estimate in the case of unblock ####
dat.block
X2 <- rbind(X, X)
dat <- as.data.frame(dat)
dat2 <- cbind(rbind(dat[,1:3], dat[,1:3]), y = c(dat[,4], dat[,5]))
lmout2 <- lm(y ~ V1*V2*V3, data = dat2)

coef <- summary(lmout2)$coefficients[,1]
anova2 <- anova(lmout2)
ssct <- sum(anova2[,2])
sst <- sum(est.coef[ -c(1, 8)]^2)*16
sst

sum(anova2[1:6,2])
sse <- ssct - sst - ssb 
sse

se.est <- sse/6
se.est
# Ftest

ftest <- (sst/6)/(sse/6)
pvalue <- 1 - pf(ftest, 6,6)
pvalue 
##########finish 1a)###############

########1b#####################
## LSE of all estimable factorial effects and compute their standard error######
coef[-c(1,8)]

### standard error of those estimates#####
se.coef <- sqrt(sse/6/16) 
se.coef
sse


###2#############
v123 <- coef[8]
n <- 16
sse.2b <- ssb - n*v123^2
mse.2b <- sse.2b/2
mse.2b

se.v123 <- sqrt(mse.2b/n)
se.v123
ttest  <- v123/se.v123

pvalue2 <- 2*(1-pt(ttest, 2))
pvalue2

ftest2 <- n*v123^2/mse.2b

pvalue22 <- 1 - pf(ftest2, 1, 2)
pvalue22==pvalue2
########finish 2#############


#############3###############

block <-c(1,1,2,2,3,4,3,4,5,5,6,6,7,8,7,8)
block <- as.factor(block)
block

dat.block <- (cbind(rbind(X, X), block, y= c(dat[,4], dat[,5])))
rownames(dat.block) <- 1:16
dat.block <- as.data.frame(dat.block)
dat.block$block <- as.factor(dat.block$block)
anova1 <- anova(lm(y ~block, data = dat.block))
ssb<- anova1[1,2]
sum(anova1[,2])
ssb
## factorial estimate in the case of unblock ####
dat.block
X2 <- rbind(X, X)
dat <- as.data.frame(dat)
dat2 <- cbind(rbind(dat[,1:3], dat[,1:3]), y = c(dat[,4], dat[,5]))
lmout2 <- lm(y ~ V1*V2*V3, data = dat2)

coef <- summary(lmout2)$coefficients[,1]
anova2 <- anova(lmout2)
ssct <- sum(anova2[,2])
sst <- sum(est.coef[ -c(1, 5,6,7)]^2)*16
sst
anova2
sum(anova2[c(1,2,3,7),2])
sse <- ssct - sst - ssb 
sse

mse <- sse/4
se.est <- sqrt(mse/n)
se.est
# Ftest

ftest <- (sst/4)/(sse/4)
pvalue <- 1 - pf(ftest, 4,4)
pvalue 
##########finish 1a)###############

########1b#####################
## LSE of all estimable factorial effects and compute their standard error######
coef[c(2,3,4,8)]

### standard error of those estimates#####
se.coef <- sqrt(mse/16) 
se.coef



###4#############
v123 <- coef[c(5,6,7)]
n <- 16
sse.2b <- ssb - n*sum(v123^2)
mse.2b <- sse.2b/4
mse.2b

se.v123 <- sqrt(mse.2b/n)
se.v123

ssb
ftest2 <- n*sum(v123^2)/mse.2b

pvalue22 <- 1 - pf(ftest2, 1, 2)
pvalue22
########finish 4#############

#################5########################3


