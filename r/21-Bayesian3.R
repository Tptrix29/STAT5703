# Example 1: multivariate normal pretest and posttest scores on math exams
x = structure(c(59, 43, 34, 32, 42, 38, 55, 67, 64, 45, 49, 72, 34, 
                70, 34, 50, 41, 52, 60, 34, 28, 35, 77, 39, 46, 26, 38, 43, 68, 
                86, 77, 60, 50, 59, 38, 48, 55, 58, 54, 60, 75, 47, 48, 33), .Dim = c(22L, 
                                                                                      2L), .Dimnames = list(NULL, c("pretest", "posttest")))
# Manual sampling from the posterior with Gibbs 
mu0<-c(50,50) # by design of the exam
L0<-matrix( c(625,312.5,312.5,625),nrow=2,ncol=2)

nu0<-4
S0 = matrix( c(625,312.5,312.5,625),nrow=2,ncol=2) # Prior covariance matrix

n<-dim(x)[1]
xbar = apply(x, 2, mean)
Sigma<-cov(x) ; THETA<-SIGMA<-NULL

library(MASS) # for the mvrnorm function
# install.packages("MCMCpack")
library(MCMCpack) # for the riwish function

for(s in 1:20000) 
{
  
  ###update theta
  Ln<-solve( solve(L0) + n*solve(Sigma) )
  mun<-Ln%*%( solve(L0)%*%mu0 + n*solve(Sigma)%*% xbar )
  theta = mvrnorm(1,mun,Ln)  
  
  
  ###update Sigma
  Sn<- S0 + ( t(x)-c(theta) )%*%t( t(x)-c(theta) ) 
  Sigma<-riwish(nu0+n, Sn)
  # Sigma<-solve( rwish(1, nu0+n, solve(Sn)) )
  
  # Update the output
  THETA = rbind(THETA,theta)
  SIGMA = rbind(SIGMA,c(Sigma)) # notice the Sigma matrix is vectorized
}


quantile(   THETA[,2]-THETA[,1], prob=c(.025,.5,.975) )

mean( THETA[,2]-THETA[,1])
mean( THETA[,2]>THETA[,1]) 
hist(THETA[,2])
hist(THETA[,1], add = T, col = "red")
hist(THETA[,2]-THETA[,1])

bivn.kde <- kde2d(THETA[,1], THETA[,2], n = 200) 
contour(bivn.kde, nlevels = 50)

apply(THETA, 2, mean) # posterior mean of theta

cov(THETA) # covariance matrix of the posterior distribution of theta
# Compare to covariance of prior distribution
L0

#install.packages("emdbook")
library(emdbook)
library(coda)
HPDregionplot(mcmc(THETA), add = T, col = "red")
abline(a=0,b=1, col="red")


# More RStan examples
# Example 2: regression

par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))

x1<-c(0,0,0,0,0,0,1,1,1,1,1,1) # Two types of exercises
x2<-c(23,22,22,25,27,20,31,23,27,28,22,24) # age
# change in oxygen uptake
y<-c(-0.87,-10.74,-3.27,-1.97,7.50,-7.25,17.05,4.96,10.40,11.05,0.26,2.51)

par(mfrow=c(1,1))
plot(y~x2,pch=16,xlab="age",ylab="change in maximal oxygen uptake", 
     col=c("black","gray")[x1+1])
legend(27,0,legend=c("aerobic","running"),pch=c(16,16),col=c("gray","black"))

# Model with 
# b1 = group difference
# b2 = age effect
# b3 = interaction
par(mfrow=c(2,2),mar=c(3,3,1,1),mgp=c(1.75,.75,0),oma=c(0,0,.25,0))


plot(y~x2,pch=16,col=c("black","gray")[x1+1],ylab="change in maximal oxygen uptake",xlab="",xaxt="n")
abline(h=mean(y[x1==0]),col="black") 
abline(h=mean(y[x1==1]),col="gray")
mtext(side=3,expression(paste(beta[2]==0,"  ",beta[3]==0)) )

plot(y~x2,pch=16,col=c("black","gray")[x1+1],xlab="",ylab="",xaxt="n",yaxt="n")
abline(lm(y~x2),col="black")
abline(lm((y+.5)~x2),col="gray")
mtext(side=3,expression(paste(beta[1]==0,"  ",beta[3]==0)) )

plot(y~x2,pch=16,col=c("black","gray")[x1+1],
     xlab="age",ylab="change in maximal oxygen uptake" )
fit<-lm( y~x1+x2)
abline(a=fit$coef[1],b=fit$coef[3],col="black")
abline(a=fit$coef[1]+fit$coef[2],b=fit$coef[3],col="gray")
mtext(side=3,expression(beta[3]==0)) 

plot(y~x2,pch=16,col=c("black","gray")[x1+1],
     xlab="age",ylab="",yaxt="n", main = "With Interaction")
abline(lm(y[x1==0]~x2[x1==0]),col="black")
abline(lm(y[x1==1]~x2[x1==1]),col="gray")

# OLS for interaction model
n<-length(y)
X<-cbind(rep(1,n),x1,x2,x1*x2)
p<-dim(X)[2]
(beta.ols<- solve(t(X)%*%X)%*%t(X)%*%y)

# With Rstan language:
library(rstan)
write(
  "data {
  int N; // number of observations
  vector[N] y; // response
  // number of columns in the design matrix X
  int K;
  // design matrix X
  // should not include an intercept
  matrix [N, K] X;
  real scale_alpha; // prior sd on alpha
  vector[K] scale_beta; // prior sds on betas
  real loc_sigma; // prior sd on the error sigma
}

parameters {
 real alpha; // intercept
  vector[K] beta;  // regression coefficients beta vector
  real sigma;
}

// this is a convenient way to utilize the fact the mean of Y depends on x
transformed parameters {
  vector[N] mu; // defines the mean of each Y
  mu = alpha + X * beta;  //notice * is vector multiplication
}

model {
  // priors
  alpha ~ normal(0, scale_alpha);
  beta ~ normal(0, scale_beta); // notice the beta priors are independent
  // to model correlated betas you can use lkj_corr prior
  sigma ~ exponential(loc_sigma); // this does not match the textbook
 
  y ~ normal(mu, sigma); // likelihood
}
", "Example3.stan")

library(rstan)
mod = stan_model("Example3.stan")

mod_data = list(
  X = X[,-1],
  K = ncol(X)-1,
  N = length(x1),
  y = y
)

mod_data$scale_alpha = 150 
mod_data$scale_beta <- c(30, 6, 5) # prior parameters for beta sd
mod_data$loc_sigma <- sd(y)

mod_fit = sampling(mod, data = mod_data, iter = 3000)

summary(mod_fit, pars = c("alpha", "beta", "sigma"), probs = c(0.025, 0.975))$summary
# b3 CI contains zero, therefore the interaction effect is not significant.

# Easier version (will do next time)
# Start over and do with rethinking package  to bypass the RStan coding

d = data.frame(y,x1,x2)
d$exercise <- ifelse( d$x1 == 1 , 2 , 1 )
dat = d[,-2]

# Sanity check
summary(lm(y~ x1*x2, data=d))

library(rethinking)
# Including exercise type as the only predictor:
m1 = ulam(
  alist(
    y ~ dnorm( mu , sigma ) ,
    mu <- a[exercise] ,
    a[exercise] ~ dnorm(-45, 5) ,
    sigma ~ dexp(1)
  ) , data = dat, iter = 2000, chains = 4)
precis(m1, depth = 2)
# traceplot(m1)

post = extract.samples(m1)
hist(post$a[,1])
hist(post$a[,2])
HPDI(post$a[,1]-post$a[,2], prob = 0.95)


# Age only
m2 = ulam(
  alist(
    y ~ dnorm( mu , sigma ) ,
    mu <- a+b*x2 ,
    a~ dnorm(-45 , 5) ,
    b ~ dnorm(0,5),
    sigma ~ dexp( 1 )
  ) , data = dat, iter = 2000, chains = 4)
precis(m2, depth = 2)

# Sanity check
summary(lm(y~ x2, data=d))

# Parallel lines
m3s = ulam(
  alist(
    y ~ dnorm( mu , sigma ) ,
    mu <- a[exercise]+b*x2 ,
    a[exercise]~ dnorm(-45 , 5) ,
    b ~ dnorm(0,5),
    sigma ~ dexp( 1 )
  ) , data = dat, iter = 2000, chains = 4)
precis(m3s, depth = 2, prob = 0.95)


# Sanity check:
summary(lm(y ~ x1+x2))

# Nonparallel lines:
m4s = ulam(
  alist(
    y ~ dnorm( mu , sigma ) ,
    mu <- a[exercise]+b[exercise]*x2 ,
    a[exercise]~ dnorm(-45 , 5) ,
    b[exercise] ~ dnorm(0,5),
    sigma ~ dexp( 1 )
  ) , data = dat, iter = 2000, chains = 4)
precis(m4s, depth = 2, prob = 0.95)
pairs(m4s)

post4 = extract.samples(m4s)
post4$diff = post4$b[,2] - post4$b[,1]
hist(post4$diff)
plot(post4$b[,1],post4$b[,2])
HPDI(post4$diff, prob = 0.95)


# If we have time
# Example 3: Random effects regression model
#### NELS data
load("nelsSES.RData") 

ids<-sort(unique(nels$sch_id)) 
m<-length(ids)
Y<-list() ; X<-list() ; N<-NULL
for(j in 1:m) 
{
  Y[[j]]<-nels[nels$sch_id==ids[j], 4] 
  N[j]<- sum(nels$sch_id==ids[j])
  xj<-nels[nels$sch_id==ids[j], 3] 
  xj<-(xj-mean(xj))
  X[[j]]<-cbind( rep(1,N[j]), xj  )
}


#### OLS fits
S2.LS<-BETA.LS<-NULL
for(j in 1:m) {
  fit<-lm(Y[[j]]~-1+X[[j]] )
  BETA.LS<-rbind(BETA.LS,c(fit$coef)) 
  S2.LS<-c(S2.LS, summary(fit)$sigma^2) 
} 

par(mar=c(2.75,2.75,.5,.5),mgp=c(1.7,.7,0))
par(mfrow=c(1,2))

plot( range(nels[,3]),range(nels[,4]),type="n",xlab="SES", 
      ylab="math score")
for(j in 1:m) {    abline(BETA.LS[j,1],BETA.LS[j,2],col="gray")  }

BETA.MLS<-apply(BETA.LS,2,mean)
abline(BETA.MLS[1],BETA.MLS[2],lwd=2)

N = nrow(nels)
J = length(unique(nels$sch_id))
id<-rep(1:J,each=50) #index of groups 
id = nels$sch_id
id = as.numeric(as.factor(id))
K=2
X = cbind(rep(1, N), nels$stu_ses)

y = nels$stu_mathscore

dim(X)
head(X)

(mu0 <- apply(BETA.LS,2,mean)) # The prior for betas is the average OLS of all schools

write("
data {
  int<lower=1> N; //the number of observations
  int<lower=1> J; //the number of groups
  int<lower=1> K; //number of columns in the model matrix
  int<lower=1,upper=J> id[N]; //vector of group indices
  matrix[N,K] X; //the model matrix
  vector[N] y; //the response variable
}
parameters {
  vector[K] theta; //population-level regression coefficients
  vector<lower=0>[K] tau; //the standard deviation of the regression coefficients
  vector[K] beta_raw[J];
  real<lower=0> sigma; //standard deviation of the individual observations
}
transformed parameters {
  vector[K] beta[J]; //matrix of group-level regression coefficients
  //computing the group-level coefficient, based on non-centered parameterization based on section 22.6 STAN (v2.12) user's guide
  for(j in 1:J){
    beta[j] = theta + tau .* beta_raw[j]; // regression parameters of each group
  }
}
model {
  vector[N] mu; //linear predictor
  //priors
  theta[1] ~ normal(48, 5); //weakly informative priors on the regression coefficients
  theta[2] ~ normal(2.4, 5);
  tau ~ cauchy(0,2.5); //weakly informative priors, see section 6.9 in STAN user guide
  sigma ~ gamma(2,0.1); //weakly informative priors, see section 6.9 in STAN user guide
  for(j in 1:J){
   beta_raw[j] ~ normal(0,1); //fill the matrix of group-level regression coefficients;
   //implies beta~normal(gamma,tau)
  }
  for(n in 1:N){
    mu[n] = X[n] * beta[id[n]]; //compute the linear predictor using relevant group-level regression coefficients
  }
  //likelihood
  y ~ normal(mu,sigma);
}", "Example3.stan")

library(rstan)
mod <- stan_model("Example3.stan")

mod1_fit <- sampling(mod,  data=list(N=N,J=J,K=K,id=id,X=X,y=y))
b=summary(mod1_fit, pars = c("beta"),probs = c(0.025, 0.975))$summary[,1]

a=summary(mod1_fit, pars = c("theta", "sigma", "tau"),probs = c(0.025, 0.975))$summary
a

plot( range(nels[,3]),range(nels[,4]),type="n",xlab="SES", 
      ylab="math score")
for (i in 1:J)
{
  if ((i %% 2) ==1)abline(b[i],b[i+1])  
}
abline(a[,1][1], a[,1][2], col = "red")