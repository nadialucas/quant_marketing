# load the data from 'pbout.txt' in R

library(readr)
library(dplyr)
library(psych)
library(bayesm)
setwd("/Users/nadialucas/Documents/quant_marketing/pset1")
pb_raw <- read_table("pbout.txt")

# Compute what you consider the relevant descriptive statistics by product alternative and report them in a table
### total mkt share (by panelist/trip)
### avg # of brands purchased by each hh
### avg expenditure per hh
### expenditure rank
### fraction of trips that pb is purchased during
### avg price of alternative purchased
### avg price of alternative in general


# first clean the data, so add a 1-9 on each of the panelist/weeks

pb_cleaned <- pb_raw %>% 
  group_by(panelid, date) %>% 
  mutate(choice_alternative = row_number()) %>%
  mutate(avg_price = mean(price)) %>%
  mutate(chosen = ifelse(choice == choice_alternative, 1, 0))



### avg price of alternative in general
# ok and also collapse these down to product level for the future
pb_avg_prices <- pb_cleaned %>% group_by(choice_alternative) %>% mutate(mean_price = mean(price))

pb_avg_prices_condl_purchased <- pb_cleaned %>% filter(choice == choice_alternative) %>% mutate(mean_price = mean(price))
  

# do we want date fixed effects?

### total mkt share (by panelist/trip)


describe(pb_avg_prices_condl_purchased)


describeBy(pb_avg_prices_condl_purchased, group = pb_avg_prices_condl_purchased$choice)


lmout = lm(chosen ~ price, data = pb_cleaned)
summary(lmout)




#playing around with bayesm
data(bank)
choiceAtt = bank$choiceAtt
Z = bank$demo
# demean Z, demographic data and remove the identifiers
Z = subset(Z, select = c("age", "income", "gender"))
Z = t(t(Z)-apply(Z,2,mean))
# set number of choice alternative
p = 14
ncoef = 14



hh = levels(factor(choiceAtt$id))
nhh = length(hh)
lgtdata = NULL
for (i in 1:nhh) {
  y = choiceAtt[choiceAtt[,1] == hh[i], 2]
  nobs = length(y)
  X = as.matrix(choiceAtt[choiceAtt[,1]==hh[i], c(3:16)])
  lgtdata[[i]] = list(y=y, X=X)
}

cat("Finished Reading data", fill = TRUE)




# TRY AGAIN


if(nchar(Sys.getenv("LONG_TEST")) != 0) {R=10000} else {R=10}
set.seed(66)
p = 3 # num of choice alterns
ncoef = 3
nlgt = 300 # num of cross sectional units
nz = 2
Z = matrix(runif(nz*nlgt),ncol=nz)
Z = t(t(Z) - apply(Z,2,mean)) # demean Z
ncomp = 3 # num of mixture components
Delta = matrix(c(1,0,1,0,1,2),ncol=2)
comps=NULL
comps[[1]] = list(mu=c(0,-1,-2), rooti=diag(rep(1,3)))
comps[[2]] = list(mu=c(0,-1,-2)*2, rooti=diag(rep(1,3)))
comps[[3]] = list(mu=c(0,-1,-2)*4, rooti=diag(rep(1,3)))
pvec = c(0.4, 0.2, 0.4)
## simulate from MNL model conditional on X matrix
simmnlwX= function(n,X,beta) {
  k = length(beta)
  Xbeta = X%*%beta
  j = nrow(Xbeta) / n
  Xbeta = matrix(Xbeta, byrow=TRUE, ncol=j)
  Prob = exp(Xbeta)
  iota = c(rep(1,j))
  denom = Prob%*%iota
  Prob = Prob/as.vector(denom)
  y = vector("double",n)
  ind = 1:j
  for (i in 1:n) {
    yvec = rmultinom(1, 1, Prob[i,])
    y[i] = ind%*%yvec
  }
  return(list(y=y, X=X, beta=beta, prob=Prob))
}
## simulate data
simlgtdata = NULL
ni = rep(50, 300)
for (i in 1:nlgt) {
  betai = Delta%*%Z[i,] + as.vector(rmixture(1,pvec,comps)$x)
  Xa = matrix(runif(ni[i]*p,min=-1.5,max=0), ncol=p)
  X = createX(p, na=1, nd=NULL, Xa=Xa, Xd=NULL, base=1)
  outa = simmnlwX(ni[i], X, betai)
  simlgtdata[[i]] = list(y=outa$y, X=X, beta=betai)
}
## plot betas
if(0){
  bmat = matrix(0, nlgt, ncoef)
  for(i in 1:nlgt) {bmat[i,] = simlgtdata[[i]]$beta}
  par(mfrow = c(ncoef,1))
  for(i in 1:ncoef) { hist(bmat[,i], breaks=30, col="magenta") }
}
## set parms for priors and Z
Prior1 = list(ncomp=5)
keep = 5
Mcmc1 = list(R=R, keep=keep)
Data1 = list(p=p, lgtdata=simlgtdata, Z=Z)
## fit model without sign constraints
out1 = rhierMnlRwMixture(Data=Data1, Prior=Prior1, Mcmc=Mcmc1)
cat("Summary of Delta draws", fill=TRUE)
summary(out1$Deltadraw, tvalues=as.vector(Delta))
cat("Summary of Normal Mixture Distribution", fill=TRUE)
summary(out1$nmix)
## plotting examples
if(1) {
  plot(out1$betadraw)
  plot(out1$nmix)
}
## fit model with constraint that beta_i2 < 0 forall i
Prior2 = list(ncomp=5, SignRes=c(0,-1,0))
out2 = rhierMnlRwMixture(Data=Data1, Prior=Prior2, Mcmc=Mcmc1)

cat("Summary of Delta draws", fill=TRUE)
summary(out2$Deltadraw, tvalues=as.vector(Delta))
cat("Summary of Normal Mixture Distribution", fill=TRUE)
summary(out2$nmix)
## plotting examples
if(0) {
  plot(out2$betadraw)
  plot(out2$nmix)
}

# not this one

Data = list(lgtdata = lgtdata, Z=Z, p = 14)
Mcmc = list(R = 20000, sbeta = .2, keep = 20)
out = rhierMnlRwMixture(Data = Data, Mcmc = Mcmc)

