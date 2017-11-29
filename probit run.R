# rm(list=ls(all=TRUE))
library('mvtnorm')
set.seed(1)

setwd('U:\\modeling abundance\\git_probit')
source('probit functions.R')
source('probit main func.R')
dat=read.csv('fake data.csv',as.is=T)
dat$x1=dat$x; 

ngibbs=10000
covs=c('x1')
prior.var=1

res=probit.gibbs(dat=dat,ngibbs=ngibbs,covs=covs,burnin=5000,prior.var=prior.var)

par(mfrow=c(2,1))
plot(res$betas[,1],type='l')
abline(h=true.b1,col='red')

plot(res$betas[,2],type='l')
abline(h=true.b2,col='red')