rm(list=ls(all=TRUE))
set.seed(1)

n=500
x=seq(from=-1,to=1,length.out=n)

true.b1=b1=1
true.b2=b2=-1

med=b1+b2*x
true.z=z=rnorm(n,mean=med,sd=1)
y=rep(0,n)
y[z>0]=1

plot(x,y)
dat=data.frame(x=x,y=y)

setwd('U:\\modeling abundance\\git_probit')
write.csv(dat,'fake data.csv',row.names=F)