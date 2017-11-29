tnorm <- function(n,lo,hi,mu,sig){   #generates truncated normal variates based on cumulative normal distribution
  #normal truncated lo and hi
  
  if(length(lo) == 1 & length(mu) > 1)lo <- rep(lo,length(mu))
  if(length(hi) == 1 & length(mu) > 1)hi <- rep(hi,length(mu))
  
  q1 <- pnorm(lo,mu,sig) #cumulative distribution
  q2 <- pnorm(hi,mu,sig) #cumulative distribution
  
  z <- runif(n,q1,q2)
  z <- qnorm(z,mu,sig)
  
  #qnorm can give some imprecise results
  cond=z<lo;    z[cond] = lo[cond]
  cond=z==-Inf; z[cond] = lo[cond]
  cond=z>hi;    z[cond] = hi[cond]
  cond=z==Inf;  z[cond] = hi[cond]
  z
}
#----------------------------------------------------------------------------------------------
sample.z=function(y,xmat,betas,nobs){
  z=rep(NA,nobs)
  media=xmat%*%betas
  
  #------------------------------
  #get z
  cond=y==0
  z[cond]=tnorm(sum(cond),lo=-100,hi=0,mu=media[cond],sig=1)
  z[!cond]=tnorm(sum(!cond),lo=0,hi=100,mu=media[!cond],sig=1)
  z
}
#----------------------------------------------------------------------------------------------
sample.betas=function(z,xmat,var1){
  pmedia=var1%*%t(xmat)%*%z
  t(rmvnorm(1,pmedia,var1))
}
