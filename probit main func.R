probit.gibbs=function(dat,ngibbs,covs,burnin,prior.var){
  nobs=nrow(dat)
  
  #get initial values
  z=rep(-1,nobs)
  z[dat$y==1]=1
  xmat=data.matrix(cbind(1,dat[,covs]))
  npar=ncol(xmat)
  betas=rep(0,npar)
  xtx=t(xmat)%*%xmat
  prec=xtx+diag(1/c(10,prior.var),npar)
  var1=solve(prec)
  
  #gibbs sampler
  store.betas=matrix(NA,ngibbs,npar)
  options(warn=2)
  for (i in 1:ngibbs){
    print(i)
    z=sample.z(y=dat$y,xmat=xmat,betas=betas,nobs=nobs)
    betas=sample.betas(z=z,xmat=xmat,var1=var1)

    #store results
    store.betas[i,]=betas
  }
  
  after.burn=burnin:ngibbs
  list(betas=store.betas[after.burn,])
}

