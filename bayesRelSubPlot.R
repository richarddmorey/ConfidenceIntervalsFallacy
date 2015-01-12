library(MCMCpack)

par(mfrow=c(2,2),cex=1.4,mgp=c(3.5,2,0),mar=c(4.5,5.5,2,2))


jointPost <- Vectorize(function(sig2,mu,y,mu0,n0,sig20,nu0){
  exp(sum(dnorm(y,mu,sqrt(sig2),log=TRUE))+dnorm(mu,mu0,sqrt(sig2/n0),log=TRUE)+log(dinvgamma(sig2,nu0/2,nu0*sig20/2)
  ))},"sig2")

margPost <- Vectorize(function(mu,y,mu0,n0,sig20,nu0){
  integrate(jointPost,0,Inf,mu=mu,y=y,mu0=mu0,n0=n0,sig20=sig20,nu0=nu0)$value  
},"mu")

margPostAna = Vectorize(function(mu,ybar,s2,N,mu0,n0,nu0,sig20){
  mu1 = (N * ybar + n0 * mu0)/(N + n0)
  nu1 = N + nu0
  S21 = nu0 * sig20 + (N - 1) * s2 + N * n0/(n0 + N) * (ybar - mu0)
  sig21 = S21/nu1
  dt((mu - mu1)/sqrt(sig21/(n0 + N)), nu1)/sqrt(sig21/(n0 + N))
},"mu")

credInt = function(ybar,s2,N,mu0,n0,nu0,sig20,alpha=.5){
  
  n1 = n0 + N
  mu1 = (N * ybar + n0 * mu0)/n1
  nu1 = N + nu0
  S1 = nu0 * sig20 + (N - 1) * s2 + N * n0/n1 * (ybar - mu0)^2
  sig21 = S1/nu1
  
  tstar = qt(1-alpha/2,nu1)
  
  mu1 + c(-1,1)*tstar*sqrt(sig21/n1)
}


M = 1000
N = 2

mu0=8
n0=1
sig20=12^2
nu0=1
alpha = .5


sig2 = rinvgamma(M,nu0/2,nu0*sig20/2)
mu = rnorm(M,mu0,sqrt(sig2/n0))
y = mapply(rnorm,mean=mu,sd=sqrt(sig2),MoreArgs=list(n=N))
ybar = apply(y,2,mean)
s2 = apply(y,2,var)

creds = mapply(credInt,ybar=ybar,s2=s2,MoreArgs=list(N=N,mu0=mu0,n0=n0,sig20=sig20,nu0=nu0,alpha=alpha))
confs = apply(y,2,sort)

creds2 = t((t(creds) - mu)/sqrt(sig2))
confs2 = t((t(confs) - mu)/sqrt(sig2))

containsCred = creds[1,]<mu & creds[2,]>mu
containsConf = confs[1,]<mu & confs[2,]>mu

widthCred = creds[2,]-creds[1,]
ordCred = order(widthCred)

widthConf = confs[2,]-confs[1,]
ordConf = order(widthConf)

par(mfcol=c(2,2))

plot(0,0,xlim=range(confs2),ylim=c(0,M+1),ty='n',xlab="(Normalized) Conf. Int.", ylab="")
segments(confs2[1,ordConf],1:M,confs2[2,ordConf],1:M,col=2-containsConf[ordConf])

plot(cumsum(containsConf[ordConf])/M,ty='l',ylab="Cumulative Prob.")
abline(0,.5/M,col="red")


plot(0,0,xlim=range(creds2),ylim=c(0,M+1),ty='n',xlab="(Normalized) Cred. Int.", ylab="")
segments(creds2[1,ordCred],1:M,creds2[2,ordCred],1:M,col=2-containsCred[ordCred])

plot(cumsum(containsCred[ordCred])/M,ty='l',ylab="Cumulative Prob.")
abline(0,.5/M,col="red")

