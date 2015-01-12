par(mfrow=c(1,2),cex=1.3,mgp=c(2.5,1,0),mar=c(4.5,3.5,2,.2),las=1)


dbivar = function(xbar,s,mu,sigma=1,N=2){
  exp(dnorm(xbar,mu,sigma/sqrt(N), log=TRUE) + log(2) + log(s) + dchisq((N-1)*s^2/sigma^2,N-1,log=TRUE)/(sigma^2/(N-1)))
}

M = 10000
sigma = 1

mu = 0
critp = .25
critt = qt(critp/2,1)

y = rnorm(M*2,mu,sigma)
dim(y) = c(M,2)

ybar = (y[,1] + y[,2])/2
s = abs(y[,1] - y[,2])/sqrt(2)
tval = ybar/s*sqrt(2)
pvalGtCrit = (tval>critt & tval< -critt)
contains = apply(y,1,function(v) min(v)<mu & max(v)>mu)

mean(pvalGtCrit)

counts = table(pvalGtCrit,contains)
counts / rowSums(counts)

myCols = apply(col2rgb(1:4,.1),2,function(v) rgb(v[1],v[2],v[3],.1*255,max=255))

xxbar = seq(mu-3*sigma/sqrt(2),mu+3*sigma/sqrt(2),len=50)
ss = seq(0,4,len=50)

zz = outer(xxbar,ss,dbivar,mu=mu,sigma=sigma,N=2)

contour(xxbar,ss,zz,xlim=mu+4*c(-1,1)*sigma/sqrt(2),ylim=c(0,4),xlab=expression(bar(x)),ylab="s")

#plot(ybar,s,bg=myCols[1+pvalGtCrit + 2*contains],pch=21,col=NULL,
#     xlim=mu+4*c(-1,1)*sigma/sqrt(2),ylim=c(0,4),xlab=expression(bar(x)))
abline(0,-sqrt(2)/critt,lwd=2,lty=2)
abline(0,sqrt(2)/critt,lwd=2,lty=2)
abline(-mu*sqrt(2),sqrt(2),lwd=2,lty=3)
abline(mu*sqrt(2),-sqrt(2),lwd=2,lty=3)

mtext(expression(paste(mu," = 0")),3,adj=.9,cex=1.3)




mu = 2.5

y = rnorm(M*2,mu,sigma)
dim(y) = c(M,2)

ybar = (y[,1] + y[,2])/2
s = abs(y[,1] - y[,2])/sqrt(2)
tval = ybar/s*sqrt(2)
pvalGtCrit = (tval>critt & tval< -critt)
contains = apply(y,1,function(v) min(v)<mu & max(v)>mu)

mean(pvalGtCrit)

counts = table(pvalGtCrit,contains)
counts / rowSums(counts)

myCols = apply(col2rgb(1:4,.1),2,function(v) rgb(v[1],v[2],v[3],.1*255,max=255))

xxbar = seq(mu-3*sigma/sqrt(2),mu+3*sigma/sqrt(2),len=50)
ss = seq(0,4,len=50)

zz = outer(xxbar,ss,dbivar,mu=mu,sigma=sigma,N=2)

contour(xxbar,ss,zz,xlim=mu+4*c(-1,1)*sigma/sqrt(2),ylim=c(0,4),xlab=expression(bar(x)),ylab="s")

#plot(ybar,s,bg=myCols[1+pvalGtCrit + 2*contains],pch=21,col=NULL,
#     xlim=mu+4*c(-1,1)*sigma/sqrt(2),ylim=c(0,4),xlab=expression(bar(x)))
abline(0,-sqrt(2)/critt,lwd=2,lty=2)
abline(0,sqrt(2)/critt,lwd=2,lty=2)
abline(-mu*sqrt(2),sqrt(2),lwd=2,lty=3)
abline(mu*sqrt(2),-sqrt(2),lwd=2,lty=3)

mtext(expression(paste(mu," = 2.5")),3,adj=.9,cex=1.3)
