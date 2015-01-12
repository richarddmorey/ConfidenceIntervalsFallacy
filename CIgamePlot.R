par(las=1,cex=1.3)

aa = seq(.001,6,len=1000)

winningProp = .5 + pchisq(aa^2,1)*(1-pchisq(aa^2,1))

plot(aa,winningProp,ty='l',log="x",xlim=c(.005,5),ylab="Probability that Paul wins",
     xlab=expression(paste("Criterion (a, in units of ",sigma,")",sep='')),
     xaxt='n',lwd=2)
axis(1,at=4^(-4:-1),lab=paste("1/",4^(4:1),sep=""))
axis(1,at=4^(0:1),lab=4^(0:1))

abline(h=.5,col="gray")

abline(v=qnorm(.75),lty=2,col="darkgray")
