par(mfrow=c(2,1),cex=1.3,mgp=c(2.5,1,0),mar=c(4.5,4,2,.3),las=1)

plot(0,0,ty='n',xlim=c(0,10),ylim=c(0,5),yaxt="n",ylab="Density",xlab="Width (meters)")
polygon(c(0,10,0,0),c(0,0,2.5,0),col=rgb(1,0,0,.2))
polygon(c(0,5,5,0),c(0,0,5,0),col=rgb(0,0,1,.2))
abline(h=0,col="gray")
text(7.5,1.25,"CP1",col="red")
text(5,4,"CP3",col="blue",adj=-.1)
abline(v=10/3,col="darkgray",lty=2)

segments(2 * (5 - 10/sqrt(8)),0,2 * (5 - 10/sqrt(8)),5,col=rgb(0,1,0,.5),lwd=2)
points(2 * (5 - 10/sqrt(8)),5,col=rgb(0,1,0,.5),lwd=2,pch=19)
text(2 * (5 - 10/sqrt(8)),5*2/3,"CP2",col="green",adj=1)

mtext("A",3,-2,adj=.9,cex=2)


bayesCIprob = Vectorize(function(q){
  q = abs(q)
  if(q < (1/4)){
    return(
      1/2 - 8/3*q^2
    )
  }else if(q<(3/4)){
    return(
      3/4 - 2*q + 4/3*q^2
    )
  }else{
    return(0)
  }
},"q")

freqCIprob = Vectorize(function(q){
  (2*(q + 1/2) * (1-q-1/2))*(abs(q)<1/2)
},"q")


pxbar2 <- Vectorize(function(xbar,theta,scale){
  if(xbar<(theta-scale)) return(0) 
  if(xbar>(theta+scale)) return(1)
  z = (xbar - theta)/scale
  if(xbar<theta) return((z + 1)^2/2)
  if(xbar>theta) return(1 - (1 - z)^2/2)
},"xbar")


pC2in <- Vectorize(function(thetaStar,theta,scale){
  w = scale * (1 - 1 / sqrt(2))
  pxbar2(thetaStar + w, theta, scale) - pxbar2(thetaStar - w, theta, scale)
},"thetaStar")


qq = seq(-3/4,3/4,len=200)
aa = bayesCIprob(qq)
bb = freqCIprob(qq)
cc = pC2in(qq,theta=0,scale=.5) 
plot(qq,aa,ty='l',ylab="Probability CI contains value",xlab=expression(paste("Alternative ", theta,"'")),col="blue",axes=FALSE,lwd=2)
lines(qq,bb,col="red",lwd=2)
lines(qq,cc,col="green",lwd=2)

box()
axis(2)
axis(1,at=-3/4,lab=expression(theta-7.5 ))
axis(1,at=-2/4,lab=expression(theta-5 ))
axis(1,at=-1/4,lab=expression(theta-2.5 ))
axis(1,at=0,lab=expression(theta))
axis(1,at=3/4,lab=expression(theta+7.5 ))
axis(1,at=2/4,lab=expression(theta+5 ))
axis(1,at=1/4,lab=expression(theta+2.5 ))

mtext("B",3,-2,adj=.9,cex=2)

abline(h=2/9,col="darkgray",lty=2)
