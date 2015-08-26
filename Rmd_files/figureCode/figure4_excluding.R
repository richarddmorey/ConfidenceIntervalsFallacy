
source('sub.cis.R')

par(las=1,mar=c(4.5,4,.5,.5), mgp=c(2.5,1,0))

qq = seq(-3/4,3/4,len=200)
aa = bayesCIprob(qq)
bb = freqCIprob(qq) # NP
cc = pC2in(qq,theta=0,scale=.5) #SD
dd = welchCIprob(qq) # UMP
plot(qq,aa,ty='l',ylab="Probability CI contains value",xlab=expression(paste("False alternative ", theta,"'")),col="blue",axes=FALSE,lwd=2)
lines(qq,bb,col="darkred",lwd=2,lty=2)
lines(qq,cc,col="darkred",lwd=2,lty=5)
lines(qq,dd,col="darkred",lwd=2,lty=3)
abline(h=.5,col="gray",lwd=2,lty=4)

box()
axis(2)
axis(1,at=-3/4,lab=expression(theta-7.5 ))
axis(1,at=-2/4,lab=expression(theta-5 ))
axis(1,at=-1/4,lab=expression(theta-2.5 ))
axis(1,at=0,lab=expression(theta))
axis(1,at=3/4,lab=expression(theta+7.5 ))
axis(1,at=2/4,lab=expression(theta+5 ))
axis(1,at=1/4,lab=expression(theta+2.5 ))

text(.6, .47, "T", col="darkgray", cex = 1)
text(.3, .4, "NP", col="darkred", cex = 1)
text(.5, 3.5, "SD", col="darkred", cex = 1)
text(-.1, .3, "UMP", col="darkred", cex = 1)
text(-.6, .1, "B", col="blue", cex = 1)

legend(-3/4,.4,legend=c("SD"), col="darkred",lty=5,lwd=2,bty='n',text.col="darkred")
