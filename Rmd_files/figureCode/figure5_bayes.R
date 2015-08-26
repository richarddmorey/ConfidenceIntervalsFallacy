
par(mgp=c(2.5,1,0),mar=c(0,3.5,3.5,1.5),cex=1.3,mfcol=c(3,2),cex.axis=1.3,cex.lab=1.3)


plot(0,0,ty='n',ylim=c(0,2),xlim=c(-.5,1.5),axes=F,ylab="Prior",xlab="")
axis(3,at=1:5/2-1,lab=expression(theta - 10,
                                 theta - 5,
                                 theta,
                                 theta + 5,
                                 theta + 10
))
#axis(2,at=0)

abline(h=1, col="blue", lwd=2)
abline(h=0,col="gray")
box()

par(mar=c(1,3.5,1,1.5))



plot(0,0,ty='n',ylim=c(0,2),xlim=c(-.5,1.5),axes=F,ylab="Likelihood",xlab="")
points(x.a,0*x.a + 1.5,pch=21,col="black",bg="lightblue",cex=1)
box()
abline(h=0,col="gray")
rect(x.a[1] - .5, 1.5 - .1, x.a[1] + .5, 1.5 + .1, col=rgb(0,0,1,.2),lty=0)
rect(x.a[2] - .5, 1.5 - .1, x.a[2] + .5, 1.5 + .1, col=rgb(0,0,1,.2),lty=0)
segments(min(x.a) + .5, 0, min(x.a) + .5, 1, col="blue", lwd=2)
segments(max(x.a) - .5, 0, max(x.a) - .5, 1,col="blue", lwd=2)
segments(max(x.a) - .5, 1, min(x.a) + .5, 1,  col="blue", lwd=2)

#axis(2,at=0)

par(mar=c(3.5,3.5,0,1.5))

plot(0,0,ty='n',ylim=c(0,2),xlim=c(-.5,1.5),axes=F,ylab="Posterior",xlab="Location")
box()
abline(h=0,col="gray")
segments(min(x.a) + .5, 0, min(x.a) + .5, 1,  col="blue", lwd=2)
segments(max(x.a) - .5, 0, max(x.a) - .5, 1,  col="blue", lwd=2)
segments(max(x.a) - .5, 1, min(x.a) + .5, 1,  col="blue", lwd=2)

#axis(2,at=0)

axis(1,at=1:5/2-1,lab=expression(theta - 10,
                                 theta - 5,
                                 theta,
                                 theta + 5,
                                 theta + 10
))

rect(mean(x.a) - (1/4 - diff(x.a)/4),
     0, mean(x.a) + (1/4 - diff(x.a)/4), 1, 
     col=rgb(0,0,1,.2),lty=0)

##### Second column

par(mar=c(0,1.5,3.5,3.5))


plot(0,0,ty='n',ylim=c(0,2),xlim=c(-.5,1.5),axes=F,ylab="",xlab="")
xx = seq(par()$usr[1],par()$usr[2],len=100)

lines(xx,dnorm(xx,.25,1)*3, col="blue", lwd=2)
axis(3,at=1:5/2-1,lab=expression(theta - 10,
                                 theta - 5,
                                 theta,
                                 theta + 5,
                                 theta + 10
))

xx = seq(-1,2, len=200)

abline(h=0,col="gray")
box()

par(mar=c(1,1.5,1,3.5))



plot(0,0,ty='n',ylim=c(0,2),xlim=c(-.5,1.5),axes=F,ylab="Likelihood",xlab="")
points(x.b,0*x.b + 1.5,pch=21,col="black",bg="lightblue",cex=1)
box()
abline(h=0,col="gray")
rect(x.b[1] - .5, 1.5 - .1, x.b[1] + .5, 1.5 + .1, col=rgb(0,0,1,.2),lty=0)
rect(x.b[2] - .5, 1.5 - .1, x.b[2] + .5, 1.5 + .1, col=rgb(0,0,1,.2),lty=0)
segments(min(x.b) + .5, 0, min(x.b) + .5, 1, col="blue", lwd=2)
segments(max(x.b) - .5, 0, max(x.b) - .5, 1, col="blue", lwd=2)
segments(max(x.b) - .5, 1, min(x.b) + .5, 1, col="blue", lwd=2)


par(mar=c(3.5,1.5,0,3.5))

plot(0,0,ty='n',ylim=c(0,2),xlim=c(-.5,1.5),axes=F,ylab="Posterior",xlab="Location")
box()
abline(h=0,col="gray")
segments(min(x.b) + .5, 0, min(x.b) + .5, dnorm(min(x.b) + .5,.25,1)*3, col="blue", lwd=2)
segments(max(x.b) - .5, 0, max(x.b) - .5, dnorm(max(x.b) - .5,.25,1)*3, col="blue", lwd=2)
xx = seq(max(x.b) - .5,min(x.b) + .5,len=100)
lines(xx,dnorm(xx,.25,1)*3, col="blue", lwd=2)

axis(1,at=1:5/2-1,lab=expression(theta - 10,
                                 theta - 5,
                                 theta,
                                 theta + 5,
                                 theta + 10
))

total.area  = pnorm(min(x.b) + .5,.25,1) - pnorm(max(x.b) - .5,.25,1)
left.side = qnorm(pnorm(max(x.b) - .5,.25,1) + total.area*.25, .25, 1)
right.side = qnorm(pnorm(max(x.b) - .5,.25,1) + total.area*.75, .25, 1)

xx = seq(left.side,right.side,len=100)

polygon(c(xx, rev(xx)), c(xx*0, dnorm(rev(xx),.25,1)*3), col=rgb(0,0,1,.2),lty=0)

