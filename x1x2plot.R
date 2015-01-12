
x.a = c(.1,.15)
x.b = c(.05,.95)

par(mfrow=c(3,2),cex=1.3,mgp=c(3.5,2,0),mar=c(4.5,5.5,.1,.3))

######### CP1

plot(0,0,ty='n',ylim=c(0,1),xlim=c(0,1),axes=FALSE,ylab=expression(x[2]),xlab=expression(x[1]))
x1 = c(0,.5,.5,1,1,0,0,.5,.5,0)
y1 = c(.5,.5,0,0,.5,.5,1,1,.5,.5)
polygon(x1,y1,col=rgb(1,0,0,.2),border=NA)
#x2 = c(0,.25,1,.75,0)
#y2 = c(1,.25,0,.75,1)
#polygon(x2,y2,col=rgb(0,0,1,.2),border=NA)
axis(1,at=2:4/2-1,lab=expression(theta - 5,
                                 theta,
                                 theta + 5
),las=1)
axis(2,at=2:4/2-1,lab=expression(theta - 5,
                                 theta,
                                 theta + 5
),las=1)
rect(0,0,1,1,border="black")

mtext("A",3,-2,adj=.9,cex=2)

points(x.a[1],x.a[2],pch='1')
points(x.b[1],x.b[2],pch='2')


plot(0,0,ty='n',ylim=c(-1,1),xlim=c(0,1),axes=FALSE,ylab=expression(x[2]-x[1]),xlab=expression(bar(x)))
polygon((x1+y1)/2,y1-x1,col=rgb(1,0,0,.2),border=NA)
#polygon((x2+y2)/2,y2-x2,col=rgb(0,0,1,.2),border=NA)
axis(2,at=-2:2*5/10,lab=-2:2*5,las=1)
axis(1,at=2:4/2-1,lab=expression(theta - 5,
                                 theta,
                                 theta + 5
),las=1)
polygon(c(0,.5,1,.5),c(0,-1,0,1))
mtext("B",3,-2,adj=.9,cex=2)

points(mean(x.a),x.a[2]-x.a[1],pch='1')
abline(h = x.a[2]-x.a[1],col="gray",lty=1)
points(mean(x.b),x.b[2]-x.b[1],pch='2')
abline(h = x.b[2]-x.b[1],col="gray",lty=1)

############# CP2

#5 - 10/sqrt(8)
plot(0,0,ty='n',ylim=c(0,1),xlim=c(0,1),axes=FALSE,ylab=expression(x[2]),xlab=expression(x[1]))
q = sqrt(2)/2
x3 = c(0,0,q,1,1,1-q)
y3 = c(1,q,0,0,1-q,1)
polygon(x3,y3,col=rgb(0,1,0,.2),border=NA)
axis(1,at=2:4/2-1,lab=expression(theta - 5,
                                 theta,
                                 theta + 5
),las=1)
axis(2,at=2:4/2-1,lab=expression(theta - 5,
                                 theta,
                                 theta + 5
),las=1)
rect(0,0,1,1,border="black")

mtext("C",3,-2,adj=.9,cex=2)

points(x.a[1],x.a[2],pch='1')
points(x.b[1],x.b[2],pch='2')


plot(0,0,ty='n',ylim=c(-1,1),xlim=c(0,1),axes=FALSE,ylab=expression(x[2]-x[1]),xlab=expression(bar(x)))
polygon((x3+y3)/2,y3-x3,col=rgb(0,1,0,.2),border=NA)
axis(2,at=-2:2*5/10,lab=-2:2*5,las=1)
axis(1,at=2:4/2-1,lab=expression(theta - 5,
                                 theta,
                                 theta + 5
),las=1)
polygon(c(0,.5,1,.5),c(0,-1,0,1))
mtext("D",3,-2,adj=.9,cex=2)

points(mean(x.a),x.a[2]-x.a[1],pch='1')
abline(h = x.a[2]-x.a[1],col="gray",lty=1)
points(mean(x.b),x.b[2]-x.b[1],pch='2')
abline(h = x.b[2]-x.b[1],col="gray",lty=1)

########## CP3

plot(0,0,ty='n',ylim=c(0,1),xlim=c(0,1),axes=FALSE,ylab=expression(x[2]),xlab=expression(x[1]))
#x1 = c(0,.5,.5,1,1,0,0,.5,.5,0)
#y1 = c(.5,.5,0,0,.5,.5,1,1,.5,.5)
#polygon(x1,y1,col=rgb(1,0,0,.2),border=NA)
x2 = c(0,.25,1,.75,0)
y2 = c(1,.25,0,.75,1)
polygon(x2,y2,col=rgb(0,0,1,.2),border=NA)
axis(1,at=2:4/2-1,lab=expression(theta - 5,
                                 theta,
                                 theta + 5
),las=1)
axis(2,at=2:4/2-1,lab=expression(theta - 5,
                                 theta,
                                 theta + 5
),las=1)
rect(0,0,1,1,border="black")

mtext("E",3,-2,adj=.9,cex=2)

points(x.a[1],x.a[2],pch='1')
points(x.b[1],x.b[2],pch='2')


plot(0,0,ty='n',ylim=c(-1,1),xlim=c(0,1),axes=FALSE,ylab=expression(x[2]-x[1]),xlab=expression(bar(x)))
#polygon((x1+y1)/2,y1-x1,col=rgb(1,0,0,.2),border=NA)
polygon((x2+y2)/2,y2-x2,col=rgb(0,0,1,.2),border=NA)
axis(2,at=-2:2*5/10,lab=-2:2*5,las=1)
axis(1,at=2:4/2-1,lab=expression(theta - 5,
                                 theta,
                                 theta + 5
),las=1)
polygon(c(0,.5,1,.5),c(0,-1,0,1))
mtext("F",3,-2,adj=.9,cex=2)

points(mean(x.a),x.a[2]-x.a[1],pch='1')
abline(h = x.a[2]-x.a[1],col="gray",lty=1)
points(mean(x.b),x.b[2]-x.b[1],pch='2')
abline(h = x.b[2]-x.b[1],col="gray",lty=1)

