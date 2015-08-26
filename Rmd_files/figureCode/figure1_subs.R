library(grImport)
#PostScriptTrace('submarine-coloring.eps','submarine.xml')
submarinePic = readPicture('../vectorFigs/submarine.xml')

ci2.width=.5*(1-1/sqrt(2))

layout(matrix(c(1,2,2,2,3,3,3),1,7))

par(mgp=c(3.5,2,0),mar=c(4.5,0,.5,0),cex=1.3)


plot(0,0,ty='n',ylim=c(-1,5.3),xlim=c(-1,1),axes=FALSE,xlab="",ylab="")

text(0:5*0+1,0:5,c("Bubbles","Likelihood","Samp. Dist.","Nonpara.","UMP","Bayes"),adj=1)

####

par(mar=c(4.5,1.5,.5,.5),cex=1.3)

x.a = c(.1,.15)


plot(0,0,ty='n',ylim=c(-1,5.3),xlim=c(-.5,1.5),axes=F,ylab="",xlab="Location")
abline(h=0:5)
axis(1,at=1:5/2-1,lab=expression(theta - 10,
                                 theta - 5,
                                 theta,
                                 theta + 5,
                                 theta + 10
))
points(x.a,0*x.a,pch=21,col="black",bg="lightblue",cex=1)
rect(0,-1,1,5,col=rgb(0,0,0,.1),lty=0)
abline(v=.5,lty=2)
mtext("A",3,-1,adj=.9,cex=2)

# Likelihood
post.height=.2
rect(x.a[2]-.5,1 - post.height/2,x.a[1]+.5,1 + post.height/2, col=rgb(0,0,1,.2),lty=0)

# Sampling distribution
arrows(mean(x.a)-ci2.width,2,mean(x.a)+ci2.width,2,code=3,ang=90,len=.1,lwd=3,col="darkred")

# Nonparametric
arrows(x.a[1],3,x.a[2],3,code=3,ang=90,len=.1,lwd=3,col="darkred")

# UMP
ump = sort(x.a)
if(diff(ump) > .5){
  ump = c(ump[2] - .5, ump[1] + .5) 
}
arrows(ump[1],4,ump[2],4,code=3,ang=90,len=.1,lwd=3,col="darkred")

# Cred. Interval
post.width = 1 - diff(x.a)
cred.int = c(x.a[2]-.5 + .25*post.width, x.a[1]+.5 - .25*post.width)
arrows(cred.int[1],5,cred.int[2],5,code=3,ang=90,len=.1,lwd=3,col="blue")

picture(submarinePic, 0, -1, 1, -.2)



#######

x.b = c(.05,.95)


plot(0,0,ty='n',ylim=c(-1,5.3),xlim=c(-.5,1.5),axes=F,ylab="",xlab="Location")
abline(h=0:5)
axis(1,at=1:5/2-1,lab=expression(theta - 10,
                                 theta - 5,
                                 theta,
                                 theta + 5,
                                 theta + 10
))
points(x.b,0*x.b,pch=21,col="black",bg="lightblue",cex=1)
rect(0,-1,1,5,col=rgb(0,0,0,.1),lty=0)
abline(v=.5,lty=2)
mtext("B",3,-1,adj=.9,cex=2)

# Likelihood
post.height=.2
rect(x.b[2]-.5,1 - post.height/2,x.b[1]+.5,1 + post.height/2, col=rgb(0,0,1,.2),lty=0)

# Sampling distribution
arrows(mean(x.b)-ci2.width,2,mean(x.b)+ci2.width,2,code=3,ang=90,len=.1,lwd=3,col="darkred")

# Nonparametric
arrows(x.b[1],3,x.b[2],3,code=3,ang=90,len=.1,lwd=3,col="darkred")

# UMP
ump = sort(x.b)
if(diff(ump) > .5){
  ump = c(ump[2] - .5, ump[1] + .5) 
}
arrows(ump[1],4,ump[2],4,code=3,ang=90,len=.1,lwd=3,col="darkred")

# Cred. Interval
post.width = 1 - diff(x.b)
cred.int = c(x.b[2]-.5 + .25*post.width, x.b[1]+.5 - .25*post.width)
arrows(cred.int[1],5,cred.int[2],5,code=3,ang=90,len=.1,lwd=3,col="blue")

picture(submarinePic, 0, -1, 1, -.2)
