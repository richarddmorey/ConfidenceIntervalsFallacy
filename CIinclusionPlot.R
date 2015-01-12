par(mfrow=c(1,2))


plot(c(0,0),ylim=c(0,1),xlim=c(0,10),ylab="Proportion of impossible values",xlab=expression(paste("|",x[1] - x[2],"|")),ty='n')

segments(0,0,10,0,col=rgb(0,0,1,1),lwd=2)
text(10,.02,"CP3",adj=c(1,0),col=rgb(0,0,1,1))
xx = seq(0,10,len=100)

yy1 = pmax(1-(10 - xx)/xx,0) 
lines(xx,yy1,col=rgb(1,0,0,1),lwd=2)
text(2,.1,"CP1",adj=c(.5,1),col=rgb(1,0,0,1))

yy2 = pmax(1-(10 - xx)/(2*(5-10/sqrt(8))),0) 
lines(xx,yy2,col=rgb(0,1,0,1),lwd=2)
text(8.3,.4,"CP2",adj=c(0,1),col=rgb(0,1,0,1))


mtext("A",3,-2,adj=.1,cex=2)

#######

plot(c(0,0),ylim=c(0,1),xlim=c(0,10),ylab="Prob. 50% CI contains true value",xlab=expression(paste("|",x[1] - x[2],"|")),ty='n')

segments(0,.5,10,.5,col=rgb(0,0,1,1),lwd=2)
text(8,.48,"CP3",adj=c(1,1),col=rgb(0,0,1,1))
xx = seq(0,10,len=100)

yy1 = pmin(xx/(10-xx),1)
lines(xx,yy1,col=rgb(1,0,0,1),lwd=2)
text(2,.1,"CP1",adj=c(.5,1),col=rgb(1,0,0,1))

yy3 = pmin(2*(5-10/sqrt(8)) / (10 - xx),1)
lines(xx,yy3,col=rgb(0,1,0,1),lwd=2)
text(7,.8,"CP2",adj=c(.4,1),col=rgb(0,1,0,1))

mtext("B",3,-2,adj=.1,cex=2)


