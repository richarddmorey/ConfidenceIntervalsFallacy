par(mfrow=c(2,1), mar = c(3.5,3.5,1,1),mgp=c(2,.5,0), las = 1, cex.axis = 1.1, cex.lab = 1.1, cex = 1.1)



FF = seq(0, 17, len = 250)



#### Figure A

Fstat.demo = 5

omega.sq = steigerCI.omega2(Fstat.demo,df1,df2,conf.level=cl)
omega.sq.other = .2

lambda.sq = N*J*omega.sq/(1-omega.sq)
lambda.sq.other = N*J*omega.sq.other/(1-omega.sq.other)

plot(FF, df(FF, df1,df2,ncp = lambda.sq[1]),ty='l', lwd=2, ylab="Density", xlab="F statistic", yaxt="n", xlim=c(0,15), col="purple")
lines(FF, df(FF, df1,df2,ncp = lambda.sq.other), lwd=2, col="gray",lty=2)
abline(v= Fstat.demo, lty=2)
abline(v = 0, col="gray")
abline(h = 0, col="gray")
text(3.5, par()$usr[4]*.8, col="purple",
     substitute(paste(omega^2,"=",o), list(o = round(omega.sq[1],2)))
)


FF0 = FF[FF>= Fstat.demo]
polygon(c(FF0,rev(FF0)), c(FF0*0, df(rev(FF0), df1,df2,ncp = lambda.sq.other)), 
        col=rgb(0,0,0,.1), lty=0)
text(9.5, .05, col="darkgray", paste0(round(100-pf(Fstat.demo,df1,df2,lambda.sq.other)*100,0),"%"),adj=c(.5,0))

polygon(c(FF0,rev(FF0)), c(FF0*0, df(rev(FF0), df1,df2,ncp = lambda.sq[1])), 
        col=rgb(1,0,1,.1), lty=0)
text(6,0 , col="purple", paste0(round(100*alpha/2,0),"%"),adj=c(.5,-.1))

text(8, par()$usr[4]*.45, col="darkgray",
     substitute(paste(omega^2,"=",o), list(o = round(omega.sq.other,2)))
)

mtext("A",3,-2,adj=.95,cex=2)

### Figure B

Fstat.demo = 5

omega.sq = steigerCI.omega2(Fstat.demo,df1,df2,conf.level=cl)

lambda.sq = N*J*omega.sq/(1-omega.sq)

plot(FF, df(FF, df1,df2,ncp = lambda.sq[2]),ty='l', lwd=2, ylab="Density", xlab="F statistic", yaxt="n", xlim=c(0,15), col="darkgreen",ylim=c(0,par()$usr[4]))
lines(FF, df(FF, df1,df2,ncp = lambda.sq.other), lwd=2, col="gray",lty=2)
abline(v= Fstat.demo, lty=2)
abline(v = 0, col="gray")
abline(h = 0, col="gray")

text(10, par()$usr[4]*.45, col="darkgreen",
     substitute(paste(omega^2,"=",o), list(o = round(omega.sq[2],2)))
)

FF0 = FF[FF<= Fstat.demo]
polygon(c(FF0,rev(FF0)), c(FF0*0, df(rev(FF0), df1,df2,ncp = lambda.sq.other)), 
        col=rgb(0,0,0,.1), lty=0)
text(1, .13, col="darkgray", paste0(round(pf(Fstat.demo,df1,df2,lambda.sq.other)*100,0),"%"),adj=c(.5,0))


polygon(c(FF0,rev(FF0)), c(FF0*0, df(rev(FF0), df1,df2,ncp = lambda.sq[2])), 
        col=rgb(.3,1,.3,.1), lty=0)
text(3.5,0 , col="darkgreen", paste0(round(100*alpha/2,0),"%"),adj=c(.5,-.1))


text(2, par()$usr[4]*.7, col="darkgray",
     substitute(paste(omega^2,"=",o), list(o = round(omega.sq.other,2)))
)


mtext("B",3,-2,adj=.95,cex=2)

### Figure C

#Fstat.demo = 5

#omega.sq = steigerCI.omega2(Fstat.demo,df1,df2,conf.level=cl)

#lambda.sq = N*J*omega.sq/(1-omega.sq)

#plot(FF, df(FF, df1,df2,ncp = lambda.sq[1]),ty='l', lwd=2, ylab="Density", xlab="F statistic", yaxt="n", xlim=c(0,15), col="blue")
#lines(FF, df(FF, df1,df2,ncp = lambda.sq[2]), lwd=2, col="red")
#abline(v= Fstat.demo, lty=2)
#abline(v = 0, col="gray")
#abline(h = 0, col="gray")
#text(3.5, par()$usr[4]*.8, col="blue",
#     substitute(paste(omega^2,"=",o), list(o = round(omega.sq[1],2)))
#     )

#text(10, par()$usr[4]*.45, col="red",
#     substitute(paste(omega^2,"=",o), list(o = round(omega.sq[2],2)))
#     )

#FF0 = FF[FF<= Fstat.demo]
#polygon(c(FF0,rev(FF0)), c(FF0*0, df(rev(FF0), df1,df2,ncp = lambda.sq[2])), 
#        col=rgb(1,0,0,.1), lty=0)
#text(3.5,0 , col="red", paste0(round(100*alpha/2,0),"%"),adj=c(.5,0))


#FF0 = FF[FF>= Fstat.demo]
#polygon(c(FF0,rev(FF0)), c(FF0*0, df(rev(FF0), df1,df2,ncp = lambda.sq[1])), 
#        col=rgb(0,0,1,.1), lty=0)
#text(6,0 , col="blue", paste0(round(100*alpha/2,0),"%"),adj=c(.5,0))


#mtext("C",3,-2,adj=.95,cex=2)

###
