

par(mfrow=c(2,2), mar = c(4,3,1,0),mgp=c(2.5,1,0), las = 1, cex.axis = 1.3, cex.lab = 1.3, cex = 1.3)

xx = seq(0, min(HPD[2]*5,1), len=100)  
plot(xx,likelihood.omega2(xx, Fstat, df1, df2), ty='l', yaxt="n", ylab = "",xlab=expression(omega^2),lwd=2, col="blue")
mtext(paste0("F = ",round(Fstat,3)),3,.1,adj=.95, cex=1.5)
mtext(paste0("Conf. = ",round(cl,2)),3,.1,adj=.05, cex=1.5)
arrows(testCI[1], par()$usr[4]*.75, testCI[2]+.0005,par()$usr[4]*.75, col="darkred", lwd=3, code = 3, angle = 90, length=.15)
abline(v=0,col="gray")
arrows(HPD[1], par()$usr[4]*.25, HPD[2],par()$usr[4]*.25, col="blue", lwd=3, code = 3, angle = 90, length=.15)
abline(h = 0, col="gray")
mtext("A",3,-2,adj=.9,cex=2.5)

mtext("Likelihood", 2, 1.5, adj=.5, las=0, cex=1.7)


Fstat = Fstat0
testCI = steigerCI.omega2(Fstat,df1,df2,conf.level=cl)
HPD = BayesHPD2.omega2(Fstat,df1,df2,conf.level=cl)

par(mar = c(4,2.5,1,.5))

plot(xx,likelihood.omega2(xx, Fstat, df1, df2), ty='l', yaxt="n", ylab = "",xlab=expression(omega^2),lwd=2, col="blue")
mtext(paste0("F = ",round(Fstat,3)),3,.1,adj=.95, cex=1.5)
mtext(paste0("Conf. = ",round(cl,2)),3,.1,adj=.05, cex=1.5)
arrows(testCI[1], par()$usr[4]*.75, testCI[2]+.0005,par()$usr[4]*.75, col="darkred", lwd=3, code = 3, angle = 90, length=.15)
abline(v=0,col="gray")
arrows(HPD[1], par()$usr[4]*.25, HPD[2],par()$usr[4]*.25, col="blue", lwd=3, code = 3, angle = 90, length=.15)
abline(h = 0, col="gray")
mtext("B",3,-2,adj=.9,cex=2.5)

Fstat = Fstat0
cl = .70
testCI = steigerCI.omega2(Fstat,df1,df2,conf.level=cl)
HPD = BayesHPD2.omega2(Fstat,df1,df2,conf.level=cl)

par(mar = c(4,3,1,0))

plot(xx,likelihood.omega2(xx, Fstat, df1, df2), ty='l', yaxt="n", ylab = "",xlab=expression(omega^2),lwd=2, col="blue")
mtext(paste0("F = ",round(Fstat,3)),3,.1,adj=.95, cex=1.5)
mtext(paste0("Conf. = ",round(cl,2)),3,.1,adj=.05, cex=1.5)
arrows(testCI[1], par()$usr[4]*.75, testCI[2]+.0005,par()$usr[4]*.75, col="darkred", lwd=3, code = 3, angle = 90, length=.15)
abline(v=0,col="gray")
arrows(HPD[1], par()$usr[4]*.25, HPD[2],par()$usr[4]*.25, col="blue", lwd=3, code = 3, angle = 90, length=.15)
abline(h = 0, col="gray")
mtext("C",3,-2,adj=.9,cex=2.5)

mtext("Likelihood", 2, 1.5, adj=.5, las=0, cex=1.7)

Fstat =  qf(.9749, df1, df2)
cl = .95
testCI = steigerCI.omega2(Fstat,df1,df2,conf.level=cl)
HPD = BayesHPD2.omega2(Fstat,df1,df2,conf.level=cl)

par(mar = c(4,2.5,1,.5))

xx = seq(0, min(HPD[2]*5,1), len=100)  
plot(xx,likelihood.omega2(xx, Fstat, df1, df2), ty='l', yaxt="n", ylab = "",xlab=expression(omega^2),lwd=2, col="blue")
mtext(paste0("F = ",round(Fstat,3)),3,.1,adj=.95, cex=1.5)
mtext(paste0("Conf. = ",round(cl,2)),3,.1,adj=.05, cex=1.5)
arrows(testCI[1], par()$usr[4]*.75, testCI[2]+.0005,par()$usr[4]*.75, col="darkred", lwd=3, code = 3, angle = 90, length=.15)
abline(v=0,col="gray")
arrows(HPD[1], par()$usr[4]*.25, HPD[2],par()$usr[4]*.25, col="blue", lwd=3, code = 3, angle = 90, length=.15)
abline(h = 0, col="gray")
mtext("D",3,-2,adj=.9,cex=2.5)
