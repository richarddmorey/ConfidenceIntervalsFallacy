source('sub.cis.R')

par(las=1,mar=c(4.5,4,.5,.5), mgp=c(2.5,1,0))

bayes.prec = width.sub(bayes.sub, 10)
samp.prec = width.sub(sampling_dist.sub, 10)
nonpara.prec = width.sub(nonpara.sub, 10)
ump.prec = width.sub(ump.sub, 10)

plot(samp.prec$precision, samp.prec$width, ty='l', 
     xlab = "Uncertainty (width of likelihood, meters)", 
     ylab = "CI width (meters)", lty = 5, lwd = 2, col = "darkred", ylim = c(0,10))
lines(bayes.prec$precision, bayes.prec$width, lty = 1, lwd = 2, col = "blue")
segments(0, 0, 5, 5, lty=3, lwd=2, col="darkred") # UMP 1
segments(5, 5, 10, 0, lty=4, lwd=2, col="darkred") # UMP 2
segments(0, 10, 5, 5, lty=2, lwd = 2, col = "darkred") # nonpara
#lines(nonpara.prec$precision, nonpara.prec$width,lty = 2, lwd = 2, col = "darkred")
#lines(ump.prec$precision, ump.prec$width, lty = 3, lwd = 2, col = "darkred")
#lines(ump.prec$precision, ump.prec$precision, lty = 3, lwd = 2, col = rgb(0,0,0,.3))

text(2, 9, "NP", col="darkred", cex = 1)
text(.5, 3.5, "SD", col="darkred", cex = 1)
text(.5, 1.5, "UMP", col="darkred", cex = 1)
text(8.8, 5, "B", col="blue", cex = 1)
