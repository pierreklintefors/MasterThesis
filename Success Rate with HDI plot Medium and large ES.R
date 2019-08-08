rm(list=ls(all=TRUE))
setwd("~/Magister/Rwork")


load("workspace SeqBFandHDIwithSnowfallMediumEffect.RData")

##Plot success-rate
loadfonts(device="win") 

windows(width = 8,height = 4)
par(mfrow = c(1,2), mar = c(4,3.3,2,3.3))


## Success rate Medium ES
plot(PropTally$nstop, PropTally$propsuccess*100, type="l", col="darkgreen", lwd=2, ylab="",xlab="", ylim = c(0,100), family = "serif")

abline(h=seq(from = 0, to = 100, by = 10), v=seq(from = 10, to = 100, by = 10), col="gray", lty=3)
title(main = paste("(A) Success rate/HDI width, effect size: 0.5"), family = "serif", cex = 2)
title(ylab = "Propotion of successful simulations", line = 2, family = "serif")
title(xlab = expression(paste(italic(n),' = number of participants in each group')),line= 2.2, family = "serif")

par(new = TRUE)

###Load workspace for medium effect size
load("workspace HDI width n10-500 Small ES.RData")

plot(M_hdiwd[,1], M_hdiwd[,2], type="l", lwd=2, xaxt = "n", yaxt = "n",
     ylab="",
     xlab="",
     ylim=c(0,2), xlim = c(0, 100))
axis(side = 4, font = 6)

abline(h=seq(from = 0, to = 2, by = 0.1), lty=3, col = "gray")
mtext(expression(paste("HDI-width in  ", italic(SD))), line= 2, family = "serif", cex = .8, side = 4)
title(xlab = expression(paste(italic(n),' = number of participants in each group')),line= 2.2, family = "serif", cex = 2)
abline(h=0.6, lty=1) 
legend("topleft", c("Success rate","HDI-width"),
       col = c("darkgreen", "black"), lty = c(1, 1), lwd = c(2,2), cex = .8, y.intersp = 1,bg = "white", text.font = 6)


## Success rate Large ES
##Clear and change working space      
rm(list=ls(all=TRUE))
load("workspace SeqBFandHDIwithSnowfallLargeEffect.R.RData")

plot(PropTally$nstop, PropTally$propsuccess*100, type="l", col="darkgreen", lwd=2, ylab="",xlab="", ylim = c(0,100), family ="serif")

abline(h=seq(from = 0, to = 100, by = 10), v=seq(from = 0, to = 100, by = 10), col="gray", lty=3)
title(main = paste("(B) Success rate/HDI width, effect size: 0.8"), family = "serif", cex = 2)
title(ylab = "Propotion of successful simulations", line = 2.2, family = "serif")
title(xlab = expression(paste(italic(n),' = number of participants in each group')),line= 2.2, family = "serif")


par(new = TRUE)

###Load workspace for Large effect size
load("workspace HDI width n10-500 Large ES.RData")

plot(M_hdiwd[,1], M_hdiwd[,2], type="l", lwd=2, xaxt = "n", yaxt = "n",
     ylab="",
     xlab="",
     ylim=c(0,2), xlim = c(0, 100))
axis(side = 4, font = 6)

abline(h=seq(from = 0, to = 2, by = 0.1), lty=3, col = "gray")
mtext(expression(paste("HDI-width in  ", italic(SD))), line= 2.2, family = "serif", cex = .8, side = 4)
title(xlab = expression(paste(italic(n),' = number of participants in each group')),line= 2.2, family = "serif", cex = 2)
abline(h=0.6, lty=1) 
legend("topleft", c("Success rate","HDI-width"),
       col = c("darkgreen", "black"), lty = c(1, 1), lwd = c(2,2), cex = .8, y.intersp = 1,bg = "white", text.font = 6)
