rm(list=ls(all=TRUE))
setwd("~/Magister/Rwork")


###Load workspace
load("workspace SeqBFandHDIwithSnowfallSmallEffect.R.RData")

##Plot success-rate
loadfonts(device="win") 

windows(width = 10,height = 3.5)
par(mfrow = c(1,3), mar = c(5,3.1,2,1.6))

## Success rate Small ES
plot(PropTally$nstop, PropTally$propsuccess*100, type="l", col="darkgreen", lwd=3, ylab="",xlab="",
     xlim = c(Min_n, Max_n), ylim = c(0,100))

  abline(h=seq(from = 0, to = 100, by = 10), v=seq(from = 10, to = 100, by = 10), col="gray", lty=3)
  title(main = paste("(A) Success rate in detecting effect size: 0.2"), family = "serif", cex = 4)
  title(ylab = "Propotion of successful simulations", line = 2.2, family = "serif")
  title(xlab = expression(paste(italic(n),' = number of participants in each group')),line= 2.2, family = "serif")
  
  
  
## Success rate Medium ES
#Clear and change working space
rm(list=ls(all=TRUE))
load("workspace SeqBFandHDIwithSnowfallMediumEffect.RData")

plot(PropTally$nstop, PropTally$propsuccess*100, type="l", col="darkgreen", lwd=3, ylab="",xlab="", ylim = c(0,100))

abline(h=seq(from = 0, to = 100, by = 10), v=seq(from = 10, to = 100, by = 10), col="gray", lty=3)
title(main = paste("(B) Success rate in detecting effect size: 0.5"), family = "serif", cex = 4)
title(ylab = "Propotion of successful simulations", line = 2.2, family = "serif")
title(xlab = expression(paste(italic(n),' = number of participants in each group')),line= 2.2, family = "serif")


## Success rate Large ES
##Clear and change working space      
rm(list=ls(all=TRUE))
load("workspace SeqBFandHDIwithSnowfallLargeEffect.R.RData")

plot(PropTally$nstop, PropTally$propsuccess*100, type="l", col="darkgreen", lwd=3, ylab="",xlab="", ylim = c(0,100))

abline(h=seq(from = 0, to = 100, by = 10), v=seq(from = 10, to = 100, by = 10), col="gray", lty=3)
title(main = paste("(C) Success rate in detecting effect size: 0.8"), family = "serif", cex = 4)
title(ylab = "Propotion of successful simulations", line = 2.2, family = "serif")
title(xlab = expression(paste(italic(n),' = number of participants in each group')),line= 2.2, family = "serif")

