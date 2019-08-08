rm(list=ls(all=TRUE))
setwd("~/Magister/Rwork")


###Load workspace
load("workspace BF_power n10-150 Small ES.RData")

library(extrafont)

##Plot success-rate
loadfonts(device="win") 

windows(width = 10,height = 4)
par(mfrow = c(1,3), mar = c(5,3,2,2))

# BF power
plot(BFPow$nSubj, BFPow$Pow*100, type="l", lwd=3, 
     ylab="",
     xlab="",
     ylim=c(0,100))

abline(h=seq(from = 0, to = 100, by = 10), v=seq(from = Min_n, to = Max_n, by = 10), col="gray", lty=3)
title(main = paste("(A) BFs, default prior, effect size: 0.2"), line = 0.7, family = "serif")        
title(ylab = expression(paste("% Power")), line= 2, family = "serif", cex = 2)
title(xlab = expression(paste(italic(n),' = number of participants in each group')),line= 2.2, family = "serif", cex = 2)



rm(list=ls(all=TRUE))


###Load workspace
load("workspace BF_power n10-150 Medium ES.RData")

# BF power
plot(BFPow$nSubj, BFPow$Pow*100, type="l", lwd=3, 
     ylab="",
     xlab="",
     ylim=c(0,100))

abline(h=seq(from = 0, to = 100, by = 10), v=seq(from = Min_n, to = Max_n, by = 10), col="gray", lty=3)
title(main = paste("(B) BFs, default prior, effect size: 0.5"), line = 0.7, family = "serif")        
title(ylab = expression(paste("% Power")), line= 2, family = "serif", cex = 2)
title(xlab = expression(paste(italic(n),' = number of participants in each group')),line= 2.2, family = "serif", cex = 2)


rm(list=ls(all=TRUE))


###Load workspace
load("workspace BF_power n10-150 Large ES.RData")

# BF power
plot(BFPow$nSubj, BFPow$Pow*100, type="l", lwd=3, 
     ylab="",
     xlab="",
     ylim=c(0,100))

abline(h=seq(from = 0, to = 100, by = 10), v=seq(from = Min_n, to = Max_n, by = 10), col="gray", lty=3)
title(main = paste("(C) BFs, default prior, effect size: 0.8"), line = 0.7, family = "serif")        
title(ylab = expression(paste("% Power")), line= 2, family = "serif", cex = 2)
title(xlab = expression(paste(italic(n),' = number of participants in each group')),line= 2.2, family = "serif", cex = 2)

