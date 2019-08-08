rm(list=ls(all=TRUE))
setwd("~/Magister/Rwork")


###Load workspace for small effect size
load("workspace HDI width n10-500 Small ES.RData")

library(extrafont)

##Plot
loadfonts(device="win") 

windows(width = 10,height = 4)
par(mfrow = c(1,3), mar = c(5,3,2,2))

# PLot
plot(M_hdiwd[,1], M_hdiwd[,2], type="l", lwd=1, 
     ylab="",
     xlab="",
     ylim=c(0,2))

abline(h=seq(from = 0, to = 2, by = 0.1), v=seq(from = 0, to = 500, by = 50), col="gray", lty=3)
title(main = expression(paste("(A) HDI- width for range of ",italic(n), ' with effect size: 0.2')), line = 0.7, family = "serif", cex = 4)     
title(ylab = expression(paste("HDI-width in  ", italic(SD))), line= 2, family = "serif", cex = 2)
title(xlab = expression(paste(italic(n),' = number of participants in each group')),line= 2.2, family = "serif", cex = 2)
abline(h=0.5, lty=1) 




rm(list=ls(all=TRUE))


###Load workspace for medium effect size
load("Workspace CI HDI Plotting.RData")

# PLot
plot(M_hdiwd[,1], M_hdiwd[,2], type="l", lwd=1, 
     ylab="",
     xlab="",
     ylim=c(0,2))

abline(h=seq(from = 0, to = 2, by = 0.1), v=seq(from =0, to = 500, by = 50), col="gray", lty=3)
title(main = expression(paste("(B) HDI- width for range of ",italic(n), ' with effect size: 0.5')), line = 0.7, family = "serif")     
title(ylab = expression(paste("HDI-width in  ", italic(SD))), line= 2, family = "serif", cex = 2)
title(xlab = expression(paste(italic(n),' = number of participants in each group')),line= 2.2, family = "serif", cex = 2)
abline(h=0.5, lty=1) 



rm(list=ls(all=TRUE))


###Load workspace
load("workspace HDI width n10-500 Large ES.RData")


# PLot
plot(M_hdiwd[,1], M_hdiwd[,2], type="l", lwd=1, 
     ylab="",
     xlab="",
     ylim=c(0,2))

abline(h=seq(from = 0, to = 2, by = 0.1), v=seq(from = 0, to = 500, by = 50), col="gray", lty=3)
title(main = expression(paste("(C) HDI- width for range of ",italic(n), ' with effect size: 0.8')), line = 0.7, family = "serif")     
title(ylab = expression(paste("HDI-width in  ", italic(SD))), line= 2, family = "serif", cex = 2)
title(xlab = expression(paste(italic(n),' = number of participants in each group')),line= 2.2, family = "serif", cex = 2)
abline(h=0.5, lty=1) 

