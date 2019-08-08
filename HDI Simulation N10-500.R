# clears workspace:  
rm(list=ls(all=TRUE))
graphics.off()

#setwd("~")

# Check that required packages are installed:
want = c("rjags","runjags","HDInterval","BEST","extrafont")
have = want %in% rownames(installed.packages())
if ( any(!have) ) { install.packages( want[!have] ) }

# load the packages

library(rjags)
library(BEST)
library(extrafont) #For plotting in "times new roman"  


####################################################
#
# caculate precision and plot the results
#
###################################################

Ngroups=2 #number of independant groups
SD <- 1 #standardize standard deviation = 1
eff_size=0.8 #standardized effect size: Small effect = 0.2, Medium Effect = 0.5, Large Effect = 0.8
Max_n=500 #max number of participants to test
Min_n=10 #min number of participants to test
nSubj <- seq(from = Min_n, to = Max_n, by = 2) #min to max sample sizes to test



# define population by draawing random samples from normal distribution
xpop <- rnorm(1000000, eff_size, SD)
ypop <- rnorm(1000000, 0, SD)


M_hdiwd = matrix(data=NA, nrow = length(nSubj), ncol = 2)
hdiDiff = matrix(data=NA, nrow = length(nSubj), ncol = 2)


# create progress bar because it will take a while
pb <- winProgressBar(title = "progress bar", label="0% done", min = 0, max = 100, initial=0)


# execute the code 
for (p in 1:length(nSubj)) { # for each sample size
  
  setWinProgressBar(pb, round(p/(length(nSubj)*2)*100, 0), label=paste(round(p/(length(nSubj)*2)*100, 0), "% done"))
  
  # number of subj to test 
  n = nSubj[p]
  
  # Simulate the data - two independant samples
  y1 <- sample(xpop, n, replace = FALSE)
  y2 <- sample(ypop, n, replace = FALSE)
  
  
  
  BESTout = BESTmcmc(y1, y2, numSavedSteps=10000, thinSteps=1, burnInSteps=2000,verbose=FALSE) 
  muDiff <- BESTout$mu1 - BESTout$mu2
  hdiDiff[p,]=HDInterval::hdi(muDiff,credMass=0.95)
  
  M_hdiwd[p,1]=n
  setWinProgressBar(pb, round((p+length(nSubj))/(length(nSubj)*2)*100, 0), label=paste(round((p+length(nSubj))/(length(nSubj)*2)*100, 0), "% done"))
  

  
  
}

M_hdiwd[,2]=hdiDiff[,2]- hdiDiff[,1]



#close progress bar
close(pb)

# PLot
plot(M_hdiwd[,1], M_hdiwd[,2], type="l", lwd=2, 
     ylab="",
     xlab="",
     ylim=c(0,2))

abline(h=seq(from = 0, to = 2, by = 0.1), v=seq(from = Min_n, to = Max_n, by = 10), col="gray", lty=3)
title(main = expression(paste("Different ",italic(n), ' with effect size: ', eff_size)), line = 0.7, family = "serif")     
title(ylab = expression(paste("HDI-width in  ", italic(SD))), line= 2, family = "serif", cex = 2)
title(xlab = expression(paste(italic(n),' = number of participants in each group')),line= 2.2, family = "serif", cex = 2)


