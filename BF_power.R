
rm(list=ls(all=TRUE)) # Warning clears the workspace  
graphics.off() # closes open graphics devices

#setwd("~")

# Check that required packages are installed:
want = c("BayesFactor","snowfall","parallel")
have = want %in% rownames(installed.packages())
if ( any(!have) ) { install.packages( want[!have] ) }

# load snowfall
library(snowfall)

# detect number of cpu cores
nCores = parallel::detectCores() 
aCores=nCores-1 # save 1 core for other processes.


BF_simFun <- function(xpop, ypop, n, rscaleBF){
  
  library(BayesFactor)
  
  # Simulate the data - two independant samples
  x <- sample(xpop, n, replace = FALSE)
  y <- sample(ypop, n, replace = FALSE)
  
  return(exp(1)^ttestBF(x,y,paired=FALSE,rscale= rscaleBF)@bayesFactor$bf)
  
}




####################################################
#
# caculate power
#
###################################################

BFthreshold <- 3 #BF threshold to test
rscaleBF <- sqrt(2)/2 # BF power - rscale is medium
nSim <- 1000 #number of simulations for each sample size
SD <- 1 # standard deviation 
eff_size=0.5 # effect size (Cohen's D: Small effect = 0.2, Medium Effect = 0.5, Large Effect = 0.8
Max_n=150 #max number of participants to test
Min_n=10 #min number of participants to test
nSubj <- seq(from = Min_n, to = Max_n, by = 2) #min to max sample sizes to test

# define population by draawing random samples from normal distribution
xpop <- rnorm(1000000, eff_size, SD)
ypop <- rnorm(1000000, 0, SD)

# define matrices for collecting the results
BFPow=matrix(data=NA, nrow = length(nSubj), ncol = 2)
BFPow=matrix(data=NA, nrow = length(nSubj), ncol = 2)


# initiate a parallel cluster with available cpus
sfInit(parallel = T, aCores, type="SOCK")

sfExport("BF_simFun","xpop", "ypop", "rscaleBF")

# create progress bar because it will take a while
pb <- txtProgressBar(min = 0, max = 100, initial = 0, style = 3)


# execute the code 
for (p in 1:length(nSubj)) { # for each sample size
      
      n = nSubj[p]
      sfExport("n")
   
      bf <- sfClusterApplyLB(1:nSim, function(i) BF_simFun(xpop, ypop, n, rscaleBF))
      
      BFPow[p,1]=n
      BFPow[p,2]=sum(bf > (BFthreshold))/nSim
  
      setTxtProgressBar(pb, p/length(nSubj)*100)
      
     } 
      
# stop the clusters
sfStop()

#close progress bar
close(pb)

# create data frame for plotting

BFPow=as.data.frame(BFPow)
colnames(BFPow) <- c("nSubj","Pow")

# plot the results, effect size power

windows(20,20) # open graphics device

# BF power
plot(BFPow$nSubj, BFPow$Pow*100, type="l", lwd=3, 
     ylab="% Power",
     xlab=expression(paste(italic(n)," = number of participants in each group")),
     ylim=c(0,100),main=expression(paste(italic(BF),"s, default \"medium\" prior")))

abline(h=seq(from = 0, to = 100, by = 10), v=seq(from = Min_n, to = Max_n, by = 10), col="gray", lty=3)


###################################
##PLOT

windows(20,20) # open graphics device

# BF power
plot(BFPow$nSubj, BFPow$Pow*100, type="l", lwd=3, 
     ylab="",
     xlab="",
     ylim=c(0,100))

abline(h=seq(from = 0, to = 100, by = 10), v=seq(from = Min_n, to = Max_n, by = 10), col="gray", lty=3)
title(main = paste("BFs, default \"medium\" prior, effect size: 0.5"), line = 0.7, family = "serif")        
title(ylab = expression(paste("% Power")), line= 2, family = "serif", cex = 2)
title(xlab = expression(paste(italic(n),' = number of participants in each group')),line= 2.2, family = "serif", cex = 2)

  
