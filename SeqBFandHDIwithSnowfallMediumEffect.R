# clears workspace:  
rm(list=ls(all=TRUE))
graphics.off()

#setwd("~")

# Check that required packages are installed:
want = c("rjags","runjags","HDInterval","BEST","extrafont", "BayesFactor", "snowfall")
have = want %in% rownames(installed.packages())
if ( any(!have) ) { install.packages( want[!have] ) }



library(extrafont) #For plotting in "times new roman"  
library(snowfall)
library(pbapply)

####################################################
#
#Creating functions for t-test with BF and HDI estimation
#
####################################################
nCores = parallel::detectCores() #Detecting available cores in the comupter

#Function with t-test using BF and Bayesian estimation with HDI
BF_HDI_seqFun <- function(xpop, ypop, Max_n, Min_n, rscale, bound, SD){
  
  library(BayesFactor)
  library(BEST)
  
  q=0
  success=0
  n_stop=0
  BF = c()
  muDiff = c()
  hdiDiff = c()
 
  
  # Simulate the data - two independant samples: Min_n=10
  x <- sample(xpop, Min_n, replace=FALSE)
  y <- sample(ypop, Min_n, replace=FALSE) 
  
  while ((q+Min_n)<=Max_n){  
    
    #BF t-est
    bft<-ttestBF(x,y,paired=FALSE,rscale = rscale)
    BF=exp(1)^bft@bayesFactor$bf
    BF2=bft@bayesFactor$bf
    
    #HDI estimation
    BESTout = BESTmcmc(x, y, numSavedSteps=10000, thinSteps=1, burnInSteps=2000,verbose=FALSE) 
    muDiff <- BESTout$mu1 - BESTout$mu2
    HDI=HDInterval::hdi(muDiff,credMass=0.95)
    
    hdiDiff=HDI[2]- HDI[1]
    
      #Stopping rules, if BF reach the bound or HDI width is half a SD
      if (BF>=bound | hdiDiff <= (SD/2)) {
      n_stop=length(x)
      success=1
      
      break} # end if
    
    # increment q and add 1 participant to each group
    q=q+1
    x <- c(x,sample(xpop, 1, replace=FALSE))
    y <- c(y,sample(ypop, 1, replace=FALSE)) 
    
  
  } # end while flag
  
  return(c(n_stop, success, BF, hdiDiff))}




####################################################
#
# caculate precision and plot the results
#
###################################################

SD <- 1 #standardize standard deviation = 1
eff_size=0.5 #standardized effect size: Small effect = 0.2, Medium Effect = 0.5, Large Effect = 0.8
Max_n=100 #max number of participants to test
Min_n=15 #min number of participants to test
nSims=500 #number of simulations
bound = 3
rscale = sqrt(2)/2

# first define population
xpop <- rnorm(1000000, eff_size, SD)
ypop <- rnorm(1000000, 0, SD)



# initiate a parallel cluster with available cpus
sfInit(parallel = T, nCores, type="SOCK")


# export the functions and variables to the cluster
sfExport("BF_HDI_seqFun","xpop", "ypop", 'Max_n', 'Min_n', 'rscale', 'bound', 'SD')


# execute function for number of sims (OBS this will take along time depending on the used effect size and number of cores in computer)
t <- sfClusterApplySR(1:nSims, function(i) BF_HDI_seqFun(xpop, ypop, Max_n, Min_n, rscale, bound, SD), perUpdate = NULL)

tally=matrix(unlist(t),nrow = nSims , ncol = 4,byrow=TRUE)
tally[,1]=ifelse(tally[,2]==0,Max_n,tally[,1])
colnames(tally)=c("nstop","success", "BF", "HDI")

# stop the clusters
sfStop()

save.image("~/Magister/Rwork/workspace_BF_HDIFullSim")

     

#############################################
###Creating dataframes of the result ########
#############################################

# define matrix for collecting the results
PropTally=matrix(data=NA, nrow = 10 , ncol = 2)

# could be done a lot better
PropTally[1,1]=10
PropTally[1,2]=sum(tally[tally[,1]<=10,2])/nSims
PropTally[2,1]=20
PropTally[2,2]=sum(tally[tally[,1]<=20,2])/nSims
PropTally[3,1]=30
PropTally[3,2]=sum(tally[tally[,1]<=30,2])/nSims
PropTally[4,1]=40
PropTally[4,2]=sum(tally[tally[,1]<=40,2])/nSims
PropTally[5,1]=50
PropTally[5,2]=sum(tally[tally[,1]<=50,2])/nSims
PropTally[6,1]=60
PropTally[6,2]=sum(tally[tally[,1]<=60,2])/nSims
PropTally[7,1]=70
PropTally[7,2]=sum(tally[tally[,1]<=70,2])/nSims
PropTally[8,1]=80
PropTally[8,2]=sum(tally[tally[,1]<=80,2])/nSims
PropTally[9,1]=90
PropTally[9,2]=sum(tally[tally[,1]<=90,2])/nSims
PropTally[10,1]=100
PropTally[10,2]=sum(tally[tally[,1]<=100,2])/nSims

PropTally=as.data.frame(PropTally)
colnames(PropTally)=c("nstop","propsuccess")

#Making a data frame of tally
tallydf=as.data.frame(tally[order(tally[,1]),])

tally2 <- tally

tally3 <- tally

c <- 0

#Calulate mean of BF and HDI for every nstop
while (c+Min_n <=Max_n){
  tally2[tally2[,1]==(c+Min_n),3]= mean(tallydf[tallydf$nstop==(c+Min_n), "BF"])
  tally2[tally2[,1]==(c+Min_n),4]= mean(tallydf[tallydf$nstop==(c+Min_n), "HDI"])
  
  c <- (c + 1) 
}

#Creating dataframe  
Dftally = as.data.frame(tally2[order(tally2[,1]),])



###If the stopping rules were change to BF <3 or HDI-width <0.8
tally3[,2]=ifelse(tally3[,4]<0.8 | tally3[,3]>3, 1, tally3[,2])



##################
####   PLOTS  ####
##################

##Plot success rate

loadfonts(device="win") 

windows()
par(mar = c(5,5,2,5))
with(PropTally, plot(PropTally$nstop, PropTally$propsuccess, type="l", col="darkgreen", family = "serif",
                     ylab="Propotion of sucessful simulations",
                     xlab=expression(paste(italic(n),' = number of participants in each group')),
                     ylim=c(0,1),log='x'))

title(main = paste("Success rate in detecting effect size of ",eff_size), family = "serif")
 # title(sub = paste( "With stopping rules: BF >", bound, "or HDI-width >=", SD/2)) 


  
#Plot HDI width with tallydf
  
  windows()
  par(mar = c(5,5,2,5))
  with(tallydf, plot(tallydf$nstop, tallydf$HDI, type="l", col="blue", family = "serif", ylim = c(0,2), xlim = c(10,Max_n),
                     ylab="HDI-width",
                     xlab=expression(paste(italic(n),' = number of participants in each group'))))
  title(main = paste("HID-width of simulations with effect size : ",eff_size), family = "serif")        
  
  par(new = TRUE)
  plot(tallydf$BF, type = "l", xaxt = "n", yaxt = "n",
       ylim = c(6,1), ylab = "", xlab = "", col = "red", lty = 2)
  axis(side = 4)
  mtext("BF", side = 4, line = 3, family = "serif")
  legend("topleft", c("HDI-width", "BF"),
         col = c("blue", "red"), lty = c(1, 2),  cex = 1, y.intersp = 1.5,bg = "white", text.font = 6)
  
 
 
  

  
  
  ##Plot success-rate 
  
  loadfonts(device="win") 
  
  windows()
  par(mar = c(5,5,2,5))
  with(PropTally, plot(PropTally$nstop, PropTally$propsuccess, type="l", col="darkgreen", family = "serif", cex= 2, xaxt ="n", yaxt ="n",
                       ylab="Propotion of successful simulations",
                       xlab=expression(paste(italic(n),' = number of participants in each group'))))
                      
  axis(side = 1, at = seq(from= 0,to= 100,by= 10), lwd.ticks = 1, font = 6)
  axis(side = 2, at = seq(from= 0,to= 1,by= .05), lwd.ticks = 1, font = 6)
  title(main = paste("Success rate in detecting effect size of ",eff_size), family = "serif")
  segments(x0 = 0,y0 = 0.85,x1 = 82, y1 = 0.85, lty = 2) # Horizontal dashed line for 85 %
  segments(x0 = 82,y0 = 0.85,x1 = 82, y1 = 0, lty = 2) # Vertical dashed line for 85%
  segments(x0 = 0,y0 = 0.90,x1 = 100, y1 = 0.90, lty = 2, col = "darkgray") # Horizontal dashed line for 90%
  segments(x0 = 100,y0 = 0.90,x1 = 100, y1 = 0, lty = 2, col = "darkgray") # Vertical dashed line for 90 %
 
  

  
  
 #Plot with Dftally, HDI-0.5
  
  windows()
  par(mar = c(5,5,2,5))
  with(Dftally, plot(Dftally$nstop, Dftally$HDI, type="l", col="blue", family = "serif", ylim = c(0.3,2),
                      ylab= expression(paste("HDI-width in   ",italic(SD))),
                      xlab=expression(paste(italic(n),' = number of participants in each group'))))
  title(main = paste("Average HDI-width and BF of ", nSims, " simulations with effect size: ",eff_size), family = "serif")        
 abline(h=0.5)
        
  
  par(new = TRUE)
  plot(Dftally$nstop, Dftally$BF, type = "l", xaxt = "n", yaxt = "n",
        ylim = c(0,40), family = "serif", 
       ylab = "", xlab = "", col = "red", lty = 1)
  axis(side = 4)
  
  mtext("BF", side = 4, line = 3, family = "serif")
  
  legend("topleft", c("HDI-width", "BF", "HDI < 0.5", "Boundary"),
         col = c("blue", "red", "black", "black"), lty = c(1, 1, 1, 2), cex = .9, y.intersp = 1.5,bg = "white", text.font = 6)
  abline(h=3, lty= 2)
 
  
  #Plot with Dftally, HDI-0.8 and line from n=50
  
  windows()
  par(mar = c(5,5,2,5))
  with(Dftally, plot(Dftally$nstop, Dftally$HDI, type="l", col="blue", family = "serif", ylim = c(0.3,1.8),
                     ylab= expression(paste("HDI-width in   ",italic(SD))),
                     xlab=expression(paste(italic(n),' = number of participants in each group'))))
  title(main = paste("Average HDI-width and BF of ", nSims, " simulations with effect size: ",eff_size), family = "serif")        
  abline(h=0.8)
  segments(x0=50, y0=0, x1=50, y1=0.8, lty=5, col = "darkgray")
  
  par(new = TRUE, mar = c(5,5,2,5) )
  plot(Dftally$nstop, Dftally$BF, type = "l",xaxt = "n", yaxt = "n",
      ylim = c(0,40), family = "serif", ylab = "", xlab = "", col = "red", lty = 1)
  axis(side = 4)
  mtext("BF", side = 4, line = 3, family = "serif")
  legend("topleft", c("HDI-width", "BF", "HDI < 0.8", "Boundary"),
         col = c("blue", "red", "black", "black"), lty = c(1, 1, 1, 2),  cex = .9, y.intersp = 1.5,bg = "white", text.font = 6)
  abline(h=3, lty= 2)
  
  
  ##Plot success-rate with HDI
  
  windows()
  par(mar = c(5,5,2,5))
  with(PropTally, plot(PropTally$nstop, PropTally$propsuccess, type="l", col="darkgreen", family = "serif", cex= 2, xaxt ="n", yaxt ="n",
                       ylab="Propotion of successful simulations",
                       xlab=expression(paste(italic(n),' = number of participants in each group'))))
  
  axis(side = 1, at = seq(from= 0,to= 100,by= 10), lwd.ticks = 1, font = 6)
  axis(side = 2, at = seq(from= 0,to= 1,by= .05), lwd.ticks = 1, font = 6)
  title(main = paste("Success rate in detecting effect size of ",eff_size, "with HDI-width"), family = "serif")
  segments(x0 = 0,y0 = 0.85,x1 = 82, y1 = 0.85, lty = 2) # Horizontal dashed line for 85 %
  segments(x0 = 82,y0 = 0.85,x1 = 82, y1 = 0, lty = 2) # Vertical dashed line for 85%
  
  par(new = TRUE, mar = c(5,5,2,5) )
  plot(Dftally$nstop, Dftally$HDI, type = "l",xaxt = "n", yaxt = "n",
       ylim = c(0.5,2), family = "serif", ylab = "", xlab = "", col = "blue", lty = 1)
  axis(side = 4)
  mtext("HDI-width", side = 4, line = 3, family = "serif")
  legend("topleft", c("Success rate", "HDI-width", "HDI < 0.6"),
         col = c("darkgreen", "blue", "black"), lty = c(1, 1, 1),  cex = .9, y.intersp = 1.5,bg = "white", text.font = 6)
  abline(h=0.6)

  
