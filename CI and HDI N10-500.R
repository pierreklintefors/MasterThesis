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
eff_size=0.5 #standardized effect size: Small effect = 0.2, Medium Effect = 0.5, Large Effect = 0.8
boundary = 3 #Boundary for BF
Max_n=500 #max number of participants to test
Min_n=10 #min number of participants to test
nSubj <- seq(from = Min_n, to = Max_n, by = 2) #min to max sample sizes to test



# define population by draawing random samples from normal distribution
xpop <- rnorm(1000000, eff_size, SD)
ypop <- rnorm(1000000, 0, SD)


# define matrices for collecting the results
TPrec = matrix(data=NA, nrow = length(nSubj), ncol = 2)
ConfInt = matrix(data=NA, nrow = length(nSubj), ncol = 2)
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


   Conf = t.test(y1,y2,paired=FALSE,var.equal=TRUE)$conf.int
   ConfInt[p,] = unlist(Conf)
   
   BESTout = BESTmcmc(y1, y2, numSavedSteps=10000, thinSteps=1, burnInSteps=2000,verbose=FALSE) 
   muDiff <- BESTout$mu1 - BESTout$mu2
   hdiDiff[p,]=HDInterval::hdi(muDiff,credMass=0.95)
   
   TPrec[p,1]=n
   M_hdiwd[p,1]=n
   setWinProgressBar(pb, round((p+length(nSubj))/(length(nSubj)*2)*100, 0), label=paste(round((p+length(nSubj))/(length(nSubj)*2)*100, 0), "% done"))
   
   bft<-ttestBF(y1,y2,paired=FALSE,rscale = "medium")
   BF=bft@bayesFactor$bf
   BF_N[p,] = unlist(BF)
   
   
   
   
   BF_N[p,1]=n
   BF_N[p,2]=BF
   

}

TPrec[,2]=ConfInt[,2]-ConfInt[,1]
M_hdiwd[,2]=hdiDiff[,2]- hdiDiff[,1]

#Vector were BF over boundary
SucessBF = matrix(data = NaN,nrow = BF_N[,2]<=boundary, ncol = 1 )
SucessBF=BF_N[,2]<=boundary

#close progress bar
close(pb)

df <- data.frame(TPrec,M_hdiwd[ , 2]) #Creating a dataframe for plotting
names(df)[1]<-paste("n")
names(df)[2]<-paste("CI")
names(df)[3]<-paste("HDI")

###############################
##Plotting
###############################

loadfonts(device="win") 

#Plot both HDI and CI
windows()
par(mar = c(5,5,2,5))
plot(TPrec[,1], TPrec[,2], type="l", col="blue", family = "serif", 
             ylab=expression(paste('Precision  ',italic(SD))), family = "serif",
             xlab=expression(paste(italic(n),' = number of participants in each group', family = "serif")),
             ylim=c(0,2))
             lines (M_hdiwd[,1],M_hdiwd[,2],col="red")
             title(main = "Width of HDI's and CI's for different sample sizes", family = "serif")

              text(x=11, y=0.2, labels = "HDI", cex = 1.3, col = "red", family = "serif")
              text(x=10, y=0.1, labels = "CI", cex = 1.3, col ="blue", family = "serif")

            
              


###################
## Graph with ggplot
###################
              
#Both HDI and CI
windows()
p <- ggplot(df, aes(n)) + 
  geom_line(aes(y = CI, colour = "CI")) + 
  geom_line(aes(y = HDI, colour = "HDI")) +
  xlab (expression(paste(italic(n),' = number of participants in each group'))) + 
  ylab(expression(paste('Precision  ',italic(SD)))) +
  ylim (0.1,2) +
  theme_bw() +
  theme(text=element_text(family="serif", size=12)) 
  
  # Add titles and adjust te position
  p + labs(title = "Width of HDI's and CI's for different sample sizes") +
          
  theme(plot.title = element_text(hjust = 0.5, size = 14 )) +  # Center title position and size
          
     
  theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  
  
##Plottning HDI-width with reference lines
  windows()
  par(mar = c(5,5,2,5))
  plot(M_hdiwd[,1], M_hdiwd[,2], type="l", col="blue", family = "serif", 
                      ylab=expression(paste('Precision  ',italic(SD))), family = "serif",
                      xlab=expression(paste(italic(n),' = number of participants in each group'), family = "serif"),
                      ylim=c(0.2,2),
                      xlim = c(20,500), xaxt = "n")
  
  axis(1, at = seq(0,500,10), lwd.ticks = 1, line = 0)
  title(main = paste("HID-width of different sample sizes with effect size: ",eff_size), family = "serif")        
  #abline(h=0.55, v=100, lty= 2) 
         segments(x0 = 114,y0 = 0.50,x1 = 114, y1 = 0, lty = 5)
         abline(h=0.5)
         
  
  
#Plot BF other axis
  par(mar = c(5,5,2,5))
  plot(BF_N[,2], type = "l", xaxt = "n", yaxt = "n",
       ylim = c(0,10), ylab = "", xlab = "", col = "red", lty = 1, family = "serif")
  axis(side = 4)
  mtext("BF", side = 4, line = 3, family = "serif")
  legend("topleft", c("HDI-width", "BF"),
         col = c("blue", "red"), lty = c(1, 1))
  








