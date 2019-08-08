# clears workspace:  
rm(list=ls(all=TRUE))
graphics.off()

# load the packages
library(snowfall)
library(extrafont) #For plotting in "times new roman"  

# detect number of cores
nCores = parallel::detectCores() 

T_seqFun <- function(xpop, ypop, Max_n, Min_n){
  
  q=0
  sig=0
  n_stop=0
  
  # Simulate the data - two independant samples: Min_n=10
  x <- sample(xpop, Min_n, replace=FALSE)
  y <- sample(ypop, Min_n, replace=FALSE) 
  
  while ((q+Min_n)<=Max_n){  
  
  if (t.test(x,y,paired=FALSE,var.equal=TRUE)$p.value<.05) {
      n_stop=length(x)
      sig=1
      break} # end if
  
  # increment q and add 1 participant to each group
  q=q+1
  x <- c(x,sample(xpop, 1, replace=FALSE))
  y <- c(y,sample(ypop, 1, replace=FALSE)) 
  
  } # end while flag
  
  return(c(n_stop,sig))}
  

####################################################
#
# caculate false alarms and plot the results
#
###################################################

SD <- 1 #standardize standard deviation = 1
eff_size=0 #standardized effect size: Small effect = 0.2, Medium Effect = 0.5, Large Effect = 0.8
Max_n=2500 #max number of participants to test
Min_n=2 #min number of participants to test
nSims=1000 #number of simulations

# first define population
xpop <- rnorm(1000000, eff_size, SD)
ypop <- rnorm(1000000, 0, SD)



# initiate a parallel cluster with available cpus
sfInit(parallel = T, nCores, type="SOCK")


  # export the functions and variables to the cluster
  sfExport("T_seqFun","xpop", "ypop", 'Max_n', 'Min_n')
  
  # execute function for number if sims
  t <- sfClusterApplySR(1:nSims, function(i) T_seqFun(xpop, ypop, Max_n, Min_n), perUpdate = 1)
  
  tally=matrix(unlist(t),nrow = nSims , ncol = 2,byrow=TRUE)
  tally[,1]=ifelse(tally[,2]==0,Max_n,tally[,1])
  colnames(tally)=c("nstop","sig")
  
# stop the clusters
sfStop()



# plot the results

# define matrix for collecting the results
PropTally=matrix(data=NA, nrow = 21 , ncol = 2)

# could be done a lot better
PropTally[1,1]=0
PropTally[1,2]=sum(tally[tally[,1]<=10,2])/nSims
PropTally[2,1]=50
PropTally[2,2]=sum(tally[tally[,1]<=50,2])/nSims
PropTally[3,1]=100
PropTally[3,2]=sum(tally[tally[,1]<=100,2])/nSims
PropTally[4,1]=150
PropTally[4,2]=sum(tally[tally[,1]<=150,2])/nSims
PropTally[5,1]=200
PropTally[5,2]=sum(tally[tally[,1]<=200,2])/nSims
PropTally[6,1]=250
PropTally[6,2]=sum(tally[tally[,1]<=250,2])/nSims
PropTally[7,1]=300
PropTally[7,2]=sum(tally[tally[,1]<=300,2])/nSims
PropTally[8,1]=350
PropTally[8,2]=sum(tally[tally[,1]<=350,2])/nSims
PropTally[9,1]=400
PropTally[9,2]=sum(tally[tally[,1]<=450,2])/nSims
PropTally[10,1]=450
PropTally[10,2]=sum(tally[tally[,1]<=500,2])/nSims
PropTally[11,1]=500
PropTally[11,2]=sum(tally[tally[,1]<=500,2])/nSims
PropTally[12,1]=550
PropTally[12,2]=sum(tally[tally[,1]<=550,2])/nSims
PropTally[13,1]=600
PropTally[13,2]=sum(tally[tally[,1]<=600,2])/nSims
PropTally[14,1]=650
PropTally[14,2]=sum(tally[tally[,1]<=650,2])/nSims
PropTally[15,1]=700
PropTally[15,2]=sum(tally[tally[,1]<=700,2])/nSims
PropTally[16,1]=800
PropTally[16,2]=sum(tally[tally[,1]<=800,2])/nSims
PropTally[17,1]=900
PropTally[17,2]=sum(tally[tally[,1]<=900,2])/nSims
PropTally[18,1]=1000
PropTally[18,2]=sum(tally[tally[,1]<=1000,2])/nSims
PropTally[19,1]=1500
PropTally[19,2]=sum(tally[tally[,1]<=1500,2])/nSims
PropTally[20,1]=2000
PropTally[20,2]=sum(tally[tally[,1]<=2000,2])/nSims
PropTally[21,1]=2500
PropTally[21,2]=sum(tally[tally[,1]<=2500,2])/nSims

PropTally=as.data.frame(PropTally)







colnames(PropTally)=c("nstop","propsig")



####Plotting

loadfonts(device="win") 


windows()
par(mar = c(5,5,2,5))
with(PropTally, plot(PropTally$nstop, PropTally$propsig, type="l", col="red3", family = "serif",
                  ylab="False Alarm rate ",
                  xlab=expression(paste(italic(n),' = number of participants in each group')),
                  xlim = c(10,2500),
                  ylim=c(0,1))) #,log='x'))
                  title(main = "False Alarm Rate with repeated testing in NHST", family = "serif")



#GGplot
windows()
p <- ggplot(PropTally, aes(nstop)) + 
  geom_line(aes(y = propsig, breaks = 100, colour ='False Alarms'))  +
  xlab (expression(paste(italic(n),' = number of participants in each group'))) + 
  ylab(expression(paste('False Alarm Rate' ))) +
  theme_bw() +
  theme(text=element_text(family="serif", size=12)) 

# Add titles and adjust te position
p + labs(title = "False Alarm Rate with repeated testing in NHST") +
  
  theme(plot.title = element_text(hjust = 0.5, size = 14 )) +  # Center title position and size
  
  
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


#Plot with grid
plot(PropTally$nstop, PropTally$propsig*100, type="l", lwd=2, col="red3",
     ylab="",
     xlab="",
     ylim=c(0,100))

abline(h=seq(from = 0, to = 100, by = 10), v=seq(from =0, to = 2500, by = 100), col="gray", lty=3)
title(main = expression(paste("False Alarm Rate with repeated testing in NHST")), line = 0.7, family = "serif")     
title(ylab = expression(paste("False Alarm Rate")), line= 2, family = "serif", cex = 2)
title(xlab = expression(paste(italic(n),' = number of participants in each group')),line= 2.2, family = "serif", cex = 2)



 