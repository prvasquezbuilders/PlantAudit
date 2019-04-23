#################################################################################



#     THIS PROGRAM IS PROPERTY OF NAM LE. KINDLY CONTACT NAM LE AT NAMLT@PROTONMAIL.CH FOR CONSULTANCY SERVICES AND TRAINING


#     Date: 20/02/2014

#     Date: 05/03/2019 Reused with modified code for the PLANT AUDIT PROJECT of Maynilad Water Services Inc.


#     Original: refer to paper entitled "a Block Replacement Model for Determination of Optimal Intervention Strategies"

#################################################################################




#-----------STEP 1--------------Loading all dependent packages


library(WeibullR) #load the Weibull package

#colors=c("red","violet","chartreuse3","chocolate1","darkgoldenrod1","darkmagenta","darkslategray","darksalmon","red","deeppink","deeppink4")

library(RColorBrewer)
library(randomcoloR)


#-----------STEP 2--------------Define functions of Weibull


#basic function

#--survival function
survi=function(t,eta,beta){exp(-(1/eta*t)^beta)} #this is survival function

#--probability density function

f=function(t,eta,beta){beta/eta*(t/eta)^(beta-1)*exp(-(t/eta)^beta)} #probability of failure

#--First part of the cost function
f_cost=function(t,eta,beta,rho){
  ((beta/eta)*(t/eta)^(beta-1)*exp(-(t/eta)^beta))*exp(-rho*t)
}
#




#-----------STEP 3--------------Define numbers of objects and sizes of corresponding objects in the program.

T=10000
Maxobj=6 #number of assets/components to be consider
breakpoint=20
c<-matrix(nrow = Maxobj) #ratio between PI/CI for each asset
eta<-matrix(nrow = Maxobj) #scale parameter
beta<-matrix(nrow = Maxobj) #shape parameter
F_cost<-matrix(nrow=T,ncol = Maxobj) #commulative distribution
FF<-matrix(nrow=T,ncol = Maxobj) #commulative distribution
#FFF<-matrix(nrow=T) #intergal part of the equation
S<-matrix(nrow=T,ncol = Maxobj) #survival probability
m<-matrix(nrow=T,ncol = Maxobj) #mean number of failure
E<-matrix(nrow=T,ncol = Maxobj) #expect meantime to failure, which equivalent to m(T)
omega<-matrix(nrow=T,ncol = Maxobj) #this is the impact per unit of time in BR model

optimal_time<-matrix(nrow=Maxobj)
min_impact<-matrix(nrow=Maxobj)



#-----------STEP 4--------------Define values of parameters



rho=0.085/(365*24) #discount factor
#random generating value for eta, beta, and ratio between preventive intervention and corrective intervention
eta<-runif(Maxobj, 6000, 8000) 
beta<-runif(Maxobj, 2, 3)
c<-runif(Maxobj, 0.1, 0.5)


#write eta and beta to excel
weibullpara=data.frame(eta,beta)
print(weibullpara)
write.csv(weibullpara, file = "weibullpara.csv")

#drawing the plots for Weibull functions


colors <- distinctColorPalette(Maxobj)


#------------------------------------------------------------------------

#Estimation of Eta and Beta based on observed data on component levels. Data was prepared by Dario of Maynilad.


source("weibullpara-motorbearing.R")

# -----------------------------------------------------------------------------



#-----------STEP 5--------------Simulate the Weibull estimation for eta and beta based on simulatation.
m<-5
etatest<-matrix(nrow=Maxobj)
betatest<-matrix(nrow=Maxobj)
for (i in 1: Maxobj){
failures<-rweibull(m, beta[i], eta[i])
#without suspension
median_percentile_ranks<-getPPP(failures)
print(median_percentile_ranks)#[,2]
MLEfit<-mlefit(mleframe(failures))
MLE_Unbiased<-c(MLEfit[1],MLEfit[2]*hrbu(length(failures)))

etatest[i]<-MLE_Unbiased[[1]]
betatest[i]<-MLE_Unbiased[[2]]

#cat("For object ", i /n)
print(MLE_Unbiased)

da1 <- data.frame(
  serial=c("1","2","3","4","5"),
  time=c(failures),
  event=c(1,1,1,1,1))

da1=wblr(da1, col="red")
da1 <- wblr.fit(da1, col="darkgreen")
#da1 <- wblr.conf(da1, col="blue")
plot(da1, main=paste("pump", i))


}









#-----------STEP 6--------------Plotting the reliability curves


plot.new()

for (i in 1:Maxobj){
  
#  png(filename=paste("E:/OP18REFCS03/Stations/VIL/Report/figures/ch05_fig_sur_pump",i,".png",sep=""),width = 800, height = 500) 
  
  #  par(mar=c(5, 4, 4, 6) + 0.1)
  plot(survi(c(0:T),eta[i],beta[i]),pch=14,axes=FALSE,ylim=c(0,1),ylab="", xlab="",col=colors[i],type="l",lwd=2,cex=1, main=paste("pump", i))
  #  par(new=TRUE)
  axis(2, ylim=c(0,1),col="darkblue",las=1)  ## las=1 makes horizontal labels
  axis(1,pretty(range(c(0:T)),breakpoint))
  mtext(expression(paste("Time (hours)")),side=1,col="black",line=2.5)  
  mtext(expression(paste("Reliability")),side=2,col="black",line=2.5) 
  box()
  legend("topright",inset=.05,legend=c(paste("pump ", i)), text.col="black",lty=c(1),col=colors[i], horiz=F,cex=0.8,bg = "gray90")
  
  
  #library(ggplot2)
  #library(grid)
  grid(26, 11, col = "lightgray", lty = "dotted",lwd = par("lwd"), equilogs = TRUE)
 # dev.off()
}

#stop()





#-----------STEP 7--------------Routines to estimate life cycle cost and determination of optimal intervention strategies


#This is the life cycle cost analysis section
plot.new()
for (i in 1:Maxobj){
  plot.new()
  
#  png(filename=paste("E:/OP18REFCS03/Stations/VIL/Report/figures/ch05_fig_ois_pump",i,".png",sep=""),width = 800, height = 500)
  
  for (t in 1: T){
    F_cost[t,i]=integrate(f_cost,0,t,eta[i],beta[i],rho)$val
    FF[t,i]<-1-exp(-(1/eta[i]*t)^beta[i])
    S[t,i]=exp(-(1/eta[i]*t)^beta[i])
    E[t,i]=integrate(survi,0,t,eta[i],beta[i])$val
    
    # m[t]=integrate(f,0,t,eta,beta)$val
    omega[t,i]=(F_cost[t,i]+S[t,i]*c[i]*exp(-rho*t))/E[t,i]
    # omega[t]=(FF[t]+S[t]*c)*exp(-rho*t)/E[t]
  }
  
  optimal_time[i]<-which.min(omega[,i])
  min_impact[i]<-min(omega[,i])
  
  unit=min(omega[,i])*3  
  
  #--------------------PLOT--------------------
  #plot.new()
  par(mar=c(5, 4, 4, 6) + 0.6)
  plot(omega[,i],type="l",lwd=2,col="red",ylab="",xlab="",xlim=c(0,T),ylim=c(0,unit),axes=FALSE,lty=1, main=paste("pump", i)) #plot the cost per unit of time curve
  
  axis(2, ylim=c(0,unit),col="black",las=1)  ## las=1 makes horizontal labels
  axis(1,c(seq(0,T, by=500)))
  
  mtext(expression(paste("Time (hours)")),side=1,col="black",line=2.2) 
  mtext(expression(paste(Omega[,i], '(mu)')),side=2,col="black",line=3.5) 
  points(which.min(omega[,i]),min(omega[,i]),pch=23,bg="black",lwd=3)
  segments(which.min(omega[,i]),min(omega[,i]),which.min(omega[,i]),0, col= 'darkviolet',lty=1,lwd=1)
  segments(0,min(omega[,i]),which.min(omega[,i]),min(omega[,i]), col= 'darkviolet',lty=1,lwd=1)
  text(which.min(omega[,i])*1,min(omega[,i])*0.9,paste("(", format(which.min(omega[,i]), 2),",", format(round(min(omega[,i]), 5), nsmall = 2),")"),pos=4,cex = 1, srt = 0)
  
  grid(10, 10, col = "lightgray", lty = "dotted",lwd = par("lwd"), equilogs = TRUE)
  box()
  #END
  
  
 #   dev.off()
  
}

#Print out the results in a data frame objects

ois<-data.frame(eta,beta,c,optimal_time,min_impact)
print(ois)
write.csv(ois, file = "ois.csv")



