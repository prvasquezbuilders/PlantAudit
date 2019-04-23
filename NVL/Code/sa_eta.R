#################################################################################



#     THIS PROGRAM IS PROPERTY OF NAM LE. KINDLY CONTACT NAM LE AT NAMLT@PROTONMAIL.CH FOR CONSULTANCY SERVICES AND TRAINING


#     Date: 20/02/2014

#     Reused with modified code for the PLANT AUDIT PROJECT of Maynilad Water Services Inc.


#     Original: refer to paper entitled "a Block Replacement Model for Determination of Optimal Intervention Strategies"

#################################################################################



data <- read.csv(file="ois.csv", header=TRUE, sep=",")
data=data.frame(data)


rho=0.085/(365*24) #discount factor
#random generating value for eta, beta, and ratio between preventive intervention and corrective intervention

eta<-10000
step=seq(2000,eta, by =200)

T=10000
factor=1

#plottime=20
#plotimpact=20









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


Maxobj=6 #number of assets/components to be consider

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



etasa<- matrix(double(1),nrow=length(step),ncol=1)

OTW<-matrix(nrow = Maxobj,ncol = length(step))
Cost<-matrix(nrow = Maxobj,ncol = length(step))

#This is the life cycle cost analysis section
plot.new()
for (i in 1:Maxobj){
#  plot.new()
  
#  png(filename=paste("E:/OP18REFCS03/Stations/VIL/Code/figures/ch05_fig_etasa_pump",i,".png",sep=""),width = 900, height = 600) 
  
  beta[i]<-data$beta[i]
  c[i]<-data$c[i]
  for (u in 1:length(step)){
    etasa[u]<-step[u]
    
 #   cat(paste(" Optimal IS for pump #",i,  "\n"))
    
      for (t in 1: T){
      F_cost[t,i]=integrate(f_cost,0,t,etasa[u],beta[i],rho)$val
      FF[t,i]<-1-exp(-(1/etasa[u]*t)^beta[i])
      S[t,i]=exp(-(1/etasa[u]*t)^beta[i])
      E[t,i]=integrate(survi,0,t,etasa[u],beta[i])$val


      omega[t,i]=(F_cost[t,i]+S[t,i]*c[i]*exp(-rho*t))/E[t,i]
 
    }
  
    optimal_time[i]<-which.min(omega[,i])
    min_impact[i]<-min(omega[,i])
    unit=min(omega[,i])*3  
    OTW[i,u]=optimal_time[i]
    Cost[i,u]=min_impact[i]
    
   #  dev.off()
  }
  
  plot.new()
  par(mar=c(5, 6, 4, 6) + 0.1)
  plot(etasa, Cost[i,], pch=2, axes=FALSE, ylim=c(0,max(Cost[i,]*2)), xlab="", ylab="", type="b",col="blueviolet", lwd=2)
  axis(2, ylim=c(0,Cost[i,]*2),col="black",las=1, main=paste("pump", i))  ## las=1 makes horzontal labels
  mtext(expression(paste("Impacts  (",, "mus)")),side=2,col="black",line=4)
  #axis(1,pretty(range(etasa),T))
  axis(1,c(seq(0,T, by=500)))
  box()
  
  par(new=TRUE)
  
  ## Plot the second plot and put axis scale on right
  plot(etasa, OTW[i,], pch=4,  xlab="", ylab="", ylim=c(0,T), 
       axes=FALSE, type="b", col="coral1",lwd=2,lty=2,main=paste("pump", i))
  ## a little farther out (line=4) to make room for labels
  mtext("OTW (Hours)",side=4,col="black",line=4) 
  axis(4, ylim=c(0,T), col="black",col.axis="black",las=1)
  mtext(expression(paste("Parameter ", eta)),side=1,col="black",line=2.5)  
  grid(10, 10, col = "lightgray", lty = "dotted",lwd = par("lwd"), equilogs = TRUE)
  legend("topright",inset=.08,legend=c("Impacts","OTW"),
         text.col=c("black"),pch=c(2,4),col=c("blueviolet","coral1"),horiz=F,cex=1,box.col = "white")

#  dev.off()
  
}

#Print out the results in a data frame objects

#ois<-data.frame(eta,beta,c,optimal_time,min_impact)

etasaresult<-data.frame(OTW, Cost)
print(etasaresult)
print(Cost)






