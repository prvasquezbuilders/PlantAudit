#Sensitivity analysis

d=10 # factor of time
YearMax<-20 #total no of year to be investigated.
#colors=c("chocolate","coral3","coral4","cyan1","cyan4","darkblue","blue","chartreuse4","red","deeppink","deeppink4") #colors for plot
T=seq(0.1,YearMax, by =0.1)
factor=1
rho=0.085 #discount factor (interest rate)
yvalue=YearMax

plottime=20
plotimpact=20

maxpara<- 2.5
step=seq(1,maxpara, by =0.02)

para<- matrix(double(1),nrow=length(step),ncol=1)

#Time<-YearMax
maxobj=length(data$assets)
breakpoint=10

OIS<-matrix(nrow = maxobj,ncol = 2)
OTW<-matrix(nrow = maxobj,ncol = length(step))
Cost<-matrix(nrow = maxobj,ncol = length(step))
#SA on alpha parameter

  #         MAIN PROGRAM
  for (k in 1:maxobj){
    
    png(filename=paste("E:/OP18REFCS03/Stations/PAG/Report/figures/ch05_fig_sam_pump",k,".png",sep=""),width = 800, height = 500)  
     for (u in 1:length(step)){
      #  for (t in 1:length(step)){
       para[u]<-step[u]
      #..............................................
    cat(paste(" Optimal IS for BP #",k,  "\n"))
    # k=1
    pi=data$pi[k]
    ci=data$ci[k]
    alpha=data$alpha[k]
    m=para[u]
    source("lcccore.R")
    OTW[k,u]=T_optimal
    Cost[k,u]=Min_C
    }
    
    plot.new()
    par(mar=c(5, 4, 4, 6) + 0.1)
    plot(para, Cost[k,], pch=2, axes=FALSE, ylim=c(0,plotimpact), xlab="", ylab="", type="b",col="blueviolet", lwd=2, main=c(paste(data$assets[k])))
    axis(2, ylim=c(0,plotimpact),col="black",las=1)  ## las=1 makes horzontal labels
    mtext(expression(paste("Impacts  (",, "mus)")),side=2,col="black",line=2.5)
    axis(1,pretty(range(para),20))
    box()
    
    par(new=TRUE)
    
    ## Plot the second plot and put axis scale on right
    plot(para, OTW[k,], pch=4,  xlab="", ylab="", ylim=c(0,plottime), 
         axes=FALSE, type="b", col="coral1",lwd=2,lty=2)
    ## a little farther out (line=4) to make room for labels
    mtext("OTW (years)",side=4,col="black",line=2.3) 
    axis(4, ylim=c(0,plottime), col="black",col.axis="black",las=1)
    mtext(expression(paste("Parameter (m)")),side=1,col="black",line=2.5)  
    grid(10, 10, col = "lightgray", lty = "dotted",lwd = par("lwd"), equilogs = TRUE)
    legend("topright",inset=.08,legend=c("Impacts","OTW"),
           text.col=c("black"),pch=c(2,4),col=c("blueviolet","coral1"),horiz=F,cex=1.2,box.col = "white")
  
    dev.off()
    
  }
#}

print(OTW)
print(Cost)

#..............END.......................

#Plotting





