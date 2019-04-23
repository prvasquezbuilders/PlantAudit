
#-----------Motor Bearing-------unbias-------

failures<-c(1992,1986,624,3408)
#without suspension
median_percentile_ranks<-getPPP(failures)
print(median_percentile_ranks)#[,2]
MLEfit<-mlefit(mleframe(failures))
MLE_Unbiased<-c(MLEfit[1],MLEfit[2]*hrbu(length(failures)))

print(MLE_Unbiased)

eta<-MLE_Unbiased[[1]]
beta<-MLE_Unbiased[[2]]

da1 <- data.frame(
  serial=c("1","2","3","4"),
  time=c(failures),
  event=c(1,1,1,1))

da1=wblr(da1, col="red")
da1 <- wblr.fit(da1, col="darkgreen")
#da1 <- wblr.conf(da1, col="blue")
plot(da1, main=paste("Mechanical Seal - bias"))



#

plot.new()

#  png(filename=paste("E:/OP18REFCS03/Stations/VIL/Report/figures/ch05_fig_sur_pump",i,".png",sep=""),width = 800, height = 500) 

#  par(mar=c(5, 4, 4, 6) + 0.1)
plot(survi(c(0:T),eta,beta),pch=14,axes=FALSE,ylim=c(0,1),ylab="", xlab="",col="red",type="l",lwd=2,cex=1, main=paste("Bearing - bias"))
#  par(new=TRUE)
axis(2, ylim=c(0,1),col="darkblue",las=1)  ## las=1 makes horizontal labels
axis(1,pretty(range(c(0:T)),breakpoint))
mtext(expression(paste("Time (hours)")),side=1,col="black",line=2.5)  
mtext(expression(paste("Reliability")),side=2,col="black",line=2.5) 
box()
legend("topright",inset=.05,legend=c(paste("Bearing - bias")), text.col="black",lty=c(1),col=colors[i], horiz=F,cex=0.8,bg = "gray90")


#library(ggplot2)
#library(grid)
grid(26, 11, col = "lightgray", lty = "dotted",lwd = par("lwd"), equilogs = TRUE)
# dev.off()










#-----------Motor Bearing-------unbias-------

failures<-c(1992,1986,3408)
#without suspension
median_percentile_ranks<-getPPP(failures)
print(median_percentile_ranks)#[,2]
MLEfit<-mlefit(mleframe(failures))
MLE_Unbiased<-c(MLEfit[1],MLEfit[2]*hrbu(length(failures)))

print(MLE_Unbiased)

eta<-MLE_Unbiased[[1]]
beta<-MLE_Unbiased[[2]]

da1 <- data.frame(
  serial=c("1","2","3"),
  time=c(failures),
  event=c(1,1,1))

da1=wblr(da1, col="red")
da1 <- wblr.fit(da1, col="darkgreen")
#da1 <- wblr.conf(da1, col="blue")
plot(da1, main=paste("Bearing - unbias"))



#

plot.new()

#  png(filename=paste("E:/OP18REFCS03/Stations/VIL/Report/figures/ch05_fig_sur_pump",i,".png",sep=""),width = 800, height = 500) 

#  par(mar=c(5, 4, 4, 6) + 0.1)
plot(survi(c(0:T),eta,beta),pch=14,axes=FALSE,ylim=c(0,1),ylab="", xlab="",col="red",type="l",lwd=2,cex=1, main=paste("Bearing - unbias"))
#  par(new=TRUE)
axis(2, ylim=c(0,1),col="darkblue",las=1)  ## las=1 makes horizontal labels
axis(1,pretty(range(c(0:T)),breakpoint))
mtext(expression(paste("Time (hours)")),side=1,col="black",line=2.5)  
mtext(expression(paste("Reliability")),side=2,col="black",line=2.5) 
box()
legend("topright",inset=.05,legend=c(paste("Bearing - unbias")), text.col="black",lty=c(1),col=colors[i], horiz=F,cex=0.8,bg = "gray90")


#library(ggplot2)
#library(grid)
grid(26, 11, col = "lightgray", lty = "dotted",lwd = par("lwd"), equilogs = TRUE)
# dev.off()









