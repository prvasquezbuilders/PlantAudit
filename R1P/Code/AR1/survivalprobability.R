
#read the data from csv file

# Evolution
YearMax<-20
yearoffocus=5
maxobj=length(data$assets)
breakpoint=20

survivalweib<-function(x,a,b){exp(-(a*x)^b)}
failureweib<-function(x,a,b){a*b*x^(b-1)*exp(-(a*x)^b)}
cdfweib<-function(x,a,b){1-exp(-(a*x)^b)}
x=c(1:YearMax)#"I1-GCS1"

#colors=c("red","violet","chartreuse3","chocolate1","darkgoldenrod1","darkmagenta","darkslategray","darksalmon","red","deeppink","deeppink4")


#library(RColorBrewer)
#library(randomcoloR)

#colors <- distinctColorPalette(maxobj)
plot.new()

for (i in 1:maxobj){
  
  png(filename=paste("E:/OP18REFCS03/Stations/PAG/Report/figures/ch05_fig_sur_pump",i,".png",sep=""),width = 800, height = 500) 
  
#  par(mar=c(5, 4, 4, 6) + 0.1)
  plot(survivalweib(c(0:YearMax),data$alpha[i],data$m[i]),pch=14,axes=FALSE,ylim=c(0,1),ylab="", xlab="",col=colors[i],type="l",lwd=2,cex=1)
#  par(new=TRUE)
  axis(2, ylim=c(0,1),col="darkblue",las=1)  ## las=1 makes horizontal labels
  axis(1,pretty(range(c(0:YearMax)),breakpoint))
  mtext(expression(paste("Time (years)")),side=1,col="black",line=2.5)  
  mtext(expression(paste("Reliability")),side=2,col="black",line=2.5) 
  box()
  legend("topright",inset=.05,legend=c(paste(data$assets[i])),text.col="black",lty=c(1),col=colors[i], horiz=F,cex=0.8,bg = "gray90")
  #library(ggplot2)
  #library(grid)
  grid(26, 11, col = "lightgray", lty = "dotted",lwd = par("lwd"), equilogs = TRUE)
  dev.off()
}

#overall
plot.new()
#png(filename=paste("E:/OP18REFCS03/Stations/PAG/Report/figures/ch05_fig_reliability01.png"),width = 800, height = 500) 
for (i in 1:maxobj){
 # par(mar=c(5, 4, 4, 6) + 0.1,xaxs="i")
  plot(survivalweib(c(0:YearMax),data$alpha[i],data$m[i]),pch=14,axes=FALSE,ylim=c(0.8,1),xlim=c(1,yearoffocus),ylab="", xlab="",col=colors[i],type="l",lwd=1,cex=0.5)
  par(new=TRUE)
}
axis(2, ylim=c(0.8,1),col="darkblue",las=1)  ## las=1 makes horizontal labels
axis(1,pretty(range(c(1:yearoffocus))))
mtext(expression(paste("Time (years)")),side=1,col="black",line=2.5)  
mtext(expression(paste("Reliability")),side=2,col="black",line=2.5)  
box()
legend("bottomleft",inset=.05,legend=c("BP1","BP2","BP3","BP4","BP5","BP6","SP1","SP2"),text.col="black",lty=c(1,1,1,1,1,1,1,1),col=colors, horiz=F,cex=1,bg = "gray90")
#library(ggplot2)
#library(grid)
axis(1, at= 1:5)
#axis(2, at= 0.6:1)
grid(10, 10, col = "lightgray", lty = "dotted",lwd = par("lwd"), equilogs = TRUE)

#dev.off()





