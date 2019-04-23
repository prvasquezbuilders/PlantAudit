#         MAIN PROGRAM
source("lcccore.R")

#png("rplot.png",width = 350, height = 350)
plot.new()
par(mar=c(5, 4, 4, 6) + 0.1)
plot(T,C/factor, axes=FALSE, ylim=c(0,yvalue), xlab="", ylab="", type="l",col="red", lwd=4)


axis(2, ylim=c(0,max(C)/factor),col="black",las=1)  ## las=1 makes horizontal labels
mtext(expression(paste("Impact (mus)" )),side=2,col="black",line=2.3)
box()
axis(1,pretty(range(seq(T)/d),d))
mtext(expression(paste("Time (years)")),side=1,col="black",line=2.5)  
grid(10, 10, col = "lightgray", lty = "dotted",lwd = par("lwd"), equilogs = TRUE)


points(T_optimal,Min_C/factor,pch=23,bg="red",lwd=3)
text(T_optimal, Min_C/factor*0.8,paste("(", format(T_optimal, 2),",", format(round(min(C), 2), nsmall = 2),")"),pos=4,cex = 1, srt = 0)


legend("topright",inset=.05,legend=c(paste(data$assets[k])),text.col="black", horiz=F,cex=0.8)

#dev.off()


