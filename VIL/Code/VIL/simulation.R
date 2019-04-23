
#options.wblr(blives=0.1) # make the legend boxes a bit shorter...
Eta=1000
Beta=3
N=100
xx=10000
factor=1
breakpoint=15

fails<-rweibull(5,3,5000)
print(fails)
median_percentile_ranks<-getPPP(fails)
print(median_percentile_ranks)#[,2]
MLEfit<-mlefit(mleframe(fails))
MLE_Unbiased<-c(MLEfit[1],MLEfit[2]*hrbu(length(fails)))

Eta<-MLE_Unbiased[[1]]
Beta<-MLE_Unbiased[[2]]
print(c(Eta,Beta))
simulation01<-wblr(fails)
simulation01 <- wblr.conf(wblr.fit(simulation01,col="green4",pch=3))
plot.wblr(list(simulation01),xlim=c(1,1e6))


#stop()
# Evolution

survivalweib<-function(x,a,b){exp(-(a*x)^b)}
failureweib<-function(x,a,b){a*b*x^(b-1)*exp(-(a*x)^b)}
cdfweib<-function(x,a,b){1-exp(-(a*x)^b)}
x=c(1:xx)#"I1-GCS1"

colors=c("red","violet","chartreuse3","chocolate1","darkgoldenrod1","darkmagenta","darkslategray","darksalmon","red","deeppink","deeppink4")

plot.new()
#  par(mar=c(5, 4, 4, 6) + 0.1)
plot(survivalweib(c(0:xx),1/Eta,Beta),pch=14,axes=FALSE,xlim=c(0,xx/factor),ylim=c(0,1),ylab="", xlab="",col=colors[1],type="l",lwd=2,cex=1)

#  par(new=TRUE)

axis(2, ylim=c(0,1),col="darkblue",las=1)  ## las=1 makes horizontal labels
axis(1,pretty(range(c(0:xx/factor)),breakpoint))
mtext(expression(paste("Time (hours)")),side=1,col="black",line=2.5)  
mtext(expression(paste("Reliability")),side=2,col="black",line=2.5) 
box()
grid(26, 11, col = "lightgray", lty = "dotted",lwd = par("lwd"), equilogs = TRUE)

