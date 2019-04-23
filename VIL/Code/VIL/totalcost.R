#INPUT

d=10 # factor of time
YearMax<-20 #total no of year to be investigated.
#colors=c("chocolate","coral3","coral4","cyan1","cyan4","darkblue","blue","chartreuse4","red","deeppink","deeppink4") #colors for plot
T=seq(0.1,YearMax, by =0.1)
factor=1
rho=0.085 #discount factor (interest rate)
yvalue=20
#Time<-YearMax
maxobj=length(data$assets)
breakpoint=10

OIS<-matrix(nrow = maxobj,ncol = 2)
OTW<-matrix(nrow = maxobj)
Cost<-matrix(nrow = maxobj)

for (k in 1:maxobj){
  #..........BP1............................
  cat(paste(" Optimal IS for BP #",k,  "\n"))
 # k=1
  pi=data$pi[k]
  ci=data$ci[k]
  alpha=data$alpha[k]
  m=data$m[k]
#  png(filename=paste("E:/OP18REFCS03/Stations/PAG/Report/figures/ch05_fig_ois_pump",k,".png",sep=""),width = 800, height = 500)
  source("lcc.R")
  OTW[k]=T_optimal
  Cost[k]=Min_C
#  dev.off()
}
assets=data$assets
OIS=data.frame(assets,OTW,Cost)
print(OIS)
write.csv(OIS, file = "OIS.csv")







