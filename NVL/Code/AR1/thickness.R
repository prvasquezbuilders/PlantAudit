data = dbConnect(MySQL(), user='root', password='', dbname='mwsi_plantaudit', host='localhost')
dbListTables(data)
dbListFields(data, 'vil_thickness')
rs = dbSendQuery(data, "select * from vil_thickness")
rs=dbFetch(rs)

#density plot
source("thickness_statistics.R")
#histogram plot
source("thickness_suction_straight.R")
#suction elbow
source("thickness_suction_elbow.R")
#discharge straigtht
source("thickness_discharge_straight.R")
#discharge straigtht
source("thickness_discharge_elbow.R")

#plot the inteplation line
#booster
plot(1, type="n", xlab="", ylab="", xlim=c(1, 25), ylim=c(0, 6))
axis(1, 1:25)
axis(2, 0:6)
mtext(expression(paste("Time (years)")),side=1,col="black",line=2.5)  
mtext(expression(paste("Thickness (mm)")),side=2,col="black",line=2.5) 
points(1,5.04,pch=23,bg="red",lwd=1)

text(0.8,4.5,"4.98",pos=4,cex = 1, srt = 0)
points(10,3.92,pch=23,bg="red",lwd=1)
text(8,3.8,"3.92",pos=4,cex = 1, srt = 0)
abline(a = 5.1, b = -0.12, col = 2, lwd=2)
abline(a = 2.9, b = 0, col = 3)
points(18.5,2.9,pch=23,bg="red",lwd=1)
text(1,3,"2.90",pos=4,cex = 1, srt = 0)

#storage
points(1,4.88,pch=22,bg="blue",lwd=1)


points(10,4.55,pch=22,bg="blue",lwd=1)
text(0.8,5.7,"5.45",pos=4,cex = 1, srt = 0)
abline(a = 5.6, b = -0.14, col = 4, lwd=2)
abline(a = 3.62, b = 0, col = 6)
text(10,4.3,"4.18",pos=4,cex = 1, srt = 0)
text(1,3.8,"3.62",pos=4,cex = 1, srt = 0)
points(14,3.62,pch=22,bg="blue",lwd=1)

#y=function(x){-0.1325*x+6.1725}

#plot(y(1:25), type='l')

legend("topright",inset=.05,legend=c("Booster", "Storage"),text.col="black",lty=c(1,1),pch=c(23,22),col=c("red", "blue"), horiz=F,cex=0.8)



#close the connection with MySQL

dbDisconnect(data) 
