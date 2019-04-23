
#the second statistical table


bp1<-(1:4)
bp2<-(5:8)
bp3<-(9:12)
bp4<-(13:16)
sp1<-(17:20)
sp2<-(21:24)


thicka<-matrix(nrow = 12)

#suction - booster
bp1suction<-c(rs[bp1,9]) #bp1
bp2suction<-c(rs[bp2,9]) #bp2
bp3suction<-c(rs[bp3,9]) #bp3
bp4suction<-c(rs[bp4,9]) #bp4

#suction - storage
sp1suction<-c(rs[sp1,9])
sp2suction<-c(rs[sp2,9])



#discharge -booster
bp1discharge<-c(rs[bp1,14])
bp2discharge<-c(rs[bp2,14])
bp3discharge<-c(rs[bp3,14])
bp4discharge<-c(rs[bp4,14])
#discharge - storage
sp1discharge<-c(rs[sp1,14])
sp2discharge<-c(rs[sp2,14])

thicknesminmax<-matrix(nrow = 6, ncol = 8)

#bp1
column=1
bps<-bp1suction
bpd<-bp1discharge


thicknesminmax[column,1]<-min(bps,na.rm=TRUE)
thicknesminmax[column,2]<-mean(bps,na.rm=TRUE)
thicknesminmax[column,3]<-0
thicknesminmax[column,4]<-max(bps,na.rm=TRUE)

thicknesminmax[column,5]<-min(bpd,na.rm=TRUE)
thicknesminmax[column,6]<-mean(bpd,na.rm=TRUE)
thicknesminmax[column,7]<-0
thicknesminmax[column,8]<-max(bpd,na.rm=TRUE)



#bp2
column=2
bps<-bp2suction
bpd<-bp2discharge

thicknesminmax[column,1]<-min(bps,na.rm=TRUE)
thicknesminmax[column,2]<-mean(bps,na.rm=TRUE)
thicknesminmax[column,3]<-0
thicknesminmax[column,4]<-max(bps,na.rm=TRUE)

thicknesminmax[column,5]<-min(bpd,na.rm=TRUE)
thicknesminmax[column,6]<-mean(bpd,na.rm=TRUE)
thicknesminmax[column,7]<-0
thicknesminmax[column,8]<-max(bpd,na.rm=TRUE)

#bp3
column=3
bps<-bp3suction
bpd<-bp3discharge

thicknesminmax[column,1]<-min(bps,na.rm=TRUE)
thicknesminmax[column,2]<-mean(bps,na.rm=TRUE)
thicknesminmax[column,3]<-0
thicknesminmax[column,4]<-max(bps,na.rm=TRUE)

thicknesminmax[column,5]<-min(bpd,na.rm=TRUE)
thicknesminmax[column,6]<-mean(bpd,na.rm=TRUE)
thicknesminmax[column,7]<-0
thicknesminmax[column,8]<-max(bpd,na.rm=TRUE)

#bp3
column=3
bps<-bp3suction
bpd<-bp3discharge

thicknesminmax[column,1]<-min(bps,na.rm=TRUE)
thicknesminmax[column,2]<-mean(bps,na.rm=TRUE)
thicknesminmax[column,3]<-0
thicknesminmax[column,4]<-max(bps,na.rm=TRUE)

thicknesminmax[column,5]<-min(bpd,na.rm=TRUE)
thicknesminmax[column,6]<-mean(bpd,na.rm=TRUE)
thicknesminmax[column,7]<-0
thicknesminmax[column,8]<-max(bpd,na.rm=TRUE)


#bp4
column=4
bps<-bp4suction
bpd<-bp4discharge

thicknesminmax[column,1]<-min(bps,na.rm=TRUE)
thicknesminmax[column,2]<-mean(bps,na.rm=TRUE)
thicknesminmax[column,3]<-0
thicknesminmax[column,4]<-max(bps,na.rm=TRUE)

thicknesminmax[column,5]<-min(bpd,na.rm=TRUE)
thicknesminmax[column,6]<-mean(bpd,na.rm=TRUE)
thicknesminmax[column,7]<-0
thicknesminmax[column,8]<-max(bpd,na.rm=TRUE)

#sp1
column=5
bps<-sp1suction
bpd<-sp1discharge

thicknesminmax[column,1]<-min(bps,na.rm=TRUE)
thicknesminmax[column,2]<-mean(bps,na.rm=TRUE)
thicknesminmax[column,3]<-0
thicknesminmax[column,4]<-max(bps,na.rm=TRUE)

thicknesminmax[column,5]<-min(bpd,na.rm=TRUE)
thicknesminmax[column,6]<-mean(bpd,na.rm=TRUE)
thicknesminmax[column,7]<-0
thicknesminmax[column,8]<-max(bpd,na.rm=TRUE)

#sp1
column=6
bps<-sp2suction
bpd<-sp2discharge

thicknesminmax[column,1]<-min(bps,na.rm=TRUE)
thicknesminmax[column,2]<-mean(bps,na.rm=TRUE)
thicknesminmax[column,3]<-0
thicknesminmax[column,4]<-max(bps,na.rm=TRUE)

thicknesminmax[column,5]<-min(bpd,na.rm=TRUE)
thicknesminmax[column,6]<-mean(bpd,na.rm=TRUE)
thicknesminmax[column,7]<-0
thicknesminmax[column,8]<-max(bpd,na.rm=TRUE)

print(thicknesminmax)


#suction straight line

suction<-matrix(nrow = 3, ncol = 6)

colnames(suction)=c("BP1","BP2","BP3","BP4","SP1","SP2")
rownames(suction)=c("min","mean","max")

#bp1

rb<-1
suction[1,rb]=thicknesminmax[rb,1]
suction[2,rb]=thicknesminmax[rb,2]
suction[3,rb]=thicknesminmax[rb,4]

#bp2

rb<-2
suction[1,rb]=thicknesminmax[rb,1]
suction[2,rb]=thicknesminmax[rb,2]
suction[3,rb]=thicknesminmax[rb,4]

#bp3

rb<-3
suction[1,rb]=thicknesminmax[rb,1]
suction[2,rb]=thicknesminmax[rb,2]
suction[3,rb]=thicknesminmax[rb,4]


#bp4

rb<-4
suction[1,rb]=thicknesminmax[rb,1]
suction[2,rb]=thicknesminmax[rb,2]
suction[3,rb]=thicknesminmax[rb,4]



#sp1
ra<-c(21:24)
rb<-5
suction[1,rb]=thicknesminmax[rb,1]
suction[2,rb]=thicknesminmax[rb,2]
suction[3,rb]=thicknesminmax[rb,4]

#sp2

rb<-6
suction[1,rb]=thicknesminmax[rb,1]
suction[2,rb]=thicknesminmax[rb,2]
suction[3,rb]=thicknesminmax[rb,4]

suction = round(suction, digits=2)

# Grouped barplot
x=barplot(suction, col=colors()[c(23,89,12)] , border="white", font.axis=1, beside=T, ylim=c(0,7), xlab="pumps", font.lab=1 )
box()
y=suction
text(x,y+0.5,labels=as.character(y),srt=90, col="red")
#text(x = xx, y = dat$freqs, label = dat$freqs, pos = 3, cex = 0.8, col = "red")

legend("topleft", legend=rownames(suction), fill=colors()[c(23,89,12)],cex=0.8);
