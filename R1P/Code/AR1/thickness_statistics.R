#booster pumps
boosterrange=(1:16)
storagerange=(17:24)
bs <-c(rs[boosterrange,5],rs[boosterrange,6],rs[boosterrange,7],rs[boosterrange,8])

bse <-c(rs[boosterrange,9])
bd <- c(rs[boosterrange,10],rs[boosterrange,11],rs[boosterrange,12],rs[boosterrange,13])
bde <-c(rs[boosterrange,14])

summary(bs)
summary(bse)
summary(bd)
summary(bde)

#storage pumps
ss <-c(rs[storagerange,5],rs[storagerange,6],rs[storagerange,7],rs[storagerange,8])

sse <-c(rs[storagerange,9])
sd <- c(rs[storagerange,10],rs[storagerange,11],rs[storagerange,12],rs[storagerange,13])
sde <-c(rs[storagerange,14])

summary(ss)
summary(sse)
summary(sd)
summary(sde)


#write the summary into a file

thicknesssat<-matrix(nrow = 6, ncol = 8)

for (i in 1:6){
  thicknesssat[i,1]<-summary(bs)[[i]]
  thicknesssat[i,2]<-summary(bse)[[i]]
  thicknesssat[i,3]<-summary(bd)[[i]]
  thicknesssat[i,4]<-summary(bde)[[i]]
  thicknesssat[i,5]<-summary(sse)[[i]]
  thicknesssat[i,6]<-summary(sse)[[i]]
  thicknesssat[i,7]<-summary(sde)[[i]]
  thicknesssat[i,8]<-summary(sde)[[i]]
  
}


#the first statistica table
print(thicknesssat)

write.csv(thicknesssat, file = "thicknesssat.csv")

#plot a line


#the second statistical table


bp1<-(1:4)
bp2<-(5:8)
bp3<-(9:12)
bp4<-(13:16)
sp1<-(17:20)
sp2<-(21:24)


thicka<-matrix(nrow = 12)

#suction - booster
bp1suction<-c(rs[bp1,5],rs[bp1,6],rs[bp1,7],rs[bp1,8],rs[bp1,9]) #bp1
bp2suction<-c(rs[bp2,5],rs[bp2,6],rs[bp2,7],rs[bp2,8],rs[bp2,9]) #bp2
bp3suction<-c(rs[bp3,5],rs[bp3,6],rs[bp3,7],rs[bp3,8],rs[bp3,9]) #bp3
bp4suction<-c(rs[bp4,5],rs[bp4,6],rs[bp4,7],rs[bp4,8],rs[bp4,9]) #bp4

#suction - storage
sp1suction<-c(rs[sp1,5],rs[sp1,6],rs[sp1,7],rs[sp1,8],rs[sp1,9])
sp2suction<-c(rs[sp2,5],rs[sp2,6],rs[sp2,7],rs[sp2,8],rs[sp2,9])



#discharge -booster
bp1discharge<-c(rs[bp1,10],rs[bp1,11],rs[bp1,12],rs[bp1,13],rs[bp1,14])
bp2discharge<-c(rs[bp2,10],rs[bp2,11],rs[bp2,12],rs[bp2,13],rs[bp2,14])
bp3discharge<-c(rs[bp3,10],rs[bp3,11],rs[bp3,12],rs[bp3,13],rs[bp3,14])
bp4discharge<-c(rs[bp4,10],rs[bp4,11],rs[bp4,12],rs[bp4,13],rs[bp4,14])
#discharge - storage
sp1discharge<-c(rs[sp1,10],rs[sp1,11],rs[sp1,12],rs[sp1,13],rs[sp1,14])
sp2discharge<-c(rs[sp2,10],rs[sp2,11],rs[sp2,12],rs[sp2,13],rs[sp2,14])

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

write.csv(thicknesminmax, file = "thicknesminmax.csv")




