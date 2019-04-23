library(FaultTree)
library(FaultTree.SCRAM)

fta_pag <- ftree.make(type="or", name="Not provide", name2="adequate LOS")
fta_pag<-addLogic(fta_pag, at=1, type="or", name="Failure of", name2="Power System")

fta_pag <- addLogic(fta_pag, at=2, type="or", name="neither emergency", name2="generator operable")
fta_pag <- addLogic(fta_pag, at=3, type="and", name="Independent failure", name2="of generators")
fta_pag <- addLatent(fta_pag, at=4, mttf=5,mttr=12/8760,inspect=1/26, name="e-gen 1 set fails")
fta_pag <- addLatent(fta_pag, at=4, mttf=5,mttr=12/8760,inspect=1/26, name="e-gen set fails")
fta_pag <- addLogic(fta_pag, at=3, type="inhibit", name="Common cause", name2="failure of generators")
fta_pag <- addProbability(fta_pag, at=7, prob=.05, name="Common cause", name2="beta factor")
fta_pag <- addLatent(fta_pag, at=7, mttf=5,mttr=12/8760,inspect=1/26, name="e-gen set fails")
fta_pag <- addDemand(fta_pag, at=2, mttf=0.5, name="External power", name2="interruption")

fta_pag <- addDemand(fta_pag, at=1, mttf=0.5, name="Failure of", name2="Control System")

fta_pag <- addDemand(fta_pag, at=1, mttf=0.5, name="Failure of PS", name2="due to Flood")


fta_pag<-addLogic(fta_pag, type="or", at=1, name="Failure of", name2="Pump System")

fta_pag<-addLogic(fta_pag, type="or", at=13, name="Failure of", name2="Storage Pumps")

fta_pag<-addLogic(fta_pag, type="or", at=14, name="SP1", name2="fails")


fta_pag<-addLogic(fta_pag, type="or", at=15, name="Drive", name2="failure")
fta_pag <- addActive(fta_pag,at=16,mttf=3,mttr=12/8760, name="Drive")

fta_pag<-addLogic(fta_pag, type="or", at=15, name="Clogging")
fta_pag <- addActive(fta_pag,at=18,mttf=3,mttr=12/8760, name="Clogging")

fta_pag<-addLogic(fta_pag, type="or", at=15, name="Mechanical", name2="failure")
fta_pag <- addActive(fta_pag,at=20,mttf=3,mttr=12/8760, name="Mech")

fta_pag<-addLogic(fta_pag, type="or", at=14, name="SP2", name2="fails")
fta_pag <- addActive(fta_pag,at=22,mttf=3,mttr=12/8760, name="SP2")


#fta_pag <- addActive(fta_pag,at=15,mttf=3,mttr=12/8760, name="Drive")


#fta_pag<-addLogic(fta_pag, type="or", at=14, name="SP3", name2="fails")
#fta_pag <- addActive(fta_pag,at=24,mttf=3,mttr=12/8760, name="SP2")

#Booster Pumps
fta_pag<-addLogic(fta_pag, type="or", at=13, name="Failure of", name2="Booster Pumps")


fta_pag<-addLogic(fta_pag, type="or", at=24, name="BP1", name2="fails")
fta_pag <- addActive(fta_pag,at=25,mttf=3,mttr=12/8760, name="BP1")

fta_pag<-addLogic(fta_pag, type="or", at=24, name="BP2", name2="fails")
fta_pag <- addActive(fta_pag,at=27,mttf=3,mttr=12/8760, name="BP1")


fta_pag<-addLogic(fta_pag, type="or", at=24, name="BP3", name2="fails")
fta_pag <- addActive(fta_pag,at=29,mttf=3,mttr=12/8760, name="BP1")

fta_pag<-addLogic(fta_pag, type="or", at=24, name="BP4", name2="fails")
fta_pag <- addActive(fta_pag,at=31,mttf=3,mttr=12/8760, name="BP1")

fta_pag<-addLogic(fta_pag, type="or", at=24, name="BP5", name2="fails")
fta_pag <- addActive(fta_pag,at=33,mttf=3,mttr=12/8760, name="BP1")


fta_pag<-addLogic(fta_pag, type="or", at=24, name="BP6", name2="fails")
fta_pag <- addActive(fta_pag,at=35,mttf=3,mttr=12/8760, name="BP1")


fta_pag<-addDemand(fta_pag, at=1, mttf=0.5, name="Sabotage at", name2="the PS")


fta_pag <- ftree.calc(fta_pag)
fta_pag[,1:21]
ftree2html(fta_pag, write_file=TRUE)

stop()


browseURL("fta_pag.html")

