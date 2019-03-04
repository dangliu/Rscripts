#!/usr/bin/Rscript
# Script to quickly make BEAGLE format maps

args <- commandArgs(TRUE)
in1 <- args[1]


mapBeagle<-read.table(in1,header=F)
mapBeagle[,2] <- "."
mapBeagle[,3] <- mapBeagle[,3]*100 # for centi-morgan

for (i in 1:24){ 
	temp <- mapBeagle[which(mapBeagle[,1]==i),] 
	write.table(temp, paste(c("referenceFromMyData_Chr",i,".map"), collapse=""), sep=" ", quote=F, col.names = F, row.names = F) 
}