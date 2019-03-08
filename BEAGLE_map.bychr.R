#!/usr/bin/Rscript
# Script to quickly make BEAGLE format maps

args <- commandArgs(TRUE)
in1 <- args[1]

# function
dec_pla <- function(n,dec_pla){
  as.numeric(format(round(n,dec_pla), nsmall = dec_pla))
}


mapBeagle<-read.table(in1,header=F)
mapBeagle[,2] <- "."
mapBeagle[,3] <- dec_pla(mapBeagle[,3]*100, 4) # for centi-morgan, to the 4th place after decimal

for (i in 1:24){ 
	temp <- mapBeagle[which(mapBeagle[,1]==i),] 
	write.table(temp, paste(c("referenceFromMyData_Chr",i,".map"), collapse=""), sep=" ", quote=F, col.names = F, row.names = F) 
}