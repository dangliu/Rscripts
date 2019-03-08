#!/usr/bin/Rscript
# Script to quickly make SHAPEIT format maps

# library
library(tidyverse)

# function
dec_pla <- function(n,dec_pla){
  sprintf(as.numeric(format(round(n,dec_pla), nsmall = dec_pla)), fmt = paste("%#.",dec_pla,"f", sep=""))
}

args <- commandArgs(TRUE)
in1 <- args[1]


mapShapeIt<-read.table(in1,header=F)
mapShapeIt[,2] <- mapShapeIt[,4] # bp
mapShapeIt[,4] <- dec_pla(mapShapeIt[,3]*100, 4) # for centi-morgan
temp <- mapShapeIt %>% 
	group_by(V1) %>% 
	mutate(diff = as.numeric(V4) - lag(as.numeric(V4), default = 0)) %>% 
	mutate(diff2 = V2/1e6 - lag(V2/1e6, default = 0)) 
	# cM/Mb (0.41...-0)/(0.721290-0.568527)
mapShapeIt[,3] <- dec_pla(temp$diff/temp$diff2, 4)


colnames(mapShapeIt) <- c("chr","pposition","rrate","gposition")

for (i in 1:24){ 
	temp <- mapShapeIt[which(mapShapeIt[,1]==i),] 
	write.table(temp[,2:4], paste(c("referenceFromMyData_Chr",i,".map"), collapse=""), sep=" ", quote=F, col.names = T, row.names = F) 
}