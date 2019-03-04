#!/usr/bin/Rscript
# Script to run f3

# Libraries
library(tidyverse)
library(admixr)

# function
msg <- function(...,sep="",new.line=FALSE){
  cat(paste(...,sep=sep))
  if(new.line) cat("\n")
}
msgn <- function(...,sep="",new.line=TRUE){
  msg(...,sep=sep,new.line=new.line)
}

# set for command line
# File1, File2, Output
args <- commandArgs(TRUE)
prefix <- args[1]

# read data
data <- eigenstrat(prefix)

# pop
ind <- read_ind(data)
table(ind$label)
pops <- unique(ind$label)
pops
pops <- pops[!pops %in% c("French","Mbuti")]


# outgroup f3
msgn("Running f3 (outgroup is Mbuti)...")
f3_res = f3(data, A=pops, B=pops, C="Mbuti")

# output
save(f3_res, file=paste(prefix, "Mbuti_f3.Rdata", sep="."))
write.table(f3_res, paste(prefix, "Mbuti_f3.txt", sep="."), sep="\t", quote=F, row.names=F)

# outgroup f3
msgn("Running f3 (outgroup is French)...")
f3_res = f3(data, A=pops, B=pops, C="French")

# output
save(f3_res, file=paste(prefix, "French_f3.Rdata", sep="."))
write.table(f3_res, paste(prefix, "French_f3.txt", sep="."), sep="\t", quote=F, row.names=F)


msgn("All done!")