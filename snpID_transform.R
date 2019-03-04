#!/usr/bin/Rscript
# Script to quickly transform Affymetrix6.0 SNP IDs to Human Origins SNP IDs according to their overlapping order


# library here
library(data.table)
library(dplyr)

# set for command line
# File1, File2, Output
args <- commandArgs(TRUE)
in1 <- args[1]
in2 <- args[2]


f1 <- fread(in1,header=F)
colnames(f1) <- c("CHR", "SNP", "START", "END", "REF", "ALT")
head(f1)
f2 <- fread(in2,header=F)
head(f2)
f1$SNP <- f2
head(f1)
# output
write.table(f1, in1, sep="\t", quote=F, col.names=F, row.names=F)