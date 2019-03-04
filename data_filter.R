#!/usr/bin/Rscript
# Script to quickly filter one dataset by a shared specific column with another

# library here
library(data.table)
library(dplyr)

# set for command line
# File1, File2, Output
args <- commandArgs(TRUE)
in1 <- args[1]
in2 <- args[2]
out <- args[3]

# Read the two files
f1 <- fread(in1, header=T)
f2 <- fread(in2, header=T)

# Merge by a shared column 
# Use left_join: if something in f1 doesn't in f2, it will become 'NA' in that column
f_merge <- f1 %>% anti_join(f2)

# output
write.table(f_merge, out, sep="\t", quote=F, row.names=F)