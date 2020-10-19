#!/usr/bin/Rscript
# Script to convert sample geo-info and refinedIBD output to MAPS inputs

# Last updated: 18.Jun.2020

# function
msg <- function(...,sep="",new.line=FALSE){
  cat(paste(...,sep=sep))
  if(new.line) cat("\n")
}
msgn <- function(...,sep="",new.line=TRUE){
  msg(...,sep=sep,new.line=new.line)
}

# Arguments
args <- commandArgs(TRUE)
info_file <- args[1]
IBD_file <- args[2]
lowerBnd <- args[3]
upperBnd <- args[4]


## read in the meta info data
meta_info_pre <- read.table(info_file, header=T)
meta_info <- data.frame(fid = meta_info_pre$FID, iid = meta_info_pre$IID, long = meta_info_pre$Longitude, 
                        lat = meta_info_pre$Latitude, stringsAsFactors = FALSE)

msgn("Info file processed...")
head(meta_info)

## read in the IBD and HBD calls
## In this pipeline, both the .ibd and .hbd files are utilized (see the relabelIBD rule in the https://github.com/halasadi/ibd_data_pipeline/blob/master/runibdpipeline)
ibd_data_pre <- read.table(IBD_file, header = FALSE, stringsAsFactors = FALSE)
colnames(ibd_data_pre) <- c("IID1", "HAP_ID1", "IID2", "HAP_ID2", "CHR", "START", "END", "LOD", "LEN_CM")

ibd_data <- data.frame(id1 = paste0(ibd_data_pre$IID1, "_", ibd_data_pre$HAP_ID1), id2 = paste0(ibd_data_pre$IID2, "_", ibd_data_pre$HAP_ID2), length = ibd_data_pre$LEN_CM)

msgn("IBD file processed...")
head(ibd_data)

# Include individual only occurs in IBD data
ibd_ids <- c(unique(c(ibd_data_pre$IID1, ibd_data_pre$IID2)))
ids <- paste0(meta_info[meta_info$iid %in% ibd_ids,]$iid)

# first long, then lat
locs <- paste0(meta_info[meta_info$iid %in% ids,]$long, " ", meta_info[meta_info$iid %in% ids,]$lat)

# BEAGLE outputs haplotype data, we treat
# the two different chromosomes of each individual as independent
ids <- c(paste0(ids, "_1"), paste0(ids, "_2"))
locs <- c(locs, locs)

msgn("Geo info integrated...")

n <- length(ids) 

msgn(paste0("Total number of individuals is ", n/2, ", and potential number of different chromosomes is ", n, ""))

# set up the matrix
ibd_summary <- matrix(nrow = n, ncol = n, 0)
rownames(ibd_summary) <- ids
colnames(ibd_summary) <- ids

# The lengths of the IBD segments
lengths <- as.numeric(ibd_data$length)

# highlight IBD segments in a length range
lowerBnd <- as.numeric(lowerBnd)
upperBnd <- as.numeric(upperBnd)
selected_inds <- which(lengths > lowerBnd & lengths < upperBnd)

msgn(paste0("Lower boundry is ", lowerBnd))
msgn(paste0("Upper boundry is ", upperBnd))

for (i in 1:length(selected_inds)){
  id1 <- as.character(ibd_data$id1[selected_inds[i]])
  id2 <- as.character(ibd_data$id2[selected_inds[i]])
  if (id1 %in% ids & id2 %in% ids){
    ibd_summary[id1, id2] = ibd_summary[id1, id2] + 1
    ibd_summary[id2, id1] = ibd_summary[id1, id2]
  }
}

## write similarity matrix and coordinates to a .sims and .coord file respectively
write.table(ibd_summary, file = paste0("maps_", lowerBnd, "_", upperBnd, ".sims"), quote=FALSE, sep = " ", row.names = FALSE, col.names=FALSE)
write.table(data.frame(locs), file = paste0("maps_", lowerBnd, "_", upperBnd, ".coord"), quote=FALSE, sep = " ", row.names = FALSE, col.names=FALSE)

msgn(paste0("All done! ", paste0("maps_", lowerBnd, "_", upperBnd, ".sims"), " and ", paste0("maps_", lowerBnd, "_", upperBnd, ".coord"), " are output"))