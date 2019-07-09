# Script to run f4
# Last updated: 29.Mar.2019

# Libraries
library(tidyverse)
library(admixr)

# read data
data <- eigenstrat('/mnt/scratch/dang/Vietnam/pca/outgroup.v2/SEA.SC.TW.merged3.outgroup.v2.noFM.pruned')

ind <- read_ind(data)
pops <- unique(ind$label)
ancient <- c("Hi-Hon_Hai_Co_Tien","Hi-Kinabatagan","Hi-Supu_Hujung","IA-Long_Long_Rak","IA-Vat_Komnou","BA-Nui_Nap","N-Hon_Hai_Co_Tien","N-Loyang_Ujung","N-Mai_Da_Dieu","N-Nam_Tun","N-Oakaie","N-Tam_Pa_Ling","N-Tam_Hang","N-Man_Bac","N-Gua_Cha","Ho-Gua_Cha","Ho-Pha_Faen","P-Tianyuan")
modern_drift <- c("Mamanwa", "Jehai", "Mlabri","Onge")

# re-label pops
modif_d <- relabel(
  data,
  m = pops[!pops%in%c(ancient, modern_drift)],
  md = modern_drift,
  a = ancient
)

# output
system(paste("cp ", modif_d$group, " /mnt/scratch/dang/Vietnam/pca/outgroup.v2/SEA.SC.TW.merged3.outgroup.v2.noFM.pruned.ind"))
