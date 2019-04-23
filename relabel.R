# Script to run f4
# Last updated: 29.Mar.2019

# Libraries
library(tidyverse)
library(admixr)

# read data
data <- eigenstrat('/mnt/scratch/dang/Vietnam/pca/outgroup.v2/SEA.SC.TW.merged3.outgroup.v2.noFM.pruned')

ind <- read_ind(data)
pops <- unique(ind$label)
ancient <- c("Hon_Hai_Co_Tien","Kinabatagan","Supu_Hujung","Long_Long_Rak","Vat_Komnou","Nui_Nap","Loyang_Ujung","Mai_Da_Dieu","Nam_Tun","Oakaiel","Tam_Pa_Ping","Tam_Hang","Man_Bac","Gua_Cha","Pha_Faen","Tianyuan")
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
