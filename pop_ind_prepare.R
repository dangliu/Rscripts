# Script to prepare the ind file and poplist for ChromoPainter
# Last updated: 06.Mar.2020

# Libraries
library(tidyverse)

# Read ind.list
ind_l <- read_delim("/mnt/scratch/dang/Thailand/Chromopainter/Ind.list", delim="\t", col_names=F)

# Read pop info
info <- read_csv("/r1/people/dang_liu/Projects/Thailand/ThaiCompare.info2-4.csv")


# Ind file
ind_f <- info %>% filter(IID %in% ind_l$X1) %>% select(IID, Pop) %>% arrange(factor(IID, levels = ind_l$X1), desc(Pop))
ind_f["Use"] <- ifelse(ind_f$Pop%in%c("Ignore_Mongola","Ignore_Hezhen", "Ignore", "Mongola", "Xibo"), 0, 1)
write_delim(ind_f, "/mnt/scratch/dang/Thailand/Chromopainter/CP_in_idfile.txt", delim="\t", col_names=F)


# Pop file
pop <- ind_f %>% filter(!Pop%in%c("Ignore_Mongola","Ignore_Hezhen", "Ignore", "Mongola", "Xibo")) %>% select(Pop) %>% distinct() 
pop["Type"] <- ifelse(pop$Pop %in% info[info$Country=="Thailand",]$Pop, "R", "D")
write_delim(pop, "/mnt/scratch/dang/Thailand/Chromopainter/CP_in_poplist.txt", delim="\t", col_names=F)

####
# Downsample individuals
# 4
set.seed(4)
d <- ind_f %>% filter(!Pop%in%c("Ignore_Mongola","Ignore_Hezhen", "Ignore", "Mongola", "Xibo")) %>% group_by(Pop) %>% sample_n(size=4) %>% arrange(factor(IID, levels = ind_l$X1), desc(Pop)) 
d %>% write_delim("/mnt/scratch/dang/Thailand/Chromopainter/Downsample_4_idfile.txt", delim="\t", col_names=F)

d %>% ungroup() %>% select(IID) %>% write_delim("/mnt/scratch/dang/Thailand/Chromopainter/Downsample_4.list", delim="\t", col_names=F)

# 8
set.seed(8)
d <- ind_f %>% filter(!Pop%in%c("Ignore_Mongola","Ignore_Hezhen", "Ignore", "Mongola", "Xibo")) %>% group_by(Pop) %>% sample_n(size=8) %>% arrange(factor(IID, levels = ind_l$X1), desc(Pop))  
d %>% write_delim("/mnt/scratch/dang/Thailand/Chromopainter/Downsample_8_idfile.txt", delim="\t", col_names=F)

d %>% ungroup() %>% select(IID) %>% write_delim("/mnt/scratch/dang/Thailand/Chromopainter/Downsample_8.list", delim="\t", col_names=F)
