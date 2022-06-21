# Script to prepare the ind file and poplist for running Kula samples on ChromoPainter
# Last updated: 25.Oct.2021

# Libraries
library(tidyverse)

# Read pop info
info <- read_csv("/r1/people/dang_liu/Projects/Kula/Kula.meta.info.csv") %>% filter(Pop!="Denisova")
pop_n4 <- read_delim("/mnt/scratch/dang/Kula/CP/Pop.n4.list", delim="\t", col_names=F)
colnames(pop_n4) <- "Pop"


# Groups
Collingwood_Bay <- c("Northern", "Wanigela", "Airara")
W_Mas <- c("Fergusson", "Normanby", "Mainland_Eastern_Tip")
N_Mas <- c("Trobriand", "Gawa", "Woodlark", "Laughlan")
S_Mas <- c("Misima", "Western_Calvados", "Eastern_Calvados", "Sudest", "Rossel")

Massim <- c(Collingwood_Bay, W_Mas, N_Mas, S_Mas)
Massim_noRossel <- Massim[Massim != "Rossel"]
Outgroup <- c("Africa", "WestEurasia")
Near_Out <- c("EastAsia", "EastAsia_AN", "Australia")
PNG_H <- c("S_Papuan", "Southern_Highlands", "Enga", "Western_Highlands", "Chimbu", "Eastern_Highlands", "Gulf", "Madang_Highland")
PNG_L <- c("Madang_Lowland", "Morobe", "East_Sepik", "Western", "Central")
Bismark <- c("Manus_New_Ireland", "West_New_Britain", "East_New_Britain")
Solomon <- c("Bougainville", "Vella_Lavella", "Malaita", "Santa_Cruz", "Bellona_Rennell", "Tikopia")



# Ind file
# Downsample individuals 5
set.seed(5)
ind_n4 <- pop_n4 %>% left_join(info) %>% filter(Filter=="PASS") %>% select(IID, Pop) 
ind_ds <- info %>% filter(!Pop %in% pop_n4$Pop & Filter=="PASS") %>% select(IID, Pop) %>% group_by(Pop) %>% sample_n(size=5)
ind_f <- ind_n4 %>% bind_rows(ind_ds) %>% arrange(factor(IID, levels = info$IID), desc(Pop))
ind_f['Use'] <- ifelse(ind_f$Pop%in%c(''), 0, 1)
write_delim(ind_f, "/mnt/scratch/dang/Kula/CP/CP_downsample_5_idfile.txt", delim="\t", col_names=F)
ind_f %>% select(IID) %>% write_delim("/mnt/scratch/dang/Kula/CP/CP_downsample_5_idfile.list", delim="\t", col_names=F)


# Pop file
pop <- ind_f %>% filter(!Pop%in%c("")) %>% select(Pop) %>% distinct() 
pop["Type"] <- ifelse(pop$Pop %in% info[info$Ref=="This_study",]$Pop, "R", "D")
D_target <- pop %>% filter(Type=="D")
D_target["Type"] <- "R"
pop <- pop %>% bind_rows(D_target)
write_delim(pop, "/mnt/scratch/dang/Kula/CP/CP_poplist.txt", delim="\t", col_names=F)
# v2
pop <- ind_f %>% filter(!Pop%in%c("")) %>% select(Pop) %>% distinct() 
pop["Type"] <- ifelse(pop$Pop %in% info[info$Label%in%Massim,]$Pop, "R", "D")
D_target <- pop %>% filter(Type=="D")
D_target["Type"] <- "R"
pop <- pop %>% bind_rows(D_target)
write_delim(pop, "/mnt/scratch/dang/Kula/CP/CP_poplist.v2.txt", delim="\t", col_names=F)
# v3
pop <- ind_f %>% filter(!Pop%in%c("")) %>% select(Pop) %>% distinct() 
pop["Type"] <- ifelse(pop$Pop %in% info[info$Label%in%Massim_noRossel,]$Pop, "R", "D")
D_target <- pop %>% filter(Type=="D")
D_target["Type"] <- "R"
pop <- pop %>% bind_rows(D_target)
write_delim(pop, "/mnt/scratch/dang/Kula/CP/CP_poplist.v3.txt", delim="\t", col_names=F)


