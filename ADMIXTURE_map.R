# title: "ADMIXTURE on map"
# author: "Dang Liu 21.Apr.2020"

# Last updated: 15.Dec.2021

# Libraries
library(data.table)
library(prismatic)
library(RColorBrewer)
library(tidyverse)
library(ggmap)
library(maps)
library(ggrepel)
library(scatterpie)

info <- read.table("/mnt/scratch/dang/Kula/Kula.meta.info", header=T)
head(info)
#info$Country <- factor(info$Country, levels=c("Taiwan","China","Vietnam","Cambodia","Laos","Thailand","Myanmar","Malaysia","Indonesia","Philippines","India"), ordered=T)

# Remove BGV and sample failing QC
info <- info %>% filter(Filter=="PASS" & is.na(Longitude)==F) %>% filter(Area!="Non_Oceania")

info2 <- info %>% group_by(Label) %>% 
  summarise_at(vars(Latitude,Longitude), funs(median(.))) %>% 
  left_join(select(info,-(FID:IID),-(Latitude:Longitude))) %>%
  distinct(Label, .keep_all = TRUE)

# Read admixture info
admixture <- read.table("/mnt/scratch/dang/Kula/admixture/map_view/bestK.popind.Q", header=F)
colnames(admixture) <- c("Label", "K1", "K2", "K3", "K4", "K5", "K6", "K7", "K8")
# average within pop and standarize by row
admixture2 <- admixture %>% group_by(Label) %>%
  summarise_at(vars(K1,K2,K3,K4,K5,K6,K7,K8), funs(mean(.))) %>%
  distinct(Label, .keep_all=T) %>% 
  mutate(row_sum=rowSums(select(., 2:9))) %>%
  mutate_at(2:9, ~ ./row_sum) %>%
  select(-row_sum) %>%
  left_join(info2)

# Remove 1KG samples
admixture2 <- admixture2 %>% filter(is.na(Latitude)==F)


# Incorporate sample size
size <- read.table("/mnt/scratch/dang/Kula/admixture/map_view/pop_ind.count", header=F)
colnames(size) <- c("Size","Label")

admixture2 <- admixture2 %>% left_join(size)


# Get hte map
map.world <- map_data(map="world")

# Visualization
# fix the limits with geo lon lat coordinates, according to the space you want to show by coord_quickmap
p <- ggplot()
p <- p + theme()
#p <- p + geom_map(data=map.world[map.world$region=="Papua New Guinea",], map=map.world[map.world$region=="Papua New Guinea",], aes(map_id=region), fill="white", colour="grey", size=0.15)
p <- p + geom_map(data=map.world, map=map.world, aes(map_id=region), fill="white", colour="grey", size=0.15)
p <- p + coord_quickmap(xlim=c(min(info$Longitude, na.rm=T),max(info$Longitude, na.rm=T)), ylim=c(min(info$Latitude, na.rm=T),max(info$Latitude, na.rm=T))) 
p <- p + geom_scatterpie(data=admixture2, aes(x=Longitude, y=Latitude, r=log10(Size*5)/5), cols=c("K1", "K2", "K3", "K4", "K5", "K6", "K7", "K8"), alpha=.8) #+ coord_fixed()
p <- p + geom_text_repel(data=admixture2, aes(x=Longitude, y=Latitude, label=Label), size=3, segment.alpha=0.5, nudge_x=0.5, nudge_y=0.5)
p <- p + geom_scatterpie_legend(log10(admixture2$Size*5)/5, x=162.5, y=-3.5, n=5, labeller=function(x) round(10^(x*5)/5))
p <- p + scale_fill_manual(values = c(K1="#4C005C",K2="#2BCE28",K3="#005C31",K4="#FFCC99",K5="#191919",K6="#0075DC",K7="#F0A3FF",K8="#993F00"))
#p <- p + scale_color_brewer(palette="Paired")
p <- p + theme(legend.position="none")
#p <- p + theme(legend.text = element_text(size = 12), legend.title = element_text(size = 14))
p <- p + theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14))
p <- p + theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10))
p <- p + labs(x="Longitude",y="Latitude",fill="Source")
p

# AHG
# Define groups
Collingwood_Bay <- c("Northern", "Wanigela", "Airara")
W_Mas <- c("Fergusson", "Normanby", "Mainland_Eastern_Tip")
N_Mas <- c("Trobriand", "Gawa", "Woodlark", "Laughlan")
S_Mas <- c("Misima", "Western_Calvados", "Eastern_Calvados", "Sudest", "Rossel")
Massim <- c(Collingwood_Bay, W_Mas, N_Mas, S_Mas)

AHG_anc <- admixture %>% 
  filter(Label%in%Massim) %>% 
  select(Label, K5, K6, K7) %>% 
  rename(Black="K5", Blue="K6", Pink="K7") %>%
  arrange(factor(Label, levels = Massim), desc(Label))
AHG <- admixture %>% 
  filter(Label%in%Massim) %>%
  select(Label, K5, K6, K7) %>% 
  group_by(Label) %>% 
  mutate(Condition=if_else(K5>=0.05&&K6>=0.05&&K7>=0.05, "PASS", "FAIL")) %>%
  mutate(Black_Blue_Pink=cov(log(K5)-log(K6), log(K7))) %>% # Equals to mutate(Black_Blue_Pink=cov(log(K5/K6), log(K7)))
  mutate(Blue_Pink_Black=cov(log(K6)-log(K7), log(K5))) %>%
  mutate(Pink_Black_Blue=cov(log(K7)-log(K5), log(K6))) %>%
  select(-(K5:K7)) %>%
  distinct(.keep_all=T) %>%
  arrange(factor(Label, levels = Massim), desc(Label))
write_csv(AHG_anc, "/r1/people/dang_liu/Projects/Kula/admixture/AHG_anc.csv", quote="none")
write_csv(AHG, "/r1/people/dang_liu/Projects/Kula/admixture/AHG.csv", quote="none")

#############################################################

## K=2


# Read admixture info
admixture <- read.table("/mnt/scratch/dang/Kula/admixture/map_view/K2.popind.Q", header=F)
colnames(admixture) <- c("Label", "K1", "K2")
# average within pop and standarize by row
admixture2 <- admixture %>% group_by(Label) %>%
  summarise_at(vars(K1,K2), funs(mean(.))) %>%
  distinct(Label, .keep_all=T) %>% 
  mutate(row_sum=rowSums(select(., 2:3))) %>%
  mutate_at(2:3, ~ ./row_sum) %>%
  select(-row_sum) %>%
  left_join(info2)

# Incorporate sample size
size <- read.table("/mnt/scratch/dang/Kula/admixture/map_view/pop_ind.count", header=F)
colnames(size) <- c("Size","Label")

admixture2 <- admixture2 %>% left_join(size)


# Get hte map
map.world <- map_data(map="world")

# Visualization
# fix the limits with geo lon lat coordinates, according to the space you want to show by coord_quickmap
p <- ggplot()
p <- p + theme()
#p <- p + geom_map(data=map.world[map.world$region=="Papua New Guinea",], map=map.world[map.world$region=="Papua New Guinea",], aes(map_id=region, x=long, y=lat), fill="white", colour="grey", size=0.15)
p <- p + geom_map(data=map.world, map=map.world, aes(map_id=region), fill="white", colour="grey", size=0.15)
p <- p + coord_quickmap(xlim=c(min(info$Longitude, na.rm=T),max(info$Longitude, na.rm=T)), ylim=c(min(info$Latitude, na.rm=T),max(info$Latitude, na.rm=T)))
p <- p + geom_text_repel(data=admixture2, aes(x=Longitude, y=Latitude, label=Label), size=3, segment.alpha=0.5, nudge_x=0.5, nudge_y=0.5)
p <- p + geom_scatterpie(data=admixture2, aes(x=Longitude, y=Latitude, r=log10(Size*5)/5), cols=c("K1", "K2"), alpha=.8) #+ coord_fixed()
p <- p + geom_scatterpie_legend(log10(admixture2$Size*5)/5, x=162.5, y=-3.5, n=5, labeller=function(x) round(10^(x*5)/5))
p <- p + scale_fill_manual(values = c(K1="#0075DC",K2="#F0A3FF"))
#p <- p + scale_color_brewer(palette="Paired")
p <- p + theme(legend.position="none")
#p <- p + theme(legend.text = element_text(size = 12), legend.title = element_text(size = 14))
p <- p + theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14))
p <- p + theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10))
p <- p + labs(x="Longitude",y="Latitude",fill="Source")
p
