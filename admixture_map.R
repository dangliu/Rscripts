# title: "ADMIXTURE on map"
# author: "Dang Liu 08.Oct.2019"

# Last updated: 08.Oct.2019

# Libraries
library(data.table)
library(RColorBrewer)
library(tidyverse)
library(ggmap)
library(maps)
library(ggrepel)
library(scatterpie)

info <- read.table("/mnt/scratch/dang/New_Guinea/PNG.info", header=T)
head(info)
#info$Country <- factor(info$Country, levels=c("Taiwan","China","Vietnam","Cambodia","Laos","Thailand","Myanmar","Malaysia","Indonesia","Philippines","India"), ordered=T)

# Remove BGV and sample failing QC
info <- info %>% filter(!Pop=="Bougainville" & Filter=="PASS")

info2 <- info %>% group_by(Pop) %>% 
  summarise_at(vars(Latitude,Longitude), funs(median(.))) %>% 
  left_join(select(info,-(FID:IID),-(Latitude:Longitude))) %>%
  distinct(Pop, .keep_all = TRUE)

# Read admixture info
admixture <- read.table("/mnt/scratch/dang/New_Guinea/ADMIXTURE/map_view/bestK.popind.Q", header=F)
colnames(admixture) <- c("Pop", "K1", "K2", "K3", "K4", "K5", "K6", "K7", "K8", "K9")
# average within pop and standarize by row
admixture2 <- admixture %>% group_by(Pop) %>%
  summarise_at(vars(K1,K2,K3,K4,K5,K6,K7,K8,K9), funs(mean(.))) %>%
  distinct(Pop, .keep_all=T) %>% 
  mutate(row_sum=rowSums(select(., 2:10))) %>%
  mutate_at(2:10, ~ ./row_sum) %>%
  select(-row_sum) %>%
  left_join(info2)

# Remove 1KG samples
admixture2 <- admixture2 %>% filter(is.na(Latitude)==F)


# Incorporate sample size
size <- read.table("/mnt/scratch/dang/New_Guinea/ADMIXTURE/map_view/pop_ind.count", header=F)
colnames(size) <- c("Size","Pop")

admixture2 <- admixture2 %>% left_join(size)


# Get hte map
map.world <- map_data(map="world")

# Visualization
# fix the limits with geo lon lat coordinates, according to the space you want to show by coord_quickmap
p <- ggplot()
p <- p + theme()
p <- p + geom_map(data=map.world[map.world$region=="Papua New Guinea",], map=map.world[map.world$region=="Papua New Guinea",], aes(map_id=region, x=long, y=lat), fill="white", colour="grey", size=0.15)
#p <- p + geom_map(data=map.world, map=map.world, aes(map_id=region, x=long, y=lat), fill="white", colour="grey", size=0.15)
#p <- p + coord_quickmap(xlim=c(min(info$Longitude, na.rm=T),max(info$Longitude, na.rm=T)), ylim=c(min(info$Latitude, na.rm=T),max(info$Latitude, na.rm=T))) 
p <- p + geom_scatterpie(data=admixture2, aes(x=Longitude, y=Latitude, r=log10(Size*50)/10), cols=c("K1", "K2", "K3", "K4", "K5", "K6", "K7", "K8", "K9"), alpha=.8) + coord_fixed()
p <- p + geom_scatterpie_legend(log10(admixture2$Size*50)/10, x=142, y=-11, n=4, labeller=function(x) 10^(x*10)/50)
p <- p + scale_fill_manual(values = c(K1="#191919",K2="#FFCC99",K3="#F0A3FF",K4="#808080",K5="#4C005C",K6="#993F00",K7="#0075DC",K8="#005C31",K9="#2BCE48"))
#p <- p + scale_color_brewer(palette="Paired")
p <- p + theme(legend.text = element_text(size = 12), legend.title = element_text(size = 14))
p <- p + theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14))
p <- p + theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10))
p <- p + labs(x="Longitude",y="Latitude",fill="Source")
p

# Focus on East Sepik

ES <- admixture2 %>% filter(Province%in%c("EAST_SEPIK"))

# Get hte map
map.world <- map_data(map="world")

# Visualization
# fix the limits with geo lon lat coordinates, according to the space you want to show by coord_quickmap
p <- ggplot()
#p <- p + geom_polygon(data=map.world, aes(x=long,y=lat,group=group), col="black", fill=grey(0.9))
#p <- p + coord_fixed(xlim=c(min(ES$Longitude, na.rm=T),max(ES$Longitude, na.rm=T)), ylim=c(min(ES$Latitude, na.rm=T),max(ES$Latitude, na.rm=T)))
#p <- p + geom_map(data=map.world[map.world$region=="Papua New Guinea",], map=map.world[map.world$region=="Papua New Guinea",], aes(map_id=region, x=long, y=lat), fill="white", colour="grey", size=0.15)
p <- p + geom_map(data=map.world, map=map.world, aes(map_id=region, x=long, y=lat), fill="white", colour="grey", size=0.15)
p <- p + coord_quickmap(xlim=c(min(ES$Longitude, na.rm=T),max(ES$Longitude, na.rm=T)), ylim=c(min(ES$Latitude, na.rm=T),max(ES$Latitude, na.rm=T))) 
p <- p + geom_scatterpie(data=ES, aes(x=Longitude, y=Latitude, r=log10(Size*25)/100), cols=c("K1", "K2", "K3", "K4", "K5", "K6", "K7", "K8", "K9"), alpha=.8)
p <- p + geom_scatterpie_legend(log10(admixture2$Size*25)/100, x=143.25, y=-4, n=3, labeller=function(x) 10^(x*100)/25)
p <- p + scale_fill_manual(values = c(K1="#191919",K2="#FFCC99",K3="#F0A3FF",K4="#808080",K5="#4C005C",K6="#993F00",K7="#0075DC",K8="#005C31",K9="#2BCE48"))
#p <- p + scale_color_brewer(palette="Paired")
p <- p + theme(legend.text = element_text(size = 12), legend.title = element_text(size = 14))
p <- p + theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14))
p <- p + theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10))
p <- p + labs(x="Longitude",y="Latitude",fill="Source")
p

#############################################################

## K=2


# Read admixture info
admixture <- read.table("/mnt/scratch/dang/New_Guinea/ADMIXTURE/map_view/K2.popind.Q", header=F)
colnames(admixture) <- c("Pop", "K1", "K2")
# average within pop and standarize by row
admixture2 <- admixture %>% group_by(Pop) %>%
  summarise_at(vars(K1,K2), funs(mean(.))) %>%
  distinct(Pop, .keep_all=T) %>% 
  mutate(row_sum=rowSums(select(., 2:3))) %>%
  mutate_at(2:3, ~ ./row_sum) %>%
  select(-row_sum) %>%
  left_join(info2)

# Remove 1KG samples
admixture2 <- admixture2 %>% filter(is.na(Latitude)==F)


# Incorporate sample size
size <- read.table("/mnt/scratch/dang/New_Guinea/ADMIXTURE/map_view/pop_ind.count", header=F)
colnames(size) <- c("Size","Pop")

admixture2 <- admixture2 %>% left_join(size)


# Get hte map
map.world <- map_data(map="world")

# Visualization
# fix the limits with geo lon lat coordinates, according to the space you want to show by coord_quickmap
p <- ggplot()
p <- p + theme()
p <- p + geom_map(data=map.world[map.world$region=="Papua New Guinea",], map=map.world[map.world$region=="Papua New Guinea",], aes(map_id=region, x=long, y=lat), fill="white", colour="grey", size=0.15)
#p <- p + geom_map(data=map.world, map=map.world, aes(map_id=region, x=long, y=lat), fill="white", colour="grey", size=0.15)
#p <- p + coord_quickmap(xlim=c(min(info$Longitude, na.rm=T),max(info$Longitude, na.rm=T)), ylim=c(min(info$Latitude, na.rm=T),max(info$Latitude, na.rm=T))) 
p <- p + geom_scatterpie(data=admixture2, aes(x=Longitude, y=Latitude, r=log10(Size*50)/10), cols=c("K1", "K2"), alpha=.8) + coord_fixed()
p <- p + geom_scatterpie_legend(log10(admixture2$Size*50)/10, x=142, y=-11, n=4, labeller=function(x) 10^(x*10)/50)
p <- p + scale_fill_manual(values = c(K1="#F0A3FF",K2="#0075DC"))
#p <- p + scale_color_brewer(palette="Paired")
p <- p + theme(legend.text = element_text(size = 12), legend.title = element_text(size = 14))
p <- p + theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14))
p <- p + theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10))
p <- p + labs(x="Longitude",y="Latitude",fill="Source")
p
