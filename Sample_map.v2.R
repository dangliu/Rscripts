# title: "Sample points on map"
# author: "Dang Liu 12.Feb.2019"

# Last updated: 01.Mar.2019

# Libraries
library(data.table)
library(tidyverse)
library(ggmap)
library(maps)
library(ggrepel)

info <- read.table("/mnt/genotyping/SNPChipData/Vietnam/dataset/Vietnam.am.geo.info", header=T)
head(info)
info$Language <- "Ancient"
info <- mutate(info,Language = ifelse(Pop%in%c("BoY","CoLao","LaChi","Nung","Tay","Thai"),"Tai-Kadai",Language))
info <- mutate(info,Language = ifelse(Pop%in%c("Dao","Hmong","PaThen"),"Hmong-Mien",Language))
info <- mutate(info,Language = ifelse(Pop%in%c("Cham","Ede","Giarai"),"Austronesian",Language))
info <- mutate(info,Language = ifelse(Pop%in%c("Cong","HaNhi","LaHu","LoLo","PhuLa","Sila"),"Sino-Tibetan",Language))
info <- mutate(info,Language = ifelse(Pop%in%c("KhoMu","Kinh","Mang","Muong"),"Austro-Asiatic",Language))

info2 <- info %>% group_by(Pop) %>% 
  summarise_at(vars(Latitude,Longitude), funs(median(.))) %>% 
  left_join(select(info,-(FID:IID),-(Latitude:Longitude))) %>%
  distinct(Pop, .keep_all = TRUE)

# Get hte map
map.world <- map_data(map="world")

# Visualization
# fix the limits with geo lon lat coordinates, according to the space you want to show by coord_quickmap
p <- ggplot()
p <- p + theme()
p <- p + geom_map(data=map.world, map=map.world, aes(map_id=region, x=long, y=lat), fill="white", colour="grey", size=0.15)
p <- p + coord_quickmap(ylim=c(min(info2$Latitude),max(info2$Latitude)), xlim=c(min(info2$Longitude),max(info2$Longitude))) 
p <- p + geom_point(data=info2, aes(x=Longitude, y=Latitude, color=Period), size=3) 
p <- p + scale_color_manual(values=c("#9966CC","#CC6633","#663300","#33A02C","#FFCC33","#3288BD","#FB9A99","#CCCC33","#FF7F00","#999999","#66CC99","#CC0033"))
p <- p + geom_text_repel(
  data=info2[info2$Type=="modern" & info2$Longitude>=104,], 
  aes(x=Longitude, y=Latitude,label=Pop,color=Language), 
  nudge_x=113-info2[info2$Type=="modern" & info2$Longitude>=104,]$Longitude,
  hjust=0,
  size=4, 
  segment.alpha=0.5,
  direction="y")
p <- p + geom_text_repel(
  data=info2[info2$Type=="modern" & info2$Longitude<104,], 
  aes(x=Longitude, y=Latitude,label=Pop,color=Language), 
  nudge_x=100-info2[info2$Type=="modern" & info2$Longitude<104,]$Longitude,
  hjust=1,
  size=4, 
  segment.alpha=0.5,
  direction="y")
p <- p + geom_text_repel(
  data=info2[info2$Type=="ancient",], 
  aes(x=Longitude, y=Latitude,label=Pop,color=Period),
  size=4,
  point.padding=0.25,
  segment.alpha=0.5)
p <- p + theme(legend.text = element_text(size = 12), legend.title = element_text(size = 14))
p <- p + theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14))
p <- p + theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10))
p <- p + xlab("Longitude") + ylab("Latitude")
p