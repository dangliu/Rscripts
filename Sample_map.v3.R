# title: "Sample points on map"
# author: "Dang Liu 12.Feb.2019"

# Last updated: 28.June.2019

# Libraries
library(data.table)
library(tidyverse)
library(ggmap)
library(maps)
library(ggrepel)

info <- read_csv("/r1/people/dang_liu/Projects/Vietnam/Vietnam.metadata.csv")
head(info)
#info$Language <- "NA"
#info <- mutate(info,Language = ifelse(Pop%in%c("BoY","CoLao","LaChi","Nung","Tay","Thai"),"Tai-Kadai",Language))
#info <- mutate(info,Language = ifelse(Pop%in%c("Dao","Hmong","PaThen"),"Hmong-Mien",Language))
#info <- mutate(info,Language = ifelse(Pop%in%c("Cham","Ede","Giarai"),"Austronesian",Language))
#info <- mutate(info,Language = ifelse(Pop%in%c("Cong","HaNhi","LaHu","LoLo","PhuLa","Sila"),"Sino-Tibetan",Language))
#info <- mutate(info,Language = ifelse(Pop%in%c("KhoMu","Kinh","Mang","Muong"),"Austro-Asiatic",Language))

info <- mutate(info,Platform = ifelse(Platform%in%c("HO","A"),"All",Platform))
info <- mutate(info,Platform = ifelse(Platform=="Affy6.0","Structure_only",Platform))

info2 <- info %>% group_by(Pop) %>% 
  summarise_at(vars(Latitude,Longitude), funs(median(.))) %>% 
  left_join(select(info,-(FID:IID),-(Latitude:Longitude))) %>%
  distinct(Pop, .keep_all = TRUE)

# Exclude outgroups
pop_exclude <- c("French", "Mbuti")
info2 <- info2[!info2$Pop%in%pop_exclude,]

# Order by Period
info2$Period <- factor(info2$Period, levels=c("Present","Historical","Iron_Age","Bronze_Age","Neolithic","Hoabinhian","Paleolithic"), ordered=T)

# Get hte map
map.world <- map_data(map="world")

# Visualization
# fix the limits with geo lon lat coordinates, according to the space you want to show by coord_quickmap
p <- ggplot()
p <- p + theme()
p <- p + geom_map(data=map.world, map=map.world, aes(map_id=region, x=long, y=lat), fill="white", colour="grey", size=0.15)
p <- p + coord_quickmap(ylim=c(min(info2$Latitude),max(info2$Latitude)), xlim=c(min(info2$Longitude),max(info2$Longitude)))
jitter <- position_jitter(width = 0.2, height = 0.2)
#p <- p + geom_point(data=info2[info2$Language=="NA",], aes(x=Longitude, y=Latitude, color=Period, pch=Platform), size=3, position=jitter) 
p <- p + geom_point(data=info2[!(info2$Type=="modern"&info2$Country=="Vietnam"),], aes(x=Longitude, y=Latitude, color=Period, pch=Platform), size=3, position=jitter) 
#p <- p + scale_color_brewer(palette="Accent")
p <- p + scale_color_manual(values=c("#7FC97F","#006633","#FF3300","#FF9933","#386CB0","#F0027F","#660000","#666666"))
p <- p + scale_shape_manual(values=c("All"=19,"Structure_only"=4))
p <- p + geom_text_repel(
  data=info2[info2$Type=="ancient" | info2$Country!="Vietnam",], 
  aes(x=Longitude, y=Latitude,label=Pop,color=Period),
  size=4,
  point.padding=0.25,
  segment.alpha=0.5)
p <- p + theme(legend.text = element_text(size = 12), legend.title = element_text(size = 14))
p <- p + theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14))
p <- p + theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10))
p <- p + xlab("Longitude") + ylab("Latitude")
p


# Colored by Language familiy
Language = c("Austronesian"="#CC6633","Austro-Asiatic"="#9966CC","Hmong-Mien"="#FFCC33","Sino-Tibetan"="#66CC99","Tai-Kadai"="#CC0033",
             "Indo-European"="#FFCCFF","Dravidian"="#CC0099","Andamanese"="#660033","Mongolic"="#0000FF","Tungusic"="#66CCFF","Ancient"="#000000")

p <- ggplot()
p <- p + theme()
p <- p + geom_map(data=map.world, map=map.world, aes(map_id=region, x=long, y=lat), fill="white", colour="grey", size=0.15)
p <- p + coord_quickmap(ylim=c(min(info2$Latitude),max(info2$Latitude)), xlim=c(min(info2$Longitude),max(info2$Longitude)))
jitter <- position_jitter(width = 0.2, height = 0.2)
p <- p + geom_point(data=info2[!(info2$Type=="modern"&info2$Country=="Vietnam"),], aes(x=Longitude, y=Latitude, color=Language, pch=Platform), size=3, position=jitter) 
#p <- p + scale_color_brewer(palette="Accent")
p <- p + scale_color_manual(values=Language)
p <- p + scale_shape_manual(values=c("All"=19,"Structure_only"=4))
p <- p + geom_text_repel(
  data=info2[info2$Type=="ancient" | info2$Country!="Vietnam",], 
  aes(x=Longitude, y=Latitude,label=Pop,color=Language),
  size=4,
  point.padding=0.25,
  segment.alpha=0.5)
p <- p + theme(legend.text = element_text(size = 12), legend.title = element_text(size = 14))
p <- p + theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14))
p <- p + theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10))
p <- p + xlab("Longitude") + ylab("Latitude")
p
