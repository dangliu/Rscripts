# title: "Outgroup f3 per population on GW data"
# author: "Dang Liu 04.Mar.2019"

# Last updated: 04.Mar.2019

# Use libraries
library(tidyverse)
library(RColorBrewer)
library(gtools)
library(ggmap)
library(maps)
library(ggrepel)

# load data from a huge f3 comparing matrix
load("/mnt/scratch/dang/Vietnam/outgroup_f3/HO.ancient.outgroup.Mbuti_f3.Rdata")
colnames(f3_res)[1:3] <- c("Pop", "Pop2", "Outgroup")
head(f3_res)
info <- read.table("/mnt/scratch/dang/Vietnam/outgroup/HO.ancient.outgroup.geo.info", header=T)

info2 <- info %>% group_by(Pop) %>% 
  summarise_at(vars(Latitude,Longitude), funs(median(.))) %>% 
  left_join(select(info,-(FID:IID),-(Latitude:Longitude))) %>%
  distinct(Pop, .keep_all = TRUE)

#info3 <- info2 %>% select(-Latitude, -Longitude)

#info2 <- info2 %>% select(Pop, Latitude, Longitude)
colnames(info2)[1] <- "Pop2"


# Prepare data set for annotation and order
d <- f3_res %>% left_join(info2[!info2$Pop2 %in% c("Mbuti","French"),]) %>% left_join(info3[!info3$Pop %in% c("Mbuti","French"),])
d$Country <- factor(d$Country, levels=c("Taiwan","China","Vietnam","Cambodia","Laos","Thailand","Myanmar","Malaysia","Indonesia","Philippines","India"), ordered=T)
d <- d[order(d$Country),]
# Exclude pops here
pop_exclude <- c("Atayal1","Ami1","Paiwan","Bunun","Rukai","Tao","Mamanwa1","Tianyuan")
d <- d[!d$Pop%in%pop_exclude,]
d <- d[!d$Pop2%in%pop_exclude,] 

# Add language groups for Vietnam only
d$Language <- "NA"
d <- mutate(d,Language = ifelse(Pop%in%c("BoY","CoLao","LaChi","Nung","Tay","Thai"),"Tai-Kadai",Language))
d <- mutate(d,Language = ifelse(Pop%in%c("Dao","Hmong","PaThen"),"Hmong-Mien",Language))
d <- mutate(d,Language = ifelse(Pop%in%c("Cham","Ede","Giarai"),"Austronesian",Language))
d <- mutate(d,Language = ifelse(Pop%in%c("Cong","HaNhi","LaHu","LoLo","PhuLa","Sila"),"Sino-Tibetan",Language))
d <- mutate(d,Language = ifelse(Pop%in%c("KhoMu","Kinh","Mang","Muong"),"Austro-Asiatic",Language))


# Subset data here
d2 <- d[d$Language!="NA",]
#d2 <- d[d$Language=="Tai-Kadai",]
#d2 <- d[d$Language=="Hmong-Mien",]
#d2 <- d[d$Language=="Austronesian",]
#d2 <- d[d$Language=="Sino-Tibetan",]
#d2 <- d[d$Language=="Austro-Asiatic",]

d2[d2$nsnps==-1,]$f3 <- NA
d2$Target <- ifelse(d2$Pop==d2$Pop2, "T", "F")

# Order by Period
d2$Period <- factor(d2$Period, levels=c("P","Hi","IA","BA","LN","N","Ho","Pa"), ordered=T)
d2 <- d2[order(d2$Period),]

# Order by Language groups
d2$Pop <- factor(d2$Pop, levels=c("BoY","CoLao","LaChi","Nung","Tay","Thai","Dao","Hmong","PaThen","Cham","Ede","Giarai","Cong","HaNhi","LaHu","LoLo","PhuLa","Sila","KhoMu","Kinh","Mang","Muong"), ordered=T)

# Normalization for min to max --> 0 to 1
d2$normalized_f3 <- "NA"
n <- 0
for(i in d2$Pop){
  n <- n + 1
  d2[n,]$normalized_f3 <- (d2[n,]$f3-min(d2[d2$Pop==i,]$f3, na.rm=T))/(max(d2[d2$Pop==i,]$f3, na.rm=T)-min(d2[d2$Pop==i,]$f3, na.rm=T))
}
d2$normalized_f3 <- as.numeric(as.character(d2$normalized_f3))

# Get hte map
map.world <- map_data(map="world")

# Visualization
# fix the limits with geo lon lat coordinates, according to the space you want to show by coord_quickmap
p <- ggplot()
p <- p + theme()
p <- p + geom_map(data=map.world, map=map.world, aes(map_id=region, x=long, y=lat), fill="white", colour="grey", size=0.15)
p <- p + coord_quickmap(ylim=c(min(d$Latitude),max(d$Latitude)), xlim=c(min(d$Longitude),max(d$Longitude))) 
p <- p + geom_point(data=d2, aes(x=Longitude, y=Latitude, color=normalized_f3, pch=Period), size=3) 
p <- p + scale_colour_gradientn(colours = c("#4575B4", "#FFEDA0", "#D73027"))
p <- p + scale_shape_manual(values=c(19,7,8,9,10,11,12,13))
p <- p + geom_point(data=d2[d2$Target=="T",], aes(x=Longitude, y=Latitude), pch=17, size=4, color="black")
#p <- p + facet_wrap(.~Pop, ncol=3)
p <- p + facet_wrap(.~Pop, nrow=4)

p <- p + theme(legend.text = element_text(size = 12), legend.title = element_text(size = 14))
p <- p + theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14))
p <- p + theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10))
p <- p + theme(strip.text.x = element_text(size = 12), strip.text.y = element_text(size=12))
p <- p + xlab("Longitude") + ylab("Latitude")
p

