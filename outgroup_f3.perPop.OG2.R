# title: "Outgroup f3 per population on GW data"
# author: "Dang Liu 04.Mar.2019"

# Last updated: 08.May.2019

# Use libraries
library(tidyverse)
library(RColorBrewer)
library(gtools)
library(ggmap)
library(maps)
library(ggrepel)

# load data from a huge f3 comparing matrix
load("/mnt/scratch/dang/Vietnam/outgroup_f3/HO.ancient.outgroup.v2.Mbuti_f3.Rdata")
colnames(f3_res)[1:3] <- c("Pop", "Pop2", "Outgroup")
head(f3_res)
info <- read.table("/mnt/scratch/dang/Vietnam/outgroup/HO.ancient.outgroup.v2.geo.info", header=T)

info2 <- info %>% group_by(Pop) %>% 
  summarise_at(vars(Latitude,Longitude), funs(median(.))) %>% 
  left_join(select(info,-(FID:IID),-(Latitude:Longitude))) %>%
  distinct(Pop, .keep_all = TRUE)

# Add Period info for pop1
info3 <- info2 %>% select(Pop, Period)

colnames(info2)[1] <- "Pop2"
colnames(info3) <- c("Pop", "Pop_Period")

# Prepare data set for annotation and order
# Add pop_order from pong admixture visualization order
pop_order <- read.table("/mnt/scratch/dang/Vietnam/admixture/outgroup.v2/pong_pop_order.txt", header=F)
d <- f3_res %>% left_join(info2[!info2$Pop2 %in% c("Mbuti","French"),]) %>% left_join(info3[!info3$Pop %in% c("Mbuti","French"),])
d$Pop2 <- factor(d$Pop2, levels=pop_order$V1, ordered=T)
d <- d[order(d$Pop2),]
d$Country <- factor(d$Country, levels=c("Taiwan","China","Vietnam","Cambodia","Laos","Thailand","Myanmar","Malaysia","Indonesia","Philippines","India"), ordered=T)
d <- d[order(d$Country),]
# Exclude pops here
#pop_exclude <- c("Atayal1","Ami1","Paiwan","Bunun","Rukai","Tao","Mamanwa1","GujaratiA","GujaratiB","GujaratiC","GujaratiD","Jew_Cochin")
pop_exclude <- c("Atayal1","Ami1","Mamanwa1")
d <- d[!d$Pop%in%pop_exclude,]
d <- d[!d$Pop2%in%pop_exclude,] 

# Exclude ancient samples if too few snps
#d <- d[!d$Type%in%c("ancient"),]

# Add language groups for Vietnam only
d$Language <- "NA"
d <- mutate(d,Language = ifelse(Pop%in%c("BoY","CoLao","LaChi","Nung","Tay","Thai"),"Tai-Kadai",Language))
d <- mutate(d,Language = ifelse(Pop%in%c("Dao","Hmong","PaThen"),"Hmong-Mien",Language))
d <- mutate(d,Language = ifelse(Pop%in%c("Cham","Ede","Giarai"),"Austronesian",Language))
d <- mutate(d,Language = ifelse(Pop%in%c("Cong","HaNhi","LaHu","LoLo","PhuLa","Sila"),"Sino-Tibetan",Language))
d <- mutate(d,Language = ifelse(Pop%in%c("KhoMu","Kinh","Mang","Muong"),"Austro-Asiatic",Language))

# For within Vietnam
#d$Language <- "NA"
#d <- mutate(d,Language = ifelse(Pop2%in%c("BoY","CoLao","LaChi","Nung","Tay","Thai"),"Tai-Kadai",Language))
#d <- mutate(d,Language = ifelse(Pop2%in%c("Dao","Hmong","PaThen"),"Hmong-Mien",Language))
#d <- mutate(d,Language = ifelse(Pop2%in%c("Cham","Ede","Giarai"),"Austronesian",Language))
#d <- mutate(d,Language = ifelse(Pop2%in%c("Cong","HaNhi","LaHu","LoLo","PhuLa","Sila"),"Sino-Tibetan",Language))
#d <- mutate(d,Language = ifelse(Pop2%in%c("KhoMu","Kinh","Mang","Muong"),"Austro-Asiatic",Language))



# Subset data here
#d2 <- d[d$Language!="NA",]
#d2 <- d[d$Language=="Tai-Kadai",]
#d2 <- d[d$Language=="Hmong-Mien",]
#d2 <- d[d$Language=="Austronesian",]
#d2 <- d[d$Language=="Sino-Tibetan",]
#d2 <- d[d$Language=="Austro-Asiatic",]
#d2 <- d[d$Type=="modern"&d$Country=="Vietnam"&d$Pop%in%c("BoY","CoLao","LaChi","Nung","Tay","Thai","Dao","Hmong","PaThen","Cham","Ede","Giarai","Cong","HaNhi","LaHu","LoLo","PhuLa","Sila","KhoMu","Kinh","Mang","Muong"),]
#d2 <- d[d$Country!="India"&d$Type=="modern"&d$Language!="NA"&!d$Pop2%in%c("BoY","CoLao","LaChi","Nung","Tay","Thai","Dao","Hmong","PaThen","Cham","Ede","Giarai","Cong","HaNhi","LaHu","LoLo","PhuLa","Sila","KhoMu","Kinh","Mang","Muong"),]
#d2 <- d[d$Country=="India"|d$Type=="ancient",]
#d2 <- d2[d2$Pop%in%c("BoY","CoLao","LaChi","Nung","Tay","Thai","Dao","Hmong","PaThen","Cham","Ede","Giarai","Cong","HaNhi","LaHu","LoLo","PhuLa","Sila","KhoMu","Kinh","Mang","Muong"),]
#d2 <- d[d$Country=="India"&d$Type=="modern"&d$Language!="NA",]
#d2 <- d[d$Country=="China"&d$Type=="modern"&d$Language!="NA",]
d2 <- d[!d$Country%in%c("India","Taiwan","China","Vietnam")&d$Type=="modern"&d$Language!="NA",]

# Subset py Period
#d2 <- d[d$Pop_Period!="P",]


d2[d2$nsnps==-1,]$f3 <- NA
d2$Target <- ifelse(d2$Pop==d2$Pop2, "T", "F")

# Order by Period
d2$Period <- factor(d2$Period, levels=c("P","Hi","IA","BA","LN","N","Ho","Pa"), ordered=T)
d2 <- d2[order(d2$Period),]

# Order by Language groups
d2$Pop <- factor(d2$Pop, levels=c("BoY","CoLao","LaChi","Nung","Tay","Thai","Dao","Hmong","PaThen","Cham","Ede","Giarai","Cong","HaNhi","LaHu","LoLo","PhuLa","Sila","KhoMu","Kinh","Mang","Muong"), ordered=T)
#d2$Pop <- factor(d2$Pop, levels=c("Hon_Hai_Co_Tien","Kinabatagan","Supu_Hujung","Long_Long_Rak","Vat_Komnou","Nui_Nap","Loyang_Ujung","Mai_Da_Dieu","Nam_Tun","Oakaiel","Tam_Pa_Ping","Man_Bac","Tam_Hang","Gua_Cha","Pha_Faen","Tianyuan"), ordered=T)
d2$Pop2 <- factor(d2$Pop2, levels=c("BoY","CoLao","LaChi","Nung","Tay","Thai","Dao","Hmong","PaThen","Cham","Ede","Giarai","Cong","HaNhi","LaHu","LoLo","PhuLa","Sila","KhoMu","Kinh","Mang","Muong"), ordered=T)
#########################################################3

# Visualization I: point with error bar per pop 

# Order Pop2 by country
#d2$Pop2 <- factor(d2$Pop2, levels=c("Atayal","Hakka","Minnan","Pingpu","Ami","Han","Yi","Miao","Dai","Lahu","She","Naxi","Kinh","BoY","Cham","CoLao","Cong","Dao","Ede","Giarai","HaNhi","Hmong","KhoMu","LaChi","LaHu","LoLo","Mang","Muong","Nung","PaThen","PhuLa","Sila","Tay","Thai","Man_Bac","Nui_Nap","Hon_Hai_Co_Tien","Mai_Da_Dieu","Nam_Tun","Cambodian","Vat_Komnou","Tam_Pa_Ping","Pha_Faen","Tam_Hang","Thai1","Mlabri","Htin_Mal","Long_Long_Rak","Oakaiel","Supu_Hujung","Kinabatagan","Gua_Cha","Borneo","Semende","Loyang_Ujung","Mamanwa","Onge"))

# X tick 90 angle

## From point of View of other countries

d2.max <- d2 %>% group_by(Pop2) %>% summarise(MAX = max(f3, na.rm=T))

d2 %>% 
  ggplot(aes(x=Pop, y=f3, ymin=f3-3*stderr, ymax=f3+3*stderr, color=Language)) + 
  geom_hline(data=d2.max, aes(yintercept=MAX), color="grey") +
  geom_point() + 
  geom_errorbar() +
  #geom_hline(yintercept=0) +
  facet_wrap(~Pop2, ncol=3) +
  #scale_color_brewer(palette="Paired") +
  scale_color_manual(values=c("#9966CC","#CC6633","#FFCC33","#66CC99","#CC0033")) +
  #scale_shape_manual(values=c(19,7,8,9,10,11,12,13)) +
  scale_x_discrete(d2$Pop2) +
  coord_cartesian(ylim=c(0.26,0.30)) +
  #coord_cartesian(ylim=c(0.05,0.15)) +
  #coord_cartesian(ylim=c(0.175,0.325)) +
  #labs(x=NULL) +
  theme(axis.line.x = element_line(color="black", size = 0.5, linetype = 1),
        axis.line.y = element_line(color="black", size = 0.5, linetype = 1)) +
  theme(panel.background = element_blank()) +
  theme(legend.text = element_text(size = 12), legend.title = element_text(size = 12)) +
  theme(axis.title.x = element_blank(), axis.title.y = element_text(size = 14)) + 
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12)) +
  theme(strip.text.x = element_text(size = 12), strip.text.y = element_text(size = 12)) +
  theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1)) 

## From point of View of Vietnam
# Extract maximum in each pop 
d2.max <- d2 %>% group_by(Pop) %>% summarise(MAX = max(f3, na.rm=T))

# With Ancient and Indians
d2 %>% 
  ggplot(aes(x=Pop2, y=f3, ymin=f3-3*stderr, ymax=f3+3*stderr, color=Country, pch=Period)) + 
  #ggplot(aes(x=Pop2, y=f3, ymin=f3-3*stderr, ymax=f3+3*stderr, color=Country)) +
  geom_hline(data=d2.max, aes(yintercept=MAX), color="grey") +
  geom_point(size=2) + 
  geom_errorbar() +
  facet_wrap(~Pop, ncol=3) +
  #scale_color_brewer(palette="Paired") +
  #scale_color_manual(values=c("#A6CEE3","#1F78B4","#B2DF8A","#33A02C","#FB9A99","#E31A1C","#FDBF6F","#FF7F00","#CAB2D6","#6A3D9A","#663300")) +
  scale_color_manual(values=c("#1F78B4","#B2DF8A","#33A02C","#FB9A99","#E31A1C","#FDBF6F","#FF7F00","#CAB2D6","#663300")) +
  scale_shape_manual(values=c(19,7,8,9,10,11,12,13)) +
  #scale_x_discrete(limits=unique(d$Pop2)) +
  #scale_y_continuous(breaks=c(0.26,0.28,0.30)) +
  #coord_cartesian(ylim=c(0.2,0.325)) +
  coord_cartesian(ylim=c(0.2,0.3)) +
  #coord_cartesian(ylim=c(0.05,0.15)) +
  #coord_cartesian(ylim=c(0.175,0.325)) +
  labs(x=NULL) +
  theme(axis.line.x = element_line(color="black", size = 0.5, linetype = 1),
        axis.line.y = element_line(color="black", size = 0.5, linetype = 1)) +
  theme(panel.background = element_blank()) +
  theme(legend.text = element_text(size = 12), legend.title = element_text(size = 12)) +
  theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14)) + 
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12)) +
  theme(strip.text.x = element_text(size = 12), strip.text.y = element_text(size = 12)) +
  theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1))

# With EastAsians
d2 %>% 
  #ggplot(aes(x=Pop2, y=f3, ymin=f3-3*stderr, ymax=f3+3*stderr, color=Country, pch=Period)) + 
  ggplot(aes(x=Pop2, y=f3, ymin=f3-3*stderr, ymax=f3+3*stderr, color=Country)) +
  geom_hline(data=d2.max, aes(yintercept=MAX), color="grey") +
  geom_point() + 
  geom_errorbar() +
  facet_wrap(~Pop, ncol=3) +
  #scale_color_brewer(palette="Paired") +
  #scale_color_manual(values=c("#A6CEE3","#1F78B4","#B2DF8A","#33A02C","#FB9A99","#E31A1C","#FDBF6F","#FF7F00","#CAB2D6","#6A3D9A","#663300")) +
  scale_color_manual(values=c("#A6CEE3","#1F78B4","#33A02C","#E31A1C","#CAB2D6","#6A3D9A","#663300")) +
  #scale_shape_manual(values=c(19,7,8,9,10,11,12,13)) +
  #scale_x_discrete(limits=unique(d$Pop2)) +
  scale_y_continuous(breaks=c(0.26,0.28,0.30)) +
  #coord_cartesian(ylim=c(0.2,0.325)) +
  coord_cartesian(ylim=c(0.26,0.305)) +
  #coord_cartesian(ylim=c(0.05,0.15)) +
  #coord_cartesian(ylim=c(0.175,0.325)) +
  labs(x=NULL) +
  theme(axis.line.x = element_line(color="black", size = 0.5, linetype = 1),
        axis.line.y = element_line(color="black", size = 0.5, linetype = 1)) +
  theme(panel.background = element_blank()) +
  theme(legend.text = element_text(size = 12), legend.title = element_text(size = 12)) +
  theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14)) + 
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12)) +
  theme(strip.text.x = element_text(size = 12), strip.text.y = element_text(size = 12)) +
  theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1))

# Within Vietnam
d2 %>% 
  ggplot(aes(x=Pop2, y=f3, ymin=f3-3*stderr, ymax=f3+3*stderr, color=Language)) + 
  geom_hline(data=d2.max, aes(yintercept=MAX), color="grey") +
  geom_point() + 
  geom_errorbar() +
  geom_hline(yintercept=0) +
  facet_wrap(~Pop, ncol=3) +
  #scale_color_brewer(palette="Paired") +
  scale_color_manual(values=c("#9966CC","#CC6633","#FFCC33","#66CC99","#CC0033")) +
  scale_shape_manual(values=c(19,7,8,9,10,11,12,13)) +
  scale_x_discrete(d2$Pop2) +
  coord_cartesian(ylim=c(0.27,0.31)) +
  #coord_cartesian(ylim=c(0.05,0.15)) +
  #coord_cartesian(ylim=c(0.175,0.325)) +
  #labs(x=NULL) +
  theme(axis.line.x = element_line(color="black", size = 0.5, linetype = 1),
        axis.line.y = element_line(color="black", size = 0.5, linetype = 1)) +
  theme(panel.background = element_blank()) +
  theme(legend.text = element_text(size = 12), legend.title = element_text(size = 12)) +
  theme(axis.title.x = element_blank(), axis.title.y = element_text(size = 14)) + 
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12)) +
  theme(strip.text.x = element_text(size = 12), strip.text.y = element_text(size = 12)) +
  theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1)) 

###########################################################################################3

# Visualization II: point with gradient color on a map 

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

# fix the limits with geo lon lat coordinates, according to the space you want to show by coord_quickmap
p <- ggplot()
p <- p + theme()
p <- p + geom_map(data=map.world, map=map.world, aes(map_id=region, x=long, y=lat), fill="white", colour="grey", size=0.15)
p <- p + coord_quickmap(ylim=c(min(d$Latitude),max(d$Latitude)), xlim=c(min(d$Longitude),max(d$Longitude))) 
p <- p + geom_point(data=d2, aes(x=Longitude, y=Latitude, color=normalized_f3, pch=Period), size=3) 
p <- p + scale_colour_gradientn(colours = c("#4575B4", "#FFEDA0", "#D73027"))
p <- p + scale_shape_manual(values=c(19,7,8,9,10,11,12,13))
p <- p + geom_point(data=d2[d2$Target=="T",], aes(x=Longitude, y=Latitude), pch=17, size=4, color="black")
# <- p + facet_wrap(.~Pop, ncol=3)
p <- p + facet_wrap(.~Pop, nrow=4)

p <- p + theme(legend.text = element_text(size = 12), legend.title = element_text(size = 14))
p <- p + theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14))
p <- p + theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10))
p <- p + theme(strip.text.x = element_text(size = 12), strip.text.y = element_text(size=12))
p <- p + xlab("Longitude") + ylab("Latitude")
p

######################################################################################################
# Ancient vs Vietnam only

d$Language <- "NA"
d <- mutate(d,Language = ifelse(Pop2%in%c("BoY","CoLao","LaChi","Nung","Tay","Thai"),"Tai-Kadai",Language))
d <- mutate(d,Language = ifelse(Pop2%in%c("Dao","Hmong","PaThen"),"Hmong-Mien",Language))
d <- mutate(d,Language = ifelse(Pop2%in%c("Cham","Ede","Giarai"),"Austronesian",Language))
d <- mutate(d,Language = ifelse(Pop2%in%c("Cong","HaNhi","LaHu","LoLo","PhuLa","Sila"),"Sino-Tibetan",Language))
d <- mutate(d,Language = ifelse(Pop2%in%c("KhoMu","Kinh","Mang","Muong"),"Austro-Asiatic",Language))

d2 <- d[d$Pop_Period!="P",]
d2 <- d2[d2$Language!="NA",]
# Order by Language groups
d2$Pop2 <- factor(d2$Pop2, levels=c("BoY","CoLao","LaChi","Nung","Tay","Thai","Dao","Hmong","PaThen","Cham","Ede","Giarai","Cong","HaNhi","LaHu","LoLo","PhuLa","Sila","KhoMu","Kinh","Mang","Muong"), ordered=T)
d2$Pop <- factor(d2$Pop, levels=c("Hon_Hai_Co_Tien","Kinabatagan","Supu_Hujung","Long_Long_Rak","Vat_Komnou","Nui_Nap","Loyang_Ujung","Mai_Da_Dieu","Nam_Tun","Oakaiel","Tam_Pa_Ping","Tam_Hang","Gua_Cha_N","Man_Bac","Gua_Cha_H","Pha_Faen","Tianyuan"), ordered=T)


# X tick 90 angle
d2 %>% 
  ggplot(aes(x=Pop2, y=f3, ymin=f3-3*stderr, ymax=f3+3*stderr, color=Language, pch=Period)) + 
  geom_point() + 
  geom_errorbar() +
  geom_hline(yintercept=0) +
  facet_wrap(~Pop, ncol=3) +
  #scale_color_brewer(palette="Paired") +
  scale_color_manual(values=c("#9966CC","#CC6633","#FFCC33","#66CC99","#CC0033")) +
  scale_shape_manual(values=c(19,7,8,9,10,11,12,13)) +
  scale_x_discrete(d2$Pop2) +
  coord_cartesian(ylim=c(0.2,0.3)) +
  #coord_cartesian(ylim=c(0.05,0.15)) +
  #coord_cartesian(ylim=c(0.175,0.325)) +
  labs(x=NULL) +
  theme(axis.line.x = element_line(color="black", size = 0.5, linetype = 1),
        axis.line.y = element_line(color="black", size = 0.5, linetype = 1)) +
  theme(panel.background = element_blank()) +
  theme(legend.text = element_text(size = 12), legend.title = element_text(size = 12)) +
  theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14)) + 
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12)) +
  theme(strip.text.x = element_text(size = 12), strip.text.y = element_text(size = 12)) +
  theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1))
