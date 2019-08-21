# title: "Outgroup f3 per population on GW data"
# author: "Dang Liu 04.Mar.2019"

# Last updated: 21.Jun.2019

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
info <- read.table("/mnt/scratch/dang/Vietnam/map/all.map.am.geo.info2", header=T)

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

###################################################################################################################

# Visualization I: point with error bar per pop 

# Add language groups for Vietnam only
d$Language <- "NA"
d <- mutate(d,Language = ifelse(Pop%in%c("BoY","CoLao","LaChi","Nung","Tay","Thai"),"Tai-Kadai",Language))
d <- mutate(d,Language = ifelse(Pop%in%c("Dao","Hmong","PaThen"),"Hmong-Mien",Language))
d <- mutate(d,Language = ifelse(Pop%in%c("Cham","Ede","Giarai"),"Austronesian",Language))
d <- mutate(d,Language = ifelse(Pop%in%c("Cong","HaNhi","LaHu","LoLo","PhuLa","Sila"),"Sino-Tibetan",Language))
d <- mutate(d,Language = ifelse(Pop%in%c("KhoMu","Kinh","Mang","Muong"),"Austro-Asiatic",Language))

# Subset data here
# Only Vietnam modern
d2 <- d[d$Language!="NA",]
# By language group
#d2 <- d[d$Language=="Tai-Kadai",]
#d2 <- d[d$Language=="Hmong-Mien",]
#d2 <- d[d$Language=="Austronesian",]
#d2 <- d[d$Language=="Sino-Tibetan",]
#d2 <- d[d$Language=="Austro-Asiatic",]
# V vs. East and Southeast Asians
#d2 <- d[d$Country!="India"&d$Type=="modern"&d$Language!="NA"&!d$Pop2%in%c("BoY","CoLao","LaChi","Nung","Tay","Thai","Dao","Hmong","PaThen","Cham","Ede","Giarai","Cong","HaNhi","LaHu","LoLo","PhuLa","Sila","KhoMu","Kinh","Mang","Muong"),]
# V vs. Indian
#d2 <- d[d$Country=="India"|d$Type=="ancient",]

# Subset py Period
#d2 <- d[d$Pop_Period!="P",]


d2[d2$nsnps==-1,]$f3 <- NA
d2$Target <- ifelse(d2$Pop==d2$Pop2, "T", "F")

# Order by Period
d2$Period <- factor(d2$Period, levels=c("Present","Historical","Iron_Age","Bronze_Age","Neolithic","Hoabinhian","Paleolithic"), ordered=T)
d2 <- d2[order(d2$Period),]

# Order by Language groups
d2$Pop <- factor(d2$Pop, levels=c("BoY","CoLao","LaChi","Nung","Tay","Thai","Dao","Hmong","PaThen","Cham","Ede","Giarai","Cong","HaNhi","LaHu","LoLo","PhuLa","Sila","KhoMu","Kinh","Mang","Muong"), ordered=T)
#d2$Pop <- factor(d2$Pop, levels=c("Hon_Hai_Co_Tien","Kinabatagan","Supu_Hujung","Long_Long_Rak","Vat_Komnou","Nui_Nap","Loyang_Ujung","Mai_Da_Dieu","Nam_Tun","Oakaiel","Tam_Pa_Ping","Man_Bac","Tam_Hang","Gua_Cha","Pha_Faen","Tianyuan"), ordered=T)


# Order Pop2 by country
#d2$Pop2 <- factor(d2$Pop2, levels=c("Atayal","Hakka","Minnan","Pingpu","Ami","Han","Yi","Miao","Dai","Lahu","She","Naxi","Kinh","BoY","Cham","CoLao","Cong","Dao","Ede","Giarai","HaNhi","Hmong","KhoMu","LaChi","LaHu","LoLo","Mang","Muong","Nung","PaThen","PhuLa","Sila","Tay","Thai","Man_Bac","Nui_Nap","Hon_Hai_Co_Tien","Mai_Da_Dieu","Nam_Tun","Cambodian","Vat_Komnou","Tam_Pa_Ping","Pha_Faen","Tam_Hang","Thai1","Mlabri","Htin_Mal","Long_Long_Rak","Oakaiel","Supu_Hujung","Kinabatagan","Gua_Cha","Borneo","Semende","Loyang_Ujung","Mamanwa","Onge"))

## From point of View of Vietnam
# Extract maximum in each pop 
d2.max <- d2 %>% group_by(Pop) %>% summarise(MAX = max(f3, na.rm=T))

# With All
d2 %>% 
  ggplot(aes(x=Pop2, y=f3, ymin=f3-3*stderr, ymax=f3+3*stderr, color=Country, pch=Period)) + 
  #ggplot(aes(x=Pop2, y=f3, ymin=f3-3*stderr, ymax=f3+3*stderr, color=Country)) +
  geom_hline(data=d2.max, aes(yintercept=MAX), color="grey") +
  geom_point(size=2) + 
  geom_errorbar() +
  facet_wrap(~Pop, ncol=3) +
  #scale_color_brewer(palette="Paired") +
  scale_color_manual(values=c("#A6CEE3","#1F78B4","#B2DF8A","#33A02C","#FB9A99","#E31A1C","#FDBF6F","#FF7F00","#CAB2D6","#6A3D9A","#663300")) +
  #scale_color_manual(values=c("#1F78B4","#B2DF8A","#33A02C","#FB9A99","#E31A1C","#FDBF6F","#FF7F00","#CAB2D6","#663300")) +
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

#################################################################################################

# For within Vietnam
d$Language <- "NA"
d <- mutate(d,Language = ifelse(Pop2%in%c("BoY","CoLao","LaChi","Nung","Tay","Thai"),"Tai-Kadai",Language))
d <- mutate(d,Language = ifelse(Pop2%in%c("Dao","Hmong","PaThen"),"Hmong-Mien",Language))
d <- mutate(d,Language = ifelse(Pop2%in%c("Cham","Ede","Giarai"),"Austronesian",Language))
d <- mutate(d,Language = ifelse(Pop2%in%c("Cong","HaNhi","LaHu","LoLo","PhuLa","Sila"),"Sino-Tibetan",Language))
d <- mutate(d,Language = ifelse(Pop2%in%c("KhoMu","Kinh","Mang","Muong"),"Austro-Asiatic",Language))

d2 <- d[d$Type=="modern"&d$Country=="Vietnam"&d$Pop%in%c("BoY","CoLao","LaChi","Nung","Tay","Thai","Dao","Hmong","PaThen","Cham","Ede","Giarai","Cong","HaNhi","LaHu","LoLo","PhuLa","Sila","KhoMu","Kinh","Mang","Muong"),]
d2 <- d2[d2$Pop%in%c("BoY","CoLao","LaChi","Nung","Tay","Thai","Dao","Hmong","PaThen","Cham","Ede","Giarai","Cong","HaNhi","LaHu","LoLo","PhuLa","Sila","KhoMu","Kinh","Mang","Muong"),]

d2[d2$nsnps==-1,]$f3 <- NA
d2$Target <- ifelse(d2$Pop==d2$Pop2, "T", "F")

# Order by Language groups
d2$Pop <- factor(d2$Pop, levels=c("BoY","CoLao","LaChi","Nung","Tay","Thai","Dao","Hmong","PaThen","Cham","Ede","Giarai","Cong","HaNhi","LaHu","LoLo","PhuLa","Sila","KhoMu","Kinh","Mang","Muong"), ordered=T)
d2$Pop2 <- factor(d2$Pop2, levels=c("BoY","CoLao","LaChi","Nung","Tay","Thai","Dao","Hmong","PaThen","Cham","Ede","Giarai","Cong","HaNhi","LaHu","LoLo","PhuLa","Sila","KhoMu","Kinh","Mang","Muong"), ordered=T)

d2.max <- d2 %>% group_by(Pop) %>% summarise(MAX = max(f3, na.rm=T))

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


######################################################################################################
# Ancient/Other pop vs Vietnam only

d$Language <- "NA"
d <- mutate(d,Language = ifelse(Pop%in%c("BoY","CoLao","LaChi","Nung","Tay","Thai"),"Tai-Kadai",Language))
d <- mutate(d,Language = ifelse(Pop%in%c("Dao","Hmong","PaThen"),"Hmong-Mien",Language))
d <- mutate(d,Language = ifelse(Pop%in%c("Cham","Ede","Giarai"),"Austronesian",Language))
d <- mutate(d,Language = ifelse(Pop%in%c("Cong","HaNhi","LaHu","LoLo","PhuLa","Sila"),"Sino-Tibetan",Language))
d <- mutate(d,Language = ifelse(Pop%in%c("KhoMu","Kinh","Mang","Muong"),"Austro-Asiatic",Language))

# Ancient vs. V
#d2 <- d[d$Period!="P",]
#d2 <- d2[d2$Language!="NA",]

# ID vs. V
d2 <- d[d$Country=="India"&d$Type=="modern"&d$Language!="NA",]
# C vs. V
#d2 <- d[d$Country=="China"&d$Type=="modern"&d$Language!="NA",]
#d2 <- d[!d$Country%in%c("India","Taiwan","China","Vietnam")&d$Type=="modern"&d$Language!="NA",]


# Order by Language groups
d2$Pop <- factor(d2$Pop, levels=c("BoY","CoLao","LaChi","Nung","Tay","Thai","Dao","Hmong","PaThen","Cham","Ede","Giarai","Cong","HaNhi","LaHu","LoLo","PhuLa","Sila","KhoMu","Kinh","Mang","Muong"), ordered=T)
d2$Pop2 <- factor(d2$Pop2, levels=c("Hon_Hai_Co_Tien","Kinabatagan","Supu_Hujung","Long_Long_Rak","Vat_Komnou","Nui_Nap","Loyang_Ujung","Mai_Da_Dieu","Nam_Tun","Oakaiel","Tam_Pa_Ping","Tam_Hang","Gua_Cha_N","Man_Bac","Gua_Cha_H","Pha_Faen","Tianyuan"), ordered=T)

d2.max <- d2 %>% group_by(Pop2) %>% summarise(MAX = max(f3, na.rm=T))

## From point of view of ancient samples

# X tick 90 angle
d2 %>% 
  ggplot(aes(x=Pop, y=f3, ymin=f3-3*stderr, ymax=f3+3*stderr, color=Language)) + 
  geom_hline(data=d2.max, aes(yintercept=MAX), color="grey") +
  geom_point() + 
  geom_errorbar() +
  geom_hline(yintercept=0) +
  facet_wrap(~Pop2, ncol=3) +
  #scale_color_brewer(palette="Paired") +
  scale_color_manual(values=c("#9966CC","#CC6633","#FFCC33","#66CC99","#CC0033")) +
  scale_shape_manual(values=c(19,7,8,9,10,11,12,13)) +
  scale_x_discrete(d2$Pop) +
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


## From point of View of other countries

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
  #coord_cartesian(ylim=c(0.26,0.30)) +
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


# Add language groups for Vietnam only
d$Language <- "NA"
d <- mutate(d,Language = ifelse(Pop%in%c("BoY","CoLao","LaChi","Nung","Tay","Thai"),"Tai-Kadai",Language))
d <- mutate(d,Language = ifelse(Pop%in%c("Dao","Hmong","PaThen"),"Hmong-Mien",Language))
d <- mutate(d,Language = ifelse(Pop%in%c("Cham","Ede","Giarai"),"Austronesian",Language))
d <- mutate(d,Language = ifelse(Pop%in%c("Cong","HaNhi","LaHu","LoLo","PhuLa","Sila"),"Sino-Tibetan",Language))
d <- mutate(d,Language = ifelse(Pop%in%c("KhoMu","Kinh","Mang","Muong"),"Austro-Asiatic",Language))

# Subset data here
# Only Vietnam modern
#d2 <- d[d$Language!="NA",]
d2 <- d

# Deal with results which Pop=Pop2
# Set them with a Target tag
d2[d2$nsnps==-1,]$f3 <- NA
d2$Target <- ifelse(d2$Pop==d2$Pop2, "T", "F")

# Remove results among Vietnamese groups (To see relationship with neighboting pop only)
d2[d2$Target=="F"&d2$Country=="Vietnam",]$Country <- NA
d2 <- d2 %>% filter(is.na(Country)==F)

# Set Target Type to be ancient for visualization (To see relationship with ancietn samples only)
#d2[d2$Target=="T"&d2$Country=="Vietnam",]$Type <- "ancient"

# Subset 
# To see relationship with neighboting pops
#d2 <- d2 %>% filter(Type=="modern"&Language=="Austro-Asiatic")
d2 <- d2 %>% filter(Type=="modern"&Language!="NA")
# To see relationship with ancient samples
#d2 <- d2 %>% filter(Type=="ancient"&Language=="Austro-Asiatic")
#d2 <- d2 %>% filter(Type=="ancient"&Language=="Sino-Tibetan"&Pop2!="P-Tianyuan")
#d2 <- d2 %>% filter(Type=="ancient"&Pop2!="P-Tianyuan"&Language!="NA")
# To see relationship within Vietnam
#d2 <- d2 %>% filter(Type=="modern"&Country=="Vietnam"&Language=="Sino-Tibetan")
# To see relationship from the point of view of ancient samples
#d2[d2$Type=="ancient"&d2$Country=="Vietnam",]$Country <- "Thailand"
#d2[d2$Target=="T"&d2$Type=="ancient",]$Country <- "Vietnam"
#d2 <- d2 %>% filter(Pop%in%c("P-Tianyuan","Ho-Pha_Faen","Ho-Gua_Cha","N-Gua_Cha","N-Man_Bac","N-Nam_Tun","N-Mai_Da_Dieu","N-Hon_Hai_Co_Tien",
#                             "N-Tam_Pa_Ling","N-Tam_Hang","N-Oakaie","N-Loyang_Ujung","BA-Nui_Nap","IA-Vat_Komnou","IA-Long_Long_Rak","Hi-Hon_Hai_Co_Tien","Hi-Supu_Hujung","Hi-Kinabatagan")
#                    &Country=="Vietnam")

# Order by Period
d2$Period <- factor(d2$Period, levels=c("Present","Historical","Iron_Age","Bronze_Age","Neolithic","Hoabinhian","Paleolithic"), ordered=T)
d2 <- d2[order(d2$Period),]

# Order by Language groups
d2$Pop <- factor(d2$Pop, levels=c("BoY","CoLao","LaChi","Nung","Tay","Thai","Dao","Hmong","PaThen","Cham","Ede","Giarai","Cong","HaNhi","LaHu","LoLo","PhuLa","Sila","KhoMu","Kinh","Mang","Muong"), ordered=T)
#d2$Pop <- factor(d2$Pop, levels=c("P-Tianyuan","Ho-Pha_Faen","Ho-Gua_Cha","N-Gua_Cha","N-Man_Bac","N-Nam_Tun","N-Mai_Da_Dieu","N-Hon_Hai_Co_Tien","N-Tam_Pa_Ling","N-Tam_Hang","N-Oakaie","N-Loyang_Ujung","BA-Nui_Nap","IA-Vat_Komnou","IA-Long_Long_Rak","Hi-Hon_Hai_Co_Tien","Hi-Supu_Hujung","Hi-Kinabatagan"), ordered=T)

# Normalization for min to max --> 0 to 1
d2$normalized_f3 <- "NA"
n <- 0
for(i in d2$Pop){
  n <- n + 1
  d2[n,]$normalized_f3 <- (d2[n,]$f3-min(d2[d2$Pop==i,]$f3, na.rm=T))/(max(d2[d2$Pop==i,]$f3, na.rm=T)-min(d2[d2$Pop==i,]$f3, na.rm=T))
}
d2$normalized_f3 <- as.numeric(as.character(d2$normalized_f3))


# Adjust for the boundry of minimum
n <- 0
for(i in d2$Pop){
  n <- n + 1
  d2[n,]$normalized_f3 <- ifelse(d2[n,]$normalized_f3 < 0.75, 0.75, d2[n,]$normalized_f3)
}



# Get hte map
map.world <- map_data(map="world")

# fix the limits with geo lon lat coordinates, according to the space you want to show by coord_quickmap
p <- ggplot()
p <- p + theme()
p <- p + geom_map(data=map.world, map=map.world, aes(map_id=region, x=long, y=lat), fill="white", colour="grey", size=0.15)
p <- p + coord_quickmap(ylim=c(min(d2$Latitude),max(d2$Latitude)), xlim=c(min(d2$Longitude),max(d2$Longitude))) 
p <- p + geom_point(data=d2[is.na(d2$normalized_f3)==F,], aes(x=Longitude, y=Latitude, fill=normalized_f3), size=3, pch=21)
#jitter <- position_jitter(width = 0.5, height = 0.5) # Ancient only
#p <- p + geom_point(data=d2[is.na(d2$normalized_f3)==F,], aes(x=Longitude, y=Latitude, color=normalized_f3, pch=Period), size=3, stroke=1, position=jitter) # Ancient only
#jitter <- position_jitter(width = 0.2, height = 0.2)
#p <- p + geom_point(data=d2[is.na(d2$normalized_f3)==F,], aes(x=Longitude, y=Latitude, fill=normalized_f3), size=3, position=jitter, pch=21)
#p <- p + scale_color_gradientn(colours = c("#4575B4", "#FFEDA0", "#D73027"), labels=c("<= 0.75","0.80", "0.85", "0.90", "0.95", "1.00")) # Ancient only
p <- p + scale_fill_gradientn(colours = c("#4575B4", "#FFEDA0", "#D73027"), labels=c("<= 0.75","0.80", "0.85", "0.90", "0.95", "1.00"))
p <- p + scale_shape_manual(values=c("Present"=19,"Historical"=7,"Iron_Age"=8,"Bronze_Age"=9,"Neolithic"=10,"Hoabinhian"=11,"Paleolithic"=12))
p <- p + geom_point(data=d2[d2$Target=="T",], aes(x=Longitude, y=Latitude), pch=3, size=3, color="#000000", stroke=2)
#p <- p + facet_wrap(.~Pop, ncol=3)
p <- p + facet_wrap(.~Pop, nrow=4)
p <- p + geom_text_repel(
  data=d2[d2$normalized_f3>0.99&is.na(d2$normalized_f3)==F,], 
  aes(x=Longitude, y=Latitude,label=Pop2),
  size=4,
  point.padding=0.25,
  segment.alpha=0.5)
#p <- p + geom_text_repel(
#  data=d2[d2$normalized_f3>0.99&is.na(d2$normalized_f3)==F&d2$Longitude>=104,], 
#  aes(x=Longitude, y=Latitude,label=Pop2), 
#  nudge_x=110-d2[d2$normalized_f3>0.99&is.na(d2$normalized_f3)==F&d2$Longitude>=104,]$Longitude,
#  hjust=0,
#  size=4, 
#  segment.alpha=0.5,
#  direction="y")
#p <- p + geom_text_repel(
#  data=d2[d2$normalized_f3>0.99&is.na(d2$normalized_f3)==F&d2$Longitude<104,], 
#  aes(x=Longitude, y=Latitude,label=Pop2), 
#  nudge_x=85-d2[d2$normalized_f3>0.99&is.na(d2$normalized_f3)==F&d2$Longitude<104,]$Longitude,
#  hjust=0,
#  size=4, 
#  segment.alpha=0.5,
#  direction="y")
p <- p + theme(legend.text = element_text(size = 12), legend.title = element_text(size = 14))
p <- p + theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14))
p <- p + theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10))
p <- p + theme(strip.text.x = element_text(size = 12), strip.text.y = element_text(size=12))
#p <- p + labs(x="Longitude", y="Latitude", color=expression(paste("Normalized ", italic("f3"))))
p <- p + labs(x="Longitude", y="Latitude", fill=expression(paste("Normalized ", italic("f3"))))
p