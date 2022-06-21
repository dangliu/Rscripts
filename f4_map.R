# title: "f4 points on map"
# author: "Dang Liu 22.Feb.2022"

# Last updated: 07.Jun.2022

# Libraries
library(data.table)
library(RColorBrewer)
library(tidyverse)
library(ggmap)
library(maps)
library(ggrepel)
library(ggforce)
library(admixr)


# read data
#data <- eigenstrat('/mnt/scratch/dang/Taiwan/f4/HO.TW.ancient.filtered')
#info <- read_csv("/home/dang_liu/Projects/Taiwan/TW.meta.info.csv") %>% filter(QC=="PASS")

# read another data, separate Amis and Atayal from their Human Origins friends..
#data <- eigenstrat('/mnt/scratch/dang/Taiwan/f4/HO.TW.ancient.AmiAtaHO')
#info <- read_csv("/home/dang_liu/Projects/Taiwan/TW.meta.qpAdm.AmiAtaHO.info.csv") %>% filter(QC=="PASS")

# read another data, remove ADMIXTURE TW AN outliers
data <- eigenstrat('/mnt/scratch/dang/Taiwan/f4/HO.TW.ancient.noTW_AN_out')
info <- read_csv("/home/dang_liu/Projects/Taiwan/TW.meta.qpAdm.AmiAtaHO.info.csv") %>% filter(QC=="PASS" & PC_code<=250)

# pop
ind <- read_ind(data)
table(ind$label)
pops <- unique(ind$label)
pops

# Groups
Modern <- info %>% filter(Type=="modern") %>% select(Pop) %>% distinct(.keep_all=T) %>% pull(Pop)
Ancient <- info %>% filter(Type!="modern") %>% select(Pop) %>% distinct(.keep_all=T) %>% pull(Pop)

#Atayal.Rukai.modern groups.Mbuti
#result1 <- f4(W = "Atayal", X = "Rukai", Y = Modern[1:30], Z = "Mbuti", data = data)
#result2 <- f4(W = "Atayal", X = "Rukai", Y = Modern[31:60], Z = "Mbuti", data = data)
#result3 <- f4(W = "Atayal", X = "Rukai", Y = Modern[61:90], Z = "Mbuti", data = data)
#result4 <- f4(W = "Atayal", X = "Rukai", Y = Modern[91:120], Z = "Mbuti", data = data)
#result5 <- f4(W = "Atayal", X = "Rukai", Y = Modern[121:150], Z = "Mbuti", data = data)
#result6 <- f4(W = "Atayal", X = "Rukai", Y = Modern[151:160], Z = "Mbuti", data = data)

#result1 <- f4(W = "Atayal_HO", X = "Rukai", Y = Modern[1:30], Z = "Mbuti", data = data)
#result2 <- f4(W = "Atayal_HO", X = "Rukai", Y = Modern[31:60], Z = "Mbuti", data = data)
#result3 <- f4(W = "Atayal_HO", X = "Rukai", Y = Modern[61:90], Z = "Mbuti", data = data)
#result4 <- f4(W = "Atayal_HO", X = "Rukai", Y = Modern[91:120], Z = "Mbuti", data = data)
#result5 <- f4(W = "Atayal_HO", X = "Rukai", Y = Modern[121:150], Z = "Mbuti", data = data)
#result6 <- f4(W = "Atayal_HO", X = "Rukai", Y = Modern[151:160], Z = "Mbuti", data = data)


#result <- result1 %>% bind_rows(result2, result3, result4, result5, result6)
#save(result, file="/mnt/scratch/dang/Taiwan/f4/Ata.Ruk.Modern.Mbu.Rdata")
#save(result, file="/mnt/scratch/dang/Taiwan/f4/Ata.Ruk.Modern.Mbu.noHO.Rdata")
#save(result, file="/mnt/scratch/dang/Taiwan/f4/Ata_HO.Ruk.Modern.Mbu.Rdata")
#load("/mnt/scratch/dang/Taiwan/f4/Ata.Ruk.Modern.Mbu.Rdata")
load("/mnt/scratch/dang/Taiwan/f4/Ata.Ruk.Modern.Mbu.noHO.Rdata")
#load("/mnt/scratch/dang/Taiwan/f4/Ata_HO.Ruk.Modern.Mbu.Rdata")

#Amis.Rukai.modern groups.Mbuti
#result1 <- f4(W = "Amis", X = "Rukai", Y = Modern[1:30], Z = "Mbuti", data = data)
#result2 <- f4(W = "Amis", X = "Rukai", Y = Modern[31:60], Z = "Mbuti", data = data)
#result3 <- f4(W = "Amis", X = "Rukai", Y = Modern[61:90], Z = "Mbuti", data = data)
#result4 <- f4(W = "Amis", X = "Rukai", Y = Modern[91:120], Z = "Mbuti", data = data)
#result5 <- f4(W = "Amis", X = "Rukai", Y = Modern[121:150], Z = "Mbuti", data = data)
#result6 <- f4(W = "Amis", X = "Rukai", Y = Modern[151:160], Z = "Mbuti", data = data)

#result1 <- f4(W = "Amis_HO", X = "Rukai", Y = Modern[1:30], Z = "Mbuti", data = data)
#result2 <- f4(W = "Amis_HO", X = "Rukai", Y = Modern[31:60], Z = "Mbuti", data = data)
#result3 <- f4(W = "Amis_HO", X = "Rukai", Y = Modern[61:90], Z = "Mbuti", data = data)
#result4 <- f4(W = "Amis_HO", X = "Rukai", Y = Modern[91:120], Z = "Mbuti", data = data)
#result5 <- f4(W = "Amis_HO", X = "Rukai", Y = Modern[121:150], Z = "Mbuti", data = data)
#result6 <- f4(W = "Amis_HO", X = "Rukai", Y = Modern[151:160], Z = "Mbuti", data = data)


#result <- result1 %>% bind_rows(result2, result3, result4, result5, result6)
#save(result, file="/mnt/scratch/dang/Taiwan/f4/Ami.Ruk.Modern.Mbu.Rdata")
#save(result, file="/mnt/scratch/dang/Taiwan/f4/Ami.Ruk.Modern.Mbu.noHO.Rdata")
save(result, file="/mnt/scratch/dang/Taiwan/f4/Ami.Ruk.Modern.Mbu.noTWAN_out.Rdata")
#save(result, file="/mnt/scratch/dang/Taiwan/f4/Ami_HO.Ruk.Modern.Mbu.Rdata")
#load("/mnt/scratch/dang/Taiwan/f4/Ami.Ruk.Modern.Mbu.Rdata")
#load("/mnt/scratch/dang/Taiwan/f4/Ami.Ruk.Modern.Mbu.noHO.Rdata")
load("/mnt/scratch/dang/Taiwan/f4/Ami.Ruk.Modern.Mbu.noTWAN_out.Rdata")
#load("/mnt/scratch/dang/Taiwan/f4/Ami_HO.Ruk.Modern.Mbu.Rdata")


#Atayal.Amis.modern groups.Mbuti
#result1 <- f4(W = "Atayal", X = "Amis", Y = Modern[1:30], Z = "Mbuti", data = data)
#result2 <- f4(W = "Atayal", X = "Amis", Y = Modern[31:60], Z = "Mbuti", data = data)
#result3 <- f4(W = "Atayal", X = "Amis", Y = Modern[61:90], Z = "Mbuti", data = data)
#result4 <- f4(W = "Atayal", X = "Amis", Y = Modern[91:120], Z = "Mbuti", data = data)
#result5 <- f4(W = "Atayal", X = "Amis", Y = Modern[121:150], Z = "Mbuti", data = data)
#result6 <- f4(W = "Atayal", X = "Amis", Y = Modern[151:160], Z = "Mbuti", data = data)
#result <- result1 %>% bind_rows(result2, result3, result4, result5, result6)
#save(result, file="/mnt/scratch/dang/Taiwan/f4/Ata.Ami.Modern.Mbu.noHO.Rdata")
load("/mnt/scratch/dang/Taiwan/f4/Ata.Ami.Modern.Mbu.noHO.Rdata")


## Modern
# Remove samples failed QC
# Or further subset samples
info2 <- info %>% filter(is.na(Longitude)==FALSE) %>%
  filter(!Pop%in%c("French", "Mbuti")) %>%
  #filter(!Country%in%c("Thailand","Laos")) %>%
  filter(Type!="ancient")


# Use the median of sample coordinates
info2 <- info2 %>% group_by(Pop) %>% 
  summarise_at(vars(Latitude,Longitude), funs(median(.))) %>% 
  left_join(select(info,-(FID:IID),-(Latitude:Longitude))) %>%
  distinct(Pop, .keep_all = TRUE) %>% mutate(Longitude=ifelse(Pop=="Tongan", 170, Longitude))


info2 <- info2 %>% rename(Y="Pop") %>% left_join(result) %>% mutate(Zscore=ifelse(Zscore<(-5),-5,Zscore)) %>% mutate(Zscore=ifelse(Zscore>5,5,Zscore)) %>% drop_na()

# Get hte map
map.world <- map_data(map="world")
jitter <- position_jitter(width = 0.2, height = 0.2)
# Visualization
# fix the limits with geo lon lat coordinates, according to the space you want to show by coord_quickmap
info2 %>%
  ggplot() +
  geom_map(data=map.world, map=map.world, aes(map_id=region), fill="white", colour="grey", size=0.15) +
  coord_quickmap(xlim=c(min(info2$Longitude, na.rm=T),max(info2$Longitude, na.rm=T)), ylim=c(min(info2$Latitude, na.rm=T),max(info2$Latitude, na.rm=T))) +
  geom_point(data=info2, aes(x=Longitude, y=Latitude, fill=Zscore), size=4, position=jitter, shape=21) +
  scale_fill_gradientn(colours = c("#D73027","#FFFFFF","#4575B4"), labels=c("< -5", "-2", "0", "2", "> 5")) +
  #scale_fill_gradientn(colours = c("#D73027","#FFFFFF"), labels=c("< -5", "-2", "0"), breaks=c(-5, -2, 0)) +
  geom_text_repel(
    data=na.omit(info2[abs(info2$Zscore)>2,]),
    aes(x=Longitude, y=Latitude,label=Y,color=ifelse(Zscore>0,"#D73027","#4575B4")),
    show.legend=F,
    size=3,
    point.padding=0.25,
    segment.alpha=0.5,
    max.overlaps = 50) +
  theme() +
  theme(axis.line.x = element_line(color="black", size = 0.5, linetype = 1),
        axis.line.y = element_line(color="black", size = 0.5, linetype = 1)) +
  theme(legend.text = element_text(size = 14), legend.title = element_text(size = 16)) +
  #guides(fill=guide_legend(override.aes=list(shape=c(21)))) +
  theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16)) +
  theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14)) +
  labs(x="Longitude", y="Latitude")


info2 %>%
  ggplot() +
  geom_map(data=map.world, map=map.world, aes(map_id=region), fill="white", colour="grey", size=0.15) +
  coord_quickmap(xlim=c(min(120, na.rm=T),max(122, na.rm=T)), ylim=c(min(21.5, na.rm=T),max(25.5, na.rm=T))) +
  geom_point(data=info2, aes(x=Longitude, y=Latitude, fill=Zscore), size=4, shape=21) +
  scale_fill_gradientn(colours = c("#D73027","#FFFFFF","#4575B4"), labels=c("< -5", "-2", "0", "2", "> 5")) +
  #scale_fill_gradientn(colours = c("#D73027","#FFFFFF"), labels=c("< -5", "-2", "0"), breaks=c(-5, -2, 0)) +
  geom_text_repel(
    data=na.omit(info2[info2$Country=="Taiwan",]),
    aes(x=Longitude, y=Latitude,label=Y,color=ifelse(Zscore>0,"#D73027","#4575B4")),
    show.legend=F,
    size=3,
    point.padding=0.25,
    segment.alpha=0.5,
    max.overlaps = 30) +
  theme() +
  theme(axis.line.x = element_line(color="black", size = 0.5, linetype = 1),
        axis.line.y = element_line(color="black", size = 0.5, linetype = 1)) +
  theme(legend.text = element_text(size = 14), legend.title = element_text(size = 16)) +
  #guides(fill=guide_legend(override.aes=list(shape=c(21)))) +
  theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16)) +
  theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14)) +
  labs(x="Longitude", y="Latitude")



#############
## Ancient

#Atayal.Rukai.ancient groups.Mbuti
#result1 <- f4(W = "Atayal", X = "Rukai", Y = Ancient[1:30], Z = "Mbuti", data = data)
#result2 <- f4(W = "Atayal", X = "Rukai", Y = Ancient[31:60], Z = "Mbuti", data = data)
#result3 <- f4(W = "Atayal", X = "Rukai", Y = Ancient[61:88], Z = "Mbuti", data = data)
#result <- result1 %>% bind_rows(result2, result3)
#save(result, file="/mnt/scratch/dang/Taiwan/f4/Ata.Ruk.Ancient.Mbu.Rdata")
#load("/mnt/scratch/dang/Taiwan/f4/Ata.Ruk.Ancient.Mbu.Rdata")
#save(result, file="/mnt/scratch/dang/Taiwan/f4/Ata.Ruk.Ancient.Mbu.noHO.Rdata")
load("/mnt/scratch/dang/Taiwan/f4/Ata.Ruk.Ancient.Mbu.noHO.Rdata")


#Atayal.Rukai.ancient groups.French transversions only
#result1 <- data %>% transversions_only() %>% f4(W = "Atayal", X = "Rukai", Y = Ancient[1:30], Z = "French")
#result2 <- data %>% transversions_only() %>% f4(W = "Atayal", X = "Rukai", Y = Ancient[31:60], Z = "French")
#result3 <- data %>% transversions_only() %>% f4(W = "Atayal", X = "Rukai", Y = Ancient[61:88], Z = "French")
#result <- result1 %>% bind_rows(result2, result3)
#save(result, file="/mnt/scratch/dang/Taiwan/f4/Ata.Ruk.Ancient.Fre.tv.Rdata")
#load("/mnt/scratch/dang/Taiwan/f4/Ata.Ruk.Ancient.Fre.tv.Rdata")
#save(result, file="/mnt/scratch/dang/Taiwan/f4/Ata.Ruk.Ancient.Fre.tv.noHO.Rdata")
load("/mnt/scratch/dang/Taiwan/f4/Ata.Ruk.Ancient.Fre.tv.noHO.Rdata")

#Amis.Rukai.ancient groups.Mbuti
#result1 <- f4(W = "Amis", X = "Rukai", Y = Ancient[1:30], Z = "Mbuti", data = data)
#result2 <- f4(W = "Amis", X = "Rukai", Y = Ancient[31:60], Z = "Mbuti", data = data)
#result3 <- f4(W = "Amis", X = "Rukai", Y = Ancient[61:88], Z = "Mbuti", data = data)
#result <- result1 %>% bind_rows(result2, result3)
#save(result, file="/mnt/scratch/dang/Taiwan/f4/Ami.Ruk.Ancient.Mbu.Rdata")
#load("/mnt/scratch/dang/Taiwan/f4/Ami.Ruk.Ancient.Mbu.Rdata")
#save(result, file="/mnt/scratch/dang/Taiwan/f4/Ami.Ruk.Ancient.Mbu.noHO.Rdata")
#load("/mnt/scratch/dang/Taiwan/f4/Ami.Ruk.Ancient.Mbu.noHO.Rdata")
#save(result, file="/mnt/scratch/dang/Taiwan/f4/Ami.Ruk.Ancient.Mbu.noTWAN_out.Rdata")
load("/mnt/scratch/dang/Taiwan/f4/Ami.Ruk.Ancient.Mbu.noTWAN_out.Rdata")


#Amis.Rukai.ancient groups.French transversions only
#result1 <- data %>% transversions_only() %>% f4(W = "Amis", X = "Rukai", Y = Ancient[1:30], Z = "French")
#result2 <- data %>% transversions_only() %>% f4(W = "Amis", X = "Rukai", Y = Ancient[31:60], Z = "French")
#result3 <- data %>% transversions_only() %>% f4(W = "Amis", X = "Rukai", Y = Ancient[61:88], Z = "French")
#result <- result1 %>% bind_rows(result2, result3)
#save(result, file="/mnt/scratch/dang/Taiwan/f4/Ami.Ruk.Ancient.Fre.tv.Rdata")
#load("/mnt/scratch/dang/Taiwan/f4/Ami.Ruk.Ancient.Fre.tv.Rdata")
#save(result, file="/mnt/scratch/dang/Taiwan/f4/Ami.Ruk.Ancient.Fre.tv.noHO.Rdata")
load("/mnt/scratch/dang/Taiwan/f4/Ami.Ruk.Ancient.Fre.tv.noHO.Rdata")


#Atayal.Amis.ancient groups.Mbuti
#result1 <- f4(W = "Atayal", X = "Amis", Y = Ancient[1:30], Z = "Mbuti", data = data)
#result2 <- f4(W = "Atayal", X = "Amis", Y = Ancient[31:60], Z = "Mbuti", data = data)
#result3 <- f4(W = "Atayal", X = "Amis", Y = Ancient[61:88], Z = "Mbuti", data = data)
#result <- result1 %>% bind_rows(result2, result3)
#save(result, file="/mnt/scratch/dang/Taiwan/f4/Ata.Ami.Ancient.Mbu.noHO.Rdata")
load("/mnt/scratch/dang/Taiwan/f4/Ata.Ruk.Ancient.Mbu.noHO.Rdata")

#Atayal.Amis.ancient groups.French transversions only
#result1 <- data %>% transversions_only() %>% f4(W = "Atayal", X = "Amis", Y = Ancient[1:30], Z = "French")
#result2 <- data %>% transversions_only() %>% f4(W = "Atayal", X = "Amis", Y = Ancient[31:60], Z = "French")
#result3 <- data %>% transversions_only() %>% f4(W = "Atayal", X = "Amis", Y = Ancient[61:88], Z = "French")
#result <- result1 %>% bind_rows(result2, result3)
#save(result, file="/mnt/scratch/dang/Taiwan/f4/Ata.Ami.Ancient.Fre.tv.noHO.Rdata")
load("/mnt/scratch/dang/Taiwan/f4/Ata.Ruk.Ancient.Fre.tv.noHO.Rdata")










info3 <- info %>% filter(is.na(Longitude)==FALSE) %>% filter(Type=="ancient")

# Use the median of sample coordinates
info3 <- info3 %>% group_by(Pop) %>% 
  summarise_at(vars(Latitude,Longitude), funs(median(.))) %>% 
  left_join(select(info,-(FID:IID),-(Latitude:Longitude))) %>%
  distinct(Pop, .keep_all = TRUE) %>% 
  mutate(Longitude=ifelse(Pop=="Tonga_800BP", 170, Longitude)) %>% 
  mutate(Longitude=ifelse(Pop=="Tonga_2600BP", 170.5, Longitude)) %>%
  mutate(Longitude=ifelse(Pop=="French_Polynesia_200BP", 176, Longitude)) %>%
  mutate(Longitude=ifelse(Pop=="French_Polynesia_400BP", 176, Longitude))

info3 <- info3 %>% rename(Y="Pop") %>% left_join(result) %>% mutate(Zscore=ifelse(Zscore<(-5),-5,Zscore)) %>% mutate(Zscore=ifelse(Zscore>5,5,Zscore)) %>% drop_na()

# Get hte map
map.world <- map_data(map="world")
jitter <- position_jitter(width = 0.2, height = 0.2)
# Visualization
# fix the limits with geo lon lat coordinates, according to the space you want to show by coord_quickmap
info3 %>%
  ggplot() +
  geom_map(data=map.world, map=map.world, aes(map_id=region), fill="white", colour="grey", size=0.15) +
  coord_quickmap(xlim=c(min(info3$Longitude, na.rm=T),max(info3$Longitude, na.rm=T)), ylim=c(min(info3$Latitude, na.rm=T),max(info3$Latitude, na.rm=T))) +
  geom_point(aes(x=Longitude, y=Latitude, fill=Zscore), size=4, position=jitter, shape=21) +
  scale_fill_gradientn(colours = c("#D73027","#FFFFFF","#4575B4"), labels=c("< -5", "-2", "0", "2", "> 5"), breaks=c(-5, -2, 0, 2, 5)) +
  #scale_fill_gradientn(colours = c("#D73027","#FFFFFF"), labels=c("< -5", "-2", "0"), breaks=c(-5, -2, 0)) +
  geom_text_repel(
    data=na.omit(info3[abs(info3$Zscore)>2,]),
    aes(x=Longitude, y=Latitude,label=Y,color=ifelse(Zscore>0,"#D73027","#4575B4")),
    show.legend=F,
    size=3,
    point.padding=0.25,
    segment.alpha=0.5,
    max.overlaps = 30) +
  theme() +
  theme(axis.line.x = element_line(color="black", size = 0.5, linetype = 1),
        axis.line.y = element_line(color="black", size = 0.5, linetype = 1)) +
  theme(legend.text = element_text(size = 14), legend.title = element_text(size = 16)) +
  #guides(fill=guide_legend(override.aes=list(shape=c(21)))) +
  theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16)) +
  theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14)) +
  labs(x="Longitude", y="Latitude")


### biplots

data <- eigenstrat('/mnt/scratch/dang/Taiwan/f4/ArcChi/HO.TW.ancient.qpAdm.Archaic_Chimp')
info <- read_csv("/home/dang_liu/Projects/Taiwan/TW.meta.qpAdm.sepHO.Archaic_Chimp.info.csv") %>% filter(QC=="PASS")

# pop
ind <- read_ind(data)
table(ind$label)
pops <- unique(ind$label)
pops

# Groups
Modern <- info %>% filter(Type=="modern") %>% select(Pop) %>% distinct(.keep_all=T) %>% pull(Pop)
Ancient <- info %>% filter(Type!="modern") %>% select(Pop) %>% distinct(.keep_all=T) %>% pull(Pop)

#modern groups.Mbuti.Atayal.Chimp
result1 <- f4(W = "Atayal", X = "Rukai", Y = Modern[1:30], Z = "Mbuti", data = data)
result2 <- f4(W = "Atayal", X = "Rukai", Y = Modern[31:60], Z = "Mbuti", data = data)
result3 <- f4(W = "Atayal", X = "Rukai", Y = Modern[61:90], Z = "Mbuti", data = data)
result4 <- f4(W = "Atayal", X = "Rukai", Y = Modern[91:120], Z = "Mbuti", data = data)
result5 <- f4(W = "Atayal", X = "Rukai", Y = Modern[121:150], Z = "Mbuti", data = data)
result6 <- f4(W = "Atayal", X = "Rukai", Y = Modern[151:160], Z = "Mbuti", data = data)




d_x <- AtaRuk %>% select(Y, f4, stderr, Zscore) %>% rename("f4_x"=f4, "stderr_x"=stderr, "Zscore_x"=Zscore)
d_y <- AmiRuk %>% select(Y, f4, stderr, Zscore) %>% rename("f4_y"=f4, "stderr_y"=stderr, "Zscore_y"=Zscore)
info2 <- info %>% select(Pop, Region, Language) %>% distinct(.keep_all=T) %>% rename("Y"=Pop)
d <- d_x %>% left_join(d_y) %>% left_join(info2)

Region_col = c(nEA="#006633",sEA="#E31A1C",MSEA="#33A02C",ISEA="#FF7F00",Oceania="#1F78B4",SouthAsia="#B15928",Europe="#FB9A99",Africa="#FDBF6F",Tibet="#6A3D9A")
Language_col = c(Austronesian="#CC6633","Austroasiatic"="#9966CC","Hmong-Mien"="#FFCC33","Sino-Tibetan"="#66CC99","Tai-Kadai"="#CC0033","Trans-New_Guinea"="#FFFF99","East_New_Britain"="#BF5B17","Central_Solomons"="#666666","South_Bougainville"="#F0027F",
             "Indo-European"="#FFCCFF",Dravidian="#CC0099",Andamanese="#660033",Mongolic="#0000FF",Tungusic="#66CCFF",Turkic="#336600",Japonic="#FF0099","Central_Sudanic"="#BEAED4","N.A."="#000000")

#d$Y <- factor(d$Y, levels=c(PNG_L,Bismark,Solomon), ordered=T)

#d$Region <- factor(d$Region, levels=c("Collingwood_Bay", "Western_Massim", "Northern_Massim", "Southern_Massim", "Others"), ordered=T)

d %>% 
  filter(abs(Zscore_y)>2 & abs(Zscore_x)>2) %>%
  filter(!Y%in%c("Atayal","Atayal_HO","Amis","Amis_HO","Paiwan","Bunun")) %>%
  #mutate(Significant=as.factor(abs(Zscore_y)>3)) %>%
  ggplot() + 
  geom_smooth(aes(y=f4_y, x=f4_x), method='lm', formula=y~x, se=T, color="black", size=0.2) +
  geom_point(aes(y=f4_y, x=f4_x, color=Region), size=2.5, pch=1) +
  #geom_point(aes(y=f4_y, x=f4_x, color=Region, pch=Significant)) + 
  geom_errorbar(aes(y=f4_y, x=f4_x, ymin=f4_y-2*stderr_y, ymax=f4_y+2*stderr_y, color=Region)) +
  geom_errorbar(aes(y=f4_y, x=f4_x, xmin=f4_x-2*stderr_x, xmax=f4_x+2*stderr_x, color=Region)) +
  geom_text_repel(aes(y=f4_y, x=f4_x,label=Y,color=Region), size=3, segment.alpha=0.5, show.legend=F) +
  scale_color_manual(values = Region_col) +
  #scale_shape_manual(values = c("TRUE"=19, "FALSE"=4)) +
  theme(axis.line.x = element_line(color="black", size = 0.5, linetype = 1),
        axis.line.y = element_line(color="black", size = 0.5, linetype = 1)) +
  theme(panel.background = element_blank()) +
  #xlim(min(c(d$f4_x, d$f4_y)),max(c(d$f4_x, d$f4_y))) +
  #ylim(min(c(d$f4_x, d$f4_y)),max(c(d$f4_x, d$f4_y))) +
  #theme(legend.position = c(0.9, 0.1)) +
  labs(x="f4(Atayal, Rukai; Test Pop, Mbuti)", y="f4(Amis, Rukai; Test Pop, Mbuti)", color="Region") +
  theme(legend.text = element_text(size = 12), legend.title = element_text(size = 14)) +
  theme(axis.title.x = element_text(size = 14, face = "italic"), axis.title.y = element_text(size = 14, face = "italic")) + 
  theme(axis.text.x = element_text(size = 12, angle=45, vjust=0.5, hjust=0.5, margin = margin(t = 5)), axis.text.y = element_text(size = 12)) +
  theme(strip.text.x = element_text(size = 12), strip.text.y = element_text(size = 12))
