# Script to run qpAdm on TW data to test nEA/sEA ancestry
# Last updated: 08.Apr.2022

# Libraries
library(tidyverse)
library(admixr)
library(ggrepel)
#library(admixtools)
library(scatterpie)


# read data
#data <- eigenstrat('/mnt/scratch/dang/Taiwan/qpAdm/HO.TW.ancient.qpAdm')
#info <- read_csv("/home/dang_liu/Projects/Taiwan/qpAdm/TW.meta.qpAdm.info.csv") %>% filter(QC=="PASS")
# For separating HO Ami and Atayal
data <- eigenstrat('/mnt/scratch/dang/Taiwan/f4/HO.TW.ancient.AmiAtaHO')
info <- read_csv("/home/dang_liu/Projects/Taiwan/TW.meta.qpAdm.AmiAtaHO.info.csv") %>% filter(QC=="PASS")

# pop
ind <- read_ind(data)
table(ind$label)
pops <- unique(ind$label)
pops


# Groups
#TW <- c("Minnan", "Hakka",
#        "Pingpu",
#        "Atayal",
#        "Bunun",
#        "Rukai", "Paiwan",
#        "Amis", 
#        "Tao")
TW <- c("Minnan", "Hakka",
        "Pingpu",
        "Atayal", "Atayal_HO",
        "Bunun",
        "Rukai", "Paiwan",
        "Amis", "Amis_HO",
        "Tao")
TK <- c("Dong_Guizhou", "Dong_Hunan", "Gelao", "Li", "Maonan", "Mulam", "Zhuang")
Han <- c("Han_Fujian", "Han_Guangdong","Han_Shandong", "Han_Shanghai", "Han_Henan", "Han_Hubei")
nEA_N <- c("Xiaojingshan_7800BP", "Xiaogao_8700BP", "Wuzhuangguoliang_5000BP")
sEA_N <-c("Suogang_4550BP", "Xitoucun_4500BP", "Tanshishan_4350BP")
sEA_B <-c("Hanben_1550BP", "Shenxian_1300BP", "BaBanQinCen_1500BP")

# Settint up target, source, outgroup
#Target <- TW
Target <- c(TW, TK, Han, nEA_N, sEA_N, sEA_B)
Sources <- c("Boshan_8200BP", "Liangdao_7750BP")
#Outgroups <- c("Mbuti", "Afanasievo_5000BP", "New_Guinea_Highlander", "Tianyuan_40000BP", "Onge", "Mongolia_7300BP", "Sagly_2250BP", "Yumin_8400BP", "Jomon_3500BP", "Qihe_11550BP", "Longlin_10550BP", "Bianbian_9500BP", "ManBac_3800BP")
#Outgroups <- c("Mota_4470BP", "Ust_Ishim_44350BP", "Kostenki_38050BP", "Iran_10000BP", "Yana_31850BP", "Karelia_8450BP", "Okunevo_4300BP",
#               "Indus_Periphery_4500BP", "New_Guinea_Highlander", "Onge", "Upward_Sun_River_11400BP", "Tianyuan_40000BP", "PhaFaen_7000BP", "Kolyma_9750BP",
#               "Jomon_2800BP", "Qihe_11550BP", "Bianbian_9500BP")
Outgroups <- c("Mota_4470BP", "Ust_Ishim_44350BP", "Kostenki_38050BP", "Iran_10000BP", "Yana_31850BP", "Karelia_8450BP", "Okunevo_4300BP",
               "Indus_Periphery_4500BP", "New_Guinea_Highlander", "Onge", "Upward_Sun_River_11400BP", "Tianyuan_40000BP", "Longlin_10550BP", "Kolyma_9750BP",
               "Jomon_2800BP", "Qihe_11550BP", "Bianbian_9500BP")
#Outgroups <- c("Mota_4470BP", "Ust_Ishim_44350BP", "Kostenki_38050BP", "Yumin_8400BP", "Yana_31850BP", "Karelia_8450BP", "Okunevo_4300BP",
#               "Indus_Periphery_4500BP", "New_Guinea_Highlander", "Onge", "Upward_Sun_River_11400BP", "Tianyuan_40000BP", "Longlin_10550BP", "Kolyma_9750BP",
#               "Jomon_2800BP", "Qihe_11550BP", "Bianbian_9500BP")


# Map for the groups
data_map <- info %>% filter(Pop%in%c(Target, Sources, Outgroups)) %>% 
  group_by(Pop) %>% 
  summarise_at(vars(Latitude,Longitude), funs(median(.))) %>% 
  left_join(select(info,-(FID:IID),-(Latitude:Longitude))) %>%
  distinct(Pop, .keep_all = TRUE) %>% 
  mutate(Group=case_when(
    Pop %in% Target ~ "Target",
    Pop %in% Sources ~ "Source",
    Pop %in% Outgroups ~ "Outgroup"
  ))

# Get hte map
map.world <- map_data(map="world")
jitter <- position_jitter(width = 0.2, height = 0.2)
# Visualization
# fix the limits with geo lon lat coordinates, according to the space you want to show by coord_quickmap
data_map %>% filter(Group!="Target") %>% 
  ggplot() +
  geom_map(data=map.world, map=map.world, aes(map_id=region), fill="white", colour="grey", size=0.15) +
  coord_quickmap(xlim=c(min(data_map$Longitude, na.rm=T),max(data_map$Longitude, na.rm=T)), ylim=c(min(data_map$Latitude, na.rm=T),max(data_map$Latitude, na.rm=T))) +
  geom_point(aes(x=Longitude, y=Latitude, color=Group), pch=21, size=4, position=jitter, show.legend=TRUE) +
  theme() +
  theme(axis.line.x = element_line(color="black", size = 0.5, linetype = 1),
        axis.line.y = element_line(color="black", size = 0.5, linetype = 1)) +
  geom_text_repel(
    #data=data_map[data_map$Group!="Target",],
    aes(x=Longitude, y=Latitude,label=Pop,color=Group),
    show.legend=FALSE,
    size=5,
    point.padding=0.25,
    segment.alpha=0.5,
    max.overlaps=50) +
  theme(legend.text = element_text(size = 14), legend.title = element_text(size = 16)) +
  theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16)) +
  theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14)) +
  labs(x="Longitude", y="Latitude", label=NULL, fill="Group")




# Run qpAdm
result <- qpAdm(
  target = Target,
  sources = Sources,
  outgroups = Outgroups,
  data = data,
  params = list(inbreed = "YES")
)


# Save
#save(result, file="/mnt/scratch/dang/Taiwan/qpAdm/qpAdm.YangOG.Rdata")
#save(result, file="/mnt/scratch/dang/Taiwan/qpAdm/qpAdm.YangOG.Longlin.Rdata")
save(result, file="/mnt/scratch/dang/Taiwan/qpAdm/qpAdm.YangOG.Longlin.HOsep.Rdata")
#save(result, file="/mnt/scratch/dang/Taiwan/qpAdm/qpAdm.YangOG.Longlin.Yumin.Rdata")
#load("/mnt/scratch/dang/Taiwan/qpAdm/qpAdm.YangOG.Rdata")
#load("/mnt/scratch/dang/Taiwan/qpAdm/qpAdm.YangOG.Longlin.Rdata")
#load("/mnt/scratch/dang/Taiwan/qpAdm/qpAdm.YangOG.Longlin.HOsep.Rdata")
#load("/mnt/scratch/dang/Taiwan/qpAdm/qpAdm.YangOG.Longlin.Yumin.Rdata")



# Admixtools2
#result=qpadm(data="/mnt/scratch/dang/Taiwan/qpAdm/HO.TW.ancient.qpAdm", left = Sources, right = Outgroups, target = "Tanshishan_4350BP")




### Plotting
#info <- read.table("/mnt/scratch/dang/Taiwan/qpAdm/TW.meta.qpAdm.info", header=T)
#head(info)
# For separating HO Ami and Atayal
info <- read.table("/mnt/scratch/dang/Taiwan/TW.meta.qpAdm.AmiAtaHO.info", header=T)


# Remove BGV and sample failing QC
info <- info %>% filter(QC=="PASS" & is.na(Longitude)==F)

info2 <- info %>% group_by(Pop) %>% 
  summarise_at(vars(Latitude,Longitude), funs(median(.))) %>% 
  left_join(select(info,-(FID:IID),-(Latitude:Longitude))) %>%
  distinct(Pop, .keep_all = TRUE)

# Read admixture info
admixture <- result$proportions %>% filter(Boshan_8200BP>=0 & Liangdao_7750BP>=0 & pvalue>0.05 & stderr_Boshan_8200BP<0.25)
admixture_stderr <- admixture %>% select(target, stderr_Boshan_8200BP) %>% rename(stderr="stderr_Boshan_8200BP")
admixture <- admixture %>% select(target, Boshan_8200BP, Liangdao_7750BP) %>% pivot_longer(!target, names_to="Source", values_to="Prop") %>% left_join(admixture_stderr)
#admixture$target <-  factor(admixture$target, levels=TW, ordered=T)
admixture$target <-  factor(admixture$target, levels=c(TW, TK, Han, sEA_B, sEA_N, nEA_N), ordered=T)

# Visualization
admixture %>% ggplot() +
  geom_bar(aes(x=target, y=Prop, fill=Source), stat="identity") +
  geom_errorbar(data=admixture[admixture$Source=="Liangdao_7750BP",], aes(x=target, ymax=Prop+stderr, ymin=Prop-stderr), size=1, width=0.5, linetype="solid") + 
  scale_fill_manual(values = c(Boshan_8200BP="#0075DC", Liangdao_7750BP="#F0A3FF")) + 
  theme(axis.line.x = element_line(color="black", size = 0.5, linetype = 1),
        axis.line.y = element_line(color="black", size = 0.5, linetype = 1)) +
  theme(panel.background = element_blank()) +
  labs(x="Target", y="Proportion", fill="Source") +
  theme(legend.text = element_text(size = 14), legend.title = element_text(size = 16)) +
  theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16)) + 
  theme(axis.text.x = element_text(size = 14, angle=90, vjust=0.4, hjust=0.95), axis.text.y = element_text(size = 14))


# On a map
admixture <- result$proportions %>% filter(Boshan_8200BP>=0 & Liangdao_7750BP>=0 & pvalue>0.05 & stderr_Boshan_8200BP<0.25) %>% select(target, Boshan_8200BP, Liangdao_7750BP) %>% 
  add_row(target="Boshan_8200BP", Boshan_8200BP=1, Liangdao_7750BP=0) %>%
  add_row(target="Liangdao_7750BP", Boshan_8200BP=0, Liangdao_7750BP=1)

# Use the median of sample coordinates
info2 <- info %>% group_by(Pop) %>% 
  summarise_at(vars(Latitude,Longitude), funs(median(.))) %>% 
  left_join(select(info,-(FID:IID),-(Latitude:Longitude))) %>%
  distinct(Pop, .keep_all = TRUE) %>% mutate(Longitude=ifelse(Pop=="Tongan", 170, Longitude))


info2 <- info2 %>% rename(target="Pop") %>% select(target, Type, Longitude, Latitude) %>% left_join(admixture) %>% drop_na()

# Get hte map
map.world <- map_data(map="world")
jitter <- position_jitter(width = 0.2, height = 0.2)
# Visualization
# fix the limits with geo lon lat coordinates, according to the space you want to show by coord_quickmap
info2 %>%
  ggplot() +
  geom_map(data=map.world, map=map.world, aes(map_id=region), fill="white", colour="grey", size=0.15) +
  coord_quickmap(xlim=c(min(info2$Longitude, na.rm=T),max(info2$Longitude, na.rm=T)), ylim=c(min(info2$Latitude, na.rm=T),max(info2$Latitude, na.rm=T))) +
  geom_scatterpie(data=info2, aes(x=Longitude, y=Latitude, r=0.3), cols=c("Boshan_8200BP", "Liangdao_7750BP"), alpha=.8) +
  scale_fill_manual(values = c(Boshan_8200BP="#0075DC", Liangdao_7750BP="#F0A3FF")) +
  geom_text_repel(
    data=info2,
    aes(x=Longitude, y=Latitude,label=target),
    show.legend=F,
    size=3,
    nudge_x = 0.5,
    nudge_y = 0.5,
    point.padding=0.25,
    segment.alpha=0.5,
    max.overlaps = 50) +
  facet_wrap(.~Type) +
  theme() +
  theme(axis.line.x = element_line(color="black", size = 0.5, linetype = 1),
        axis.line.y = element_line(color="black", size = 0.5, linetype = 1)) +
  theme(legend.text = element_text(size = 14), legend.title = element_text(size = 16)) +
  #guides(fill=guide_legend(override.aes=list(shape=c(21)))) +
  theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16)) +
  theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14)) +
  theme(strip.text.x = element_text(size = 14), strip.text.y = element_text(size=14)) +
  labs(x="Longitude", y="Latitude", fill="Source")

info2 %>%
  ggplot() +
  geom_map(data=map.world, map=map.world, aes(map_id=region), fill="white", colour="grey", size=0.15) +
  coord_quickmap(xlim=c(min(120, na.rm=T),max(122, na.rm=T)), ylim=c(min(21.5, na.rm=T),max(25.5, na.rm=T))) +
  geom_scatterpie(data=info2, aes(x=Longitude, y=Latitude, r=0.1), cols=c("Boshan_8200BP", "Liangdao_7750BP"), alpha=.8) +
  scale_fill_manual(values = c(Boshan_8200BP="#0075DC", Liangdao_7750BP="#F0A3FF")) +
  geom_text_repel(
    data=na.omit(info2[info2$target%in%c(TW,"Hanben_1550BP"),]),
    aes(x=Longitude, y=Latitude,label=target),
    show.legend=F,
    size=3,
    nudge_x = 0.15,
    nudge_y = 0.15,
    point.padding=0.25,
    segment.alpha=0.5,
    max.overlaps = 50) +
  #facet_wrap(.~Type) +
  theme() +
  theme(axis.line.x = element_line(color="black", size = 0.5, linetype = 1),
        axis.line.y = element_line(color="black", size = 0.5, linetype = 1)) +
  theme(legend.text = element_text(size = 14), legend.title = element_text(size = 16)) +
  #guides(fill=guide_legend(override.aes=list(shape=c(21)))) +
  theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16)) +
  theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14)) +
  theme(strip.text.x = element_text(size = 14), strip.text.y = element_text(size=14)) +
  labs(x="Longitude", y="Latitude", fill="Source")

