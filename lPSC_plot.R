# author: "Dang Liu 08.Apr.2019"

# Last updated: 03.Jul.2019

# Use libraries
library(tidyverse)
library(RColorBrewer)
library(gtools)
library(ggmap)
library(maps)
library(ggrepel)
library(pheatmap)



# Read between data
data <- read.table("/mnt/scratch/dang/Vietnam/IBD/SHAPEIT_ref_m2/all.lPSC.tag.stats", header=T)
head(data)

# Pop info
info <- read.table("/mnt/scratch/dang/Vietnam/outgroup/HO.ancient.outgroup.v2.geo.info", header=T)
info2 <- info %>% group_by(Pop) %>% 
  summarise_at(vars(Latitude,Longitude), list(~median(.))) %>% 
  left_join(select(info,-(FID:IID),-(Latitude:Longitude))) %>%
  distinct(Pop, .keep_all = TRUE)
colnames(info2)[1] <- "Pop2"

# Combine the tables
d <- data %>% left_join(info2)

# Exclude pops here
pop_exclude <- c("Atayal1","Ami1","Paiwan","Bunun","Rukai","Tao","Mamanwa1")
d <- d[!d$Pop1%in%pop_exclude,]
d <- d[!d$Pop2%in%pop_exclude,]
head(d)

# Add language groups for Vietnam only
d$Language <- "NA"
d <- mutate(d,Language = ifelse(Pop1%in%c("BoY","CoLao","LaChi","Nung","Tay","Thai"),"Tai-Kadai",Language))
d <- mutate(d,Language = ifelse(Pop1%in%c("Dao","Hmong","PaThen"),"Hmong-Mien",Language))
d <- mutate(d,Language = ifelse(Pop1%in%c("Cham","Ede","Giarai"),"Austronesian",Language))
d <- mutate(d,Language = ifelse(Pop1%in%c("Cong","HaNhi","LaHu","LoLo","PhuLa","Sila"),"Sino-Tibetan",Language))
d <- mutate(d,Language = ifelse(Pop1%in%c("KhoMu","Kinh","Mang","Muong"),"Austro-Asiatic",Language))

# Change len label
levels(d$Len) <- c("1-5 cM", "5-10 cM", ">10 cM")

# Subset data here
d2 <- d[d$Language!="NA",]
#d2 <- d[d$Language=="Tai-Kadai",]
#d2 <- d[d$Language=="Hmong-Mien",]
#d2 <- d[d$Language=="Austronesian",]
#d2 <- d[d$Language=="Sino-Tibetan",]
#d2 <- d[d$Language=="Austro-Asiatic",]

# Set the target population here by using Pop1 == Pop2
d2$Target <- ifelse(d2$Pop1==d2$Pop2, "T", "F")
d2[d2$Pop1==d2$Pop2,]$Median <- NA
d2[d2$Pop1==d2$Pop2,]$Average <- NA
d2[d2$Pop1==d2$Pop2,]$N_ind <- NA

# Order by Language groups
d2$Pop1 <- factor(d2$Pop1, levels=c("BoY","CoLao","LaChi","Nung","Tay","Thai","Dao","Hmong","PaThen","Cham","Ede","Giarai","Cong","HaNhi","LaHu","LoLo","PhuLa","Sila","KhoMu","Kinh","Mang","Muong"), ordered=T)

# Get hte map
map.world <- map_data(map="world")

# Set a complete cases for latitude and longitude scale
cc <- complete.cases(d2$Average)

# Visualization
# fix the limits with geo lon lat coordinates, according to the space you want to show by coord_quickmap
p <- ggplot()
p <- p + theme()
p <- p + geom_map(data=map.world, map=map.world, aes(map_id=region), fill="white", colour="grey", size=0.15)
p <- p + coord_quickmap(ylim=c(min(d2$Latitude[cc]),max(d2$Latitude[cc])), xlim=c(min(d2$Longitude[cc]),max(d2$Longitude[cc]))) 
p <- p + geom_point(data=d2, aes(x=Longitude, y=Latitude, fill=N_ind, size=Average), shape=21, alpha=0.7) 
p <- p + scale_fill_gradient(low="lightblue", high="red")
p <- p + geom_point(data=d2[d2$Target=="T",], aes(x=Longitude, y=Latitude), pch=17, size=2.5, color="black")
p <- p + facet_wrap(.~Len*Pop1, nrow=3)
#p <- p + facet_wrap(.~Pop1, nrow=4)
p <- p + labs(fill="Block number mean (n)", size="Total length mean (cM)")
p <- p + theme(legend.text = element_text(size = 12), legend.title = element_text(size = 14))
p <- p + theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14))
p <- p + theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10))
p <- p + theme(strip.text.x = element_text(size = 12), strip.text.y = element_text(size=12))
p <- p + xlab("Longitude") + ylab("Latitude")
p


#######################################################################################################

# Read len data

data <- read.table("/mnt/scratch/dang/Vietnam/IBD/SHAPEIT_ref_m2/all.lPSC.tag.L.N", header=T)

data$Country2 <- factor(data$Country2, levels=c("Taiwan","China","Vietnam","Cambodia","Laos","Thailand","Myanmar","Malaysia","Indonesia","Philippines","India"), ordered=T)
data <- data[order(data$Country2),]

# Exclude pops here
pop_exclude <- c("Atayal1","Ami1","Paiwan","Bunun","Rukai","Tao","Mamanwa1")
data <- data[!data$Pop1%in%pop_exclude,]
data <- data[!data$Pop2%in%pop_exclude,]
data <- data[data$Country1=="Vietnam",]
data <- data[data$Pop1!=data$Pop2,]
head(data)

average <- data %>% group_by(Pop1, Pop2, Len) %>% 
  summarise_at(vars(Length, N_ind), list(~mean(.)))
colnames(average) <- c("Pop1", "Pop2", "Len", "A_L", "A_N")
average$A_N <- round(average$A_N)
data <- data %>% left_join(average)

data$Pop1 <- factor(data$Pop1, levels=c("BoY","CoLao","LaChi","Nung","Tay","Thai","Dao","Hmong","PaThen","Cham","Ede","Giarai","Cong","HaNhi","LaHu","LoLo","PhuLa","Sila","KhoMu","Kinh","Mang","Muong"), ordered=T)

# Order x axis by pop_order as admixture plot
pop_order <- read.table("/mnt/scratch/dang/Vietnam/admixture/outgroup.v2/pong_pop_order.txt", header=F)
data$Pop2 <- factor(data$Pop2, levels=pop_order$V1, ordered=T)
data <- data[order(data$Pop2),]

# Add language groups for Vietnam only
data$Language <- "NA"
data <- mutate(data,Language = ifelse(Pop1%in%c("BoY","CoLao","LaChi","Nung","Tay","Thai"),"Tai-Kadai",Language))
data <- mutate(data,Language = ifelse(Pop1%in%c("Dao","Hmong","PaThen"),"Hmong-Mien",Language))
data <- mutate(data,Language = ifelse(Pop1%in%c("Cham","Ede","Giarai"),"Austronesian",Language))
data <- mutate(data,Language = ifelse(Pop1%in%c("Cong","HaNhi","LaHu","LoLo","PhuLa","Sila"),"Sino-Tibetan",Language))
data <- mutate(data,Language = ifelse(Pop1%in%c("KhoMu","Kinh","Mang","Muong"),"Austro-Asiatic",Language))

# Change len label
levels(data$Len) <- c("1-5 cM", "5-10 cM", ">10 cM")

# Subset data here
d2 <- data
#d2 <- data[data$Language=="Tai-Kadai",]
#d2 <- data[data$Language=="Hmong-Mien",]
#d2 <- data[data$Language=="Austronesian",]
#d2 <- data[data$Language=="Sino-Tibetan",]
#d2 <- data[data$Language=="Austro-Asiatic",]


p <- ggplot(d2, aes(x = Pop2, y = Length, fill=A_N))
p <- p + geom_boxplot(outlier.size=0.5, outlier.shape=16, notch=F)
p <- p + coord_cartesian(ylim=c(2, 100))
p <- p + labs(x=NULL, y="Total block length (cM)", fill="Block number mean (n)")
p <- p + scale_fill_gradient(low="lightblue", high="red")
p <- p + theme(axis.line.x = element_line(color="black", size = 0.5, linetype = 1),
               axis.line.y = element_line(color="black", size = 0.5, linetype = 1))
p <- p + facet_wrap(.~Len*Pop1, nrow=3, scales="free_x")
p <- p + theme(panel.background = element_blank())
p <- p + theme(legend.text = element_text(size = 12), legend.title = element_text(size = 12))
p <- p + theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14))
p <- p + theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))
p <- p + theme(strip.text.x = element_text(size = 12), strip.text.y = element_text(size=12))
p <- p + theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1))
p <- p + scale_x_discrete(limits=unique(data$Pop2))
p






#######################################################################################################

# Analyze within Pop
data <- read.table("/mnt/scratch/dang/Vietnam/IBD/SHAPEIT_ref_m2/all.lPSC.tag.L.N", header=T)
data <- data[data$Country1=="Vietnam",]
data <- data[data$Pop1==data$Pop2,]
head(data)

average <- data %>% group_by(Pop1, Pop2, Len) %>% 
  summarise_at(vars(Length, N_ind), list(~mean(.)))
colnames(average) <- c("Pop1", "Pop2", "Len", "A_L", "A_N")
average$A_N <- round(average$A_N)
data <- data %>% left_join(average)

# Change len label
levels(data$Len) <- c("1-5 cM", "5-10 cM", ">10 cM")

p <- ggplot(data, aes(x = reorder(Pop1, Length, FUN=median), y = Length, fill=A_N))
p <- p + geom_boxplot(outlier.size=0.5, outlier.shape=16, notch=F)
sts <- boxplot.stats(data$Length)$stats
p <- p + coord_cartesian(ylim=c(min(sts)*2.5,max(sts)*2.5))
p <- p + labs(x=NULL, y="Total block length (cM)", fill="Block number mean (n)")
p <- p + scale_fill_gradient(low="lightblue", high="red")
p <- p + facet_wrap(.~Len, ncol=3)
p <- p + theme(axis.line.x = element_line(color="black", size = 0.5, linetype = 1),
               axis.line.y = element_line(color="black", size = 0.5, linetype = 1))
p <- p + theme(panel.background = element_blank())
p <- p + theme(legend.text = element_text(size = 12), legend.title = element_text(size = 12))
p <- p + theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14))
p <- p + theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))
p <- p + theme(strip.text.x = element_text(size = 12), strip.text.y = element_text(size=12))
p <- p + theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1))
#p <- p + scale_x_discrete(limits=unique(d2$Pop))
p

#######################################################################################################

# Plot within pop number vs. length to infer demography

# Change len label
levels(average$Len) <- c("1-5 cM", "5-10 cM", ">10 cM")

# Add language groups for Vietnam only
average$Language <- "NA"
average <- mutate(average,Language = ifelse(Pop1%in%c("BoY","CoLao","LaChi","Nung","Tay","Thai"),"Tai-Kadai",Language))
average <- mutate(average,Language = ifelse(Pop1%in%c("Dao","Hmong","PaThen"),"Hmong-Mien",Language))
average <- mutate(average,Language = ifelse(Pop1%in%c("Cham","Ede","Giarai"),"Austronesian",Language))
average <- mutate(average,Language = ifelse(Pop1%in%c("Cong","HaNhi","LaHu","LoLo","PhuLa","Sila"),"Sino-Tibetan",Language))
average <- mutate(average,Language = ifelse(Pop1%in%c("KhoMu","Kinh","Mang","Muong"),"Austro-Asiatic",Language))


p <- ggplot(average, aes(x = A_L, y = A_N, color=Language))
#p <- p + geom_text(aes(label=Pop1), vjust = 1.5, nudge_y = 0.0025, size=5)
p <- p + geom_point(size=4,alpha=0.8)
p <- p + geom_text_repel(
  aes(x=A_L, y=A_N,label=Pop1,color=Language), 
  size=6, 
  segment.alpha=0.5)
p <- p + scale_color_manual(values=c("#9966CC","#CC6633","#FFCC33","#66CC99","#CC0033"))
p <- p + facet_wrap(.~Len, ncol=3)
p <- p + labs(x="Total length mean (cM)", y="Block number mean (n)", color="Language group")
p <- p + theme(axis.line.x = element_line(color="black", size = 0.5, linetype = 1),
               axis.line.y = element_line(color="black", size = 0.5, linetype = 1))
p <- p + theme(panel.background = element_blank())
p <- p + theme(legend.text = element_text(size = 12), legend.title = element_text(size = 12))
p <- p + theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14))
p <- p + theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))
p <- p + theme(strip.text.x = element_text(size = 12), strip.text.y = element_text(size=12))
p

####################################################################################################

# Plot within pop number vs. length to infer demography without separating by length

data <- read.table("/mnt/scratch/dang/Vietnam/IBD/SHAPEIT_ref_m2/all.lPSC.least2cM.L.N", header=T)
data <- data[data$Country1=="Vietnam",]
data <- data[data$Length]
data <- data[data$Pop1==data$Pop2,]
head(data)

average <- data %>% group_by(Pop1, Pop2) %>% 
  summarise_at(vars(Length, N_ind), list(~mean(.)))
colnames(average) <- c("Pop1", "Pop2", "A_L", "A_N")
average$A_N <- round(average$A_N)

# Add language groups for Vietnam only
average$Language <- "NA"
average <- mutate(average,Language = ifelse(Pop1%in%c("BoY","CoLao","LaChi","Nung","Tay","Thai"),"Tai-Kadai",Language))
average <- mutate(average,Language = ifelse(Pop1%in%c("Dao","Hmong","PaThen"),"Hmong-Mien",Language))
average <- mutate(average,Language = ifelse(Pop1%in%c("Cham","Ede","Giarai"),"Austronesian",Language))
average <- mutate(average,Language = ifelse(Pop1%in%c("Cong","HaNhi","LaHu","LoLo","PhuLa","Sila"),"Sino-Tibetan",Language))
average <- mutate(average,Language = ifelse(Pop1%in%c("KhoMu","Kinh","Mang","Muong"),"Austro-Asiatic",Language))

p <- ggplot(average, aes(x = A_L, y = A_N, color=Language))
#p <- p + geom_text(aes(label=Pop1), vjust = 1.5, nudge_y = 0.0025, size=5)
p <- p + geom_point(size=4,alpha=0.8)
p <- p + geom_text_repel(
  aes(x=A_L, y=A_N,label=Pop1,color=Language), 
  size=6, 
  segment.alpha=0.5)
p <- p + scale_color_manual(values=c("#9966CC","#CC6633","#FFCC33","#66CC99","#CC0033"))
p <- p + labs(x="Total length mean (cM)", y="Block number mean (n)", color="Language group")
p <- p + theme(axis.line.x = element_line(color="black", size = 0.5, linetype = 1),
               axis.line.y = element_line(color="black", size = 0.5, linetype = 1))
p <- p + theme(panel.background = element_blank())
p <- p + theme(legend.text = element_text(size = 12), legend.title = element_text(size = 12))
p <- p + theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14))
p <- p + theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))
p <- p + theme(strip.text.x = element_text(size = 12), strip.text.y = element_text(size=12))
p


#######################################################################################################

# MDS

data <- read_delim("/mnt/scratch/dang/Vietnam/IBD/SHAPEIT_ref_m2/all.lPSC.Merged.over10cM.stats",delim="\t")
pop_order <- read_delim("/mnt/scratch/dang/Vietnam/admixture/outgroup.v2/pong_pop_order.txt",delim="\t",col_names = F)
Affy6 <- c("Puyuma","Saisiat","Manobo","Ternate","Temuan","Roti","Hiri","Besemah","Alor","Flores","Timor","Bidayuh","Jehai","Pingpu","Rukai","Paiwan","Bunun")
Outgroup <- c("French", "Mbuti")
Ancient <- c("P-Tianyuan","Ho-Pha_Faen","Ho-Gua_Cha","N-Gua_Cha","N-Man_Bac","N-Nam_Tun","N-Mai_Da_Dieu","N-Hon_Hai_Co_Tien","N-Tam_Pa_Ling","N-Tam_Hang","N-Oakaie","N-Loyang_Ujung","BA-Nui_Nap","IA-Vat_Komnou","IA-Long_Long_Rak","Hi-Hon_Hai_Co_Tien","Hi-Supu_Hujung","Hi-Kinabatagan")
pop_exclude <- c(Outgroup, Ancient,Affy6)
m_data <- data %>% filter(!Pop1%in%pop_exclude&!Pop2%in%pop_exclude)
m_pop_order <- pop_order %>% filter(!X1%in%pop_exclude)
d.matrix <- matrix(NA, nrow = nrow(m_pop_order), ncol = nrow(m_pop_order))
colnames(d.matrix) <- m_pop_order$X1
rownames(d.matrix) <- m_pop_order$X1
for (i in m_pop_order$X1){
  for (j in m_pop_order$X1) {
    if (is.na(m_data[m_data$Pop1==i&m_data$Pop2==j,]$Average)==F){
      d.matrix[i,j] <- m_data[m_data$Pop1==i&m_data$Pop2==j,]$Average
    }
  }
} 

# Normalizing
norm_data <- d.matrix
n <- 0
c <- 0
while(n<nrow(d.matrix)){
  n <- n+1
  while(c<ncol(d.matrix)){
    c <- c+1
    if (is.na(norm_data[n,c])==T){norm_data[n,c] <- mean(norm_data[,n], na.rm=T)}
    #norm_data[,n] <- 1-(norm_data[,n]+(0-min(data[,n])))/max(data[,n]+(0-min(data[,n])))
  }
  c <- 0
}

n <- 0
while(n<ncol(norm_data)){
  n <- n+1
  norm_data[,n] <- 1-(norm_data[,n]+(0-min(norm_data[,n],na.rm=T)))/max(norm_data[,n]+(0-min(norm_data[,n],na.rm=T)),na.rm=T)
}


mds <- cmdscale(as.dist(norm_data))
m <- as.data.frame(mds)
d <- add_rownames(m, "Pop")
info <- read.table("/mnt/scratch/dang/Vietnam/map/all.map.am.geo.info2", header=T)
data <- d %>% left_join(select(info,c(Pop,Country,Type,Period))) %>% distinct()

data$Country <- factor(data$Country, levels=c("Taiwan","China","Vietnam","Cambodia","Laos","Thailand","Myanmar","Malaysia","Indonesia","Philippines","India","France","Congo"))

p <- ggplot(data, aes(x=V1,y=V2,color=Country))
p <- p + geom_point(aes(color=Country), alpha=0.8)
#p <- p + geom_text(aes(label=Pop), size=4, vjust = 0, nudge_y = 0.01, size=4, check_overlap=F) #+ stat_ellipse()
p <- p + geom_text_repel(
  aes(x=V1, y=V2,label=Pop,color=Country),
  size=4,
  point.padding=0.25,
  segment.alpha=0.5)
p <- p + scale_color_manual(values=c("#A6CEE3","#1F78B4","#B2DF8A","#33A02C","#E31A1C","#CAB2D6","#6A3D9A","#B15928","#0000FF","#000000"))
p <- p + theme(axis.line.x = element_line(color="black", size = 0.5, linetype = 1),
               axis.line.y = element_line(color="black", size = 0.5, linetype = 1))
p <- p + theme(panel.background = element_blank()) 
p <- p + labs(x = "MDS1", y = "MDS2")
p <- p + theme(legend.text = element_text(size = 12), legend.title = element_text(size = 14))
p <- p + theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14))
p <- p + theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10))
p

data$Language <- "X"
data <- mutate(data,Language = ifelse(Pop%in%c("BoY","CoLao","LaChi","Nung","Tay","Thai"),"Tai-Kadai",Language))
data <- mutate(data,Language = ifelse(Pop%in%c("Dao","Hmong","PaThen"),"Hmong-Mien",Language))
data <- mutate(data,Language = ifelse(Pop%in%c("Cham","Ede","Giarai"),"Austronesian",Language))
data <- mutate(data,Language = ifelse(Pop%in%c("Cong","HaNhi","LaHu","LoLo","PhuLa","Sila"),"Sino-Tibetan",Language))
data <- mutate(data,Language = ifelse(Pop%in%c("KhoMu","Kinh","Mang","Muong"),"Austro-Asiatic",Language))

p2 <- ggplot(data, aes(x=V1,y=V2))
p2 <- p2 + geom_point(data=data[data$Language=="X",],alpha=0.2)
p2 <- p2 + geom_text_repel(
  data=data[data$Language=="X",],
  aes(x=V1, y=V2,label=Pop),
  size=4,
  point.padding=0.25,
  segment.alpha=0.2, alpha=0.2)
p2 <- p2 + geom_point(data=data[data$Language!="X",],aes(color=Language))
p2 <- p2 + geom_text_repel(
  data=data[data$Language!="X",],
  aes(x=V1, y=V2,label=Pop,color=Language),
  size=4,
  point.padding=0.25,
  segment.alpha=0.5)
p2 <- p2 + scale_color_manual(values=c("#9966CC","#CC6633","#FFCC33","#66CC99","#CC0033","#111111"))
p2 <- p2 + theme(axis.line.x = element_line(color="black", size = 0.5, linetype = 1),
                 axis.line.y = element_line(color="black", size = 0.5, linetype = 1))
p2 <- p2 + theme(panel.background = element_blank()) 
p2 <- p2 + labs(x = "MDS1", y = "MDS2")
p2 <- p2 + theme(legend.text = element_text(size = 12), legend.title = element_text(size = 14))
p2 <- p2 + theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14))
p2 <- p2 + theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10))
p2

##########################################################################################################################
# heatmap

data <- read_delim("/mnt/scratch/dang/Vietnam/IBD/SHAPEIT_ref_m2/all.lPSC.Merged.least2CM.stats",delim="\t")
info <- read_csv("/r1/people/dang_liu/Projects/Vietnam/Vietnam.metadata.csv")
pop_order <- read_delim("/mnt/scratch/dang/Vietnam/admixture/outgroup.v2/pong_pop_order.txt",delim="\t",col_names = F)
Affy6 <- c("Puyuma","Saisiat","Manobo","Ternate","Temuan","Roti","Hiri","Besemah","Alor","Flores","Timor","Bidayuh","Jehai","Pingpu","Rukai","Paiwan","Bunun")
Outgroup <- c("French", "Mbuti")
Ancient <- c("P-Tianyuan","Ho-Pha_Faen","Ho-Gua_Cha","N-Gua_Cha","N-Man_Bac","N-Nam_Tun","N-Mai_Da_Dieu","N-Hon_Hai_Co_Tien","N-Tam_Pa_Ling","N-Tam_Hang","N-Oakaie","N-Loyang_Ujung","BA-Nui_Nap","IA-Vat_Komnou","IA-Long_Long_Rak","Hi-Hon_Hai_Co_Tien","Hi-Supu_Hujung","Hi-Kinabatagan")
pop_exclude <- c(Outgroup, Ancient,Affy6)
m_data <- data %>% filter(!Pop1%in%pop_exclude&!Pop2%in%pop_exclude)
m_pop_order <- pop_order %>% filter(!X1%in%pop_exclude)
d.matrix <- matrix(0, nrow = nrow(m_pop_order), ncol = nrow(m_pop_order))
colnames(d.matrix) <- m_pop_order$X1
rownames(d.matrix) <- m_pop_order$X1
for (i in m_pop_order$X1){
  for (j in m_pop_order$X1) {
    if (is.na(m_data[m_data$Pop1==i&m_data$Pop2==j,]$Average)==F){
      d.matrix[i,j] <- ifelse(i!=j, m_data[m_data$Pop1==i&m_data$Pop2==j,]$Average, NA)
    }
  }
} 

# Normalizing
norm_data <- d.matrix
#n <- 0
#c <- 0
#while(n<nrow(d.matrix)){
#  n <- n+1
#  while(c<ncol(d.matrix)){
#    c <- c+1
#    if (is.na(norm_data[n,c])==T){norm_data[n,c] <- 0}
#    #norm_data[,n] <- 1-(norm_data[,n]+(0-min(data[,n])))/max(data[,n]+(0-min(data[,n])))
#  }
#  c <- 0
#}

#n <- 0
#while(n<ncol(norm_data)){
#  n <- n+1
#  norm_data[,n] <- (norm_data[,n]+(0-min(norm_data[,n],na.rm=T)))/max(norm_data[,n]+(0-min(norm_data[,n],na.rm=T)),na.rm=T)
#}


# annotaion here
colnames(m_pop_order)[1] <- "Pop"
annotation <- as.data.frame(m_pop_order) %>% left_join(info) %>% distinct(Pop, .keep_all = TRUE) %>% select(Country) 
rownames(annotation) <- rownames(norm_data)
# Specify colors
ann_colors = list(
  Country = c(Taiwan="#A6CEE3",China="#1F78B4",Vietnam="#B2DF8A",Cambodia="#33A02C",Laos="#FB9A99",Thailand="#E31A1C",Myanmar="#FDBF6F",Malaysia="#FF7F00",Indonesia="#CAB2D6",Philippines="#6A3D9A",India="#B15928")
)

#ann_colors = list(
#  Language = c(Austronesian="#CC6633",Austro-Asiatic="#9966CC",Hmong-Mien="#FFCC33",Sino-Tibetan="#66CC99",Tai-Kadai="#CC0033",
#               Indo-European="#FFCCFF",Dravidian="#CC0099",Andamanese="#660033",Mongolic="#0000FF",Tungusic="#66CCFF",Ancient="#000000")
#)

# plot heatmap
res <- pheatmap(
  color= colorRampPalette(c("navy", "white", "firebrick3"))(10),
  fontsize_number=12,
  fontsize_row=12,
  fontsize_col=12,
  cellwidth=NA,
  cellheight=NA,
  as.matrix(norm_data),
  #display_numbers=as.matrix(df.mat),
  annotation=annotation,
  annotation_colors=ann_colors, 
  cluster_rows=FALSE, cluster_cols=FALSE
)
