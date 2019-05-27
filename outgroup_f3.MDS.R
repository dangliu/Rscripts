# title: "Heatmap of outgroup f3 on GW data"
# author: "Dang Liu 11.Feb.2019"

# Last updated: 01.Mar.2019

# Use libraries
library(tidyverse)
library(RColorBrewer)
library(gtools)
library(data.table)
library(ggrepel)
library(Rmisc)

# load data from a huge f3 comparing matrix
load("/mnt/scratch/dang/Vietnam/outgroup_f3/HO.ancient.outgroup.v2.Mbuti_f3.Rdata")
head(f3_res)
info <- read.table("/mnt/scratch/dang/Vietnam/outgroup/HO.ancient.outgroup.v2.geo.info", header=T)
# Prepare data set for annotation and order
info <- info[!info$Pop %in% c("Mbuti","French"),] %>% select(Pop, Country)
d <- f3_res %>% select(A, B ,f3)
colnames(d) <- c("Pop", "Pop2", "f3")
d <- info %>% left_join(d)
d$Country <- factor(d$Country, levels=c("Taiwan","China","Vietnam","Cambodia","Laos","Thailand","Myanmar","Malaysia","Indonesia","Philippines","India"), ordered=T)
d <- d[order(d$Country),]
# Exclude pops here
pop_exclude <- c("Atayal1","Ami1","Mamanwa1")
d <- d[!d$Pop%in%pop_exclude,]
d <- d[!d$Pop2%in%pop_exclude,] 
# Make a matrix for heatmap function
d2 <- f3_res[!f3_res$A%in%pop_exclude,]
d2 <- d2[!d2$B%in%pop_exclude,] 
d2.matrix <- d2 %>% select(A, B, f3) %>% dcast(A ~ B, value.var = "f3") %>% remove_rownames %>% column_to_rownames(var="A") %>% data.matrix()
head(d2.matrix)

# Order the matrix
d2.matrix <- d2.matrix[unique(d$Pop),unique(d$Pop)]


# Deal with 0 (same pops vs. each other will result in outgroup f3=0; substitute it to maximum + 0.01)
norm_data <- d2.matrix
n <- 0
c <- 0
while(n<nrow(d2.matrix)){
  n <- n+1
  while(c<ncol(d2.matrix)){
    c <- c+1
    if (norm_data[n,c] == 0){norm_data[n,c] = NA}
    #norm_data[,n] <- 1-(norm_data[,n]+(0-min(data[,n])))/max(data[,n]+(0-min(data[,n])))
  }
  c <- 0
}

# Normalizing 
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
data$Period <- factor(data$Period, levels=c("P","Hi","IA","BA","LN","N","Ho","Pa"), ordered=T)

p <- ggplot(data, aes(x=V1,y=V2,color=Country))
#p <- p + geom_text(aes(label=Pop), size=4, check_overlap=F) #+ stat_ellipse()
p <- p + geom_point(aes(color=Country,pch=Period), size=ifelse(data$Type=="ancient",3,2), alpha=0.8)
p <- p + geom_text_repel(
  data=data[data$Country=="Vietnam"&data$Type=="modern",],
  aes(x=V1, y=V2,label=Pop,color=Country),
  size=4,
  point.padding=0.25,
  segment.alpha=0.5)
p <- p + scale_color_manual(values=c("#A6CEE3","#1F78B4","#B2DF8A","#33A02C","#FB9A99","#E31A1C","#FDBF6F","#FF7F00","#CAB2D6","#6A3D9A","#B15928","#0000FF","#000000"))
p <- p + scale_shape_manual(values=c(19,7,8,9,10,11,12,13))
p <- p + theme(axis.line.x = element_line(color="black", size = 0.5, linetype = 1),
                 axis.line.y = element_line(color="black", size = 0.5, linetype = 1))
p <- p + theme(panel.background = element_blank()) 
p <- p + labs(x = "MDS1", y = "MDS2")
p <- p + theme(legend.text = element_text(size = 12), legend.title = element_text(size = 14))
p <- p + theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14))
p <- p + theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10))
p


#############################Within Vietnam###############################################################################

data$Language <- "X"
data <- mutate(data,Language = ifelse(Pop%in%c("BoY","CoLao","LaChi","Nung","Tay","Thai"),"Tai-Kadai",Language))
data <- mutate(data,Language = ifelse(Pop%in%c("Dao","Hmong","PaThen"),"Hmong-Mien",Language))
data <- mutate(data,Language = ifelse(Pop%in%c("Cham","Ede","Giarai"),"Austronesian",Language))
data <- mutate(data,Language = ifelse(Pop%in%c("Cong","HaNhi","LaHu","LoLo","PhuLa","Sila"),"Sino-Tibetan",Language))
data <- mutate(data,Language = ifelse(Pop%in%c("KhoMu","Kinh","Mang","Muong"),"Austro-Asiatic",Language))

p2 <- ggplot(data[data$Language!="X",], aes(x=V1,y=V2,color=Language))
p2 <- p2 + geom_text(aes(label=Pop), size=4, vjust = 0, nudge_y = 0.001, check_overlap=F) #+ stat_ellipse()
p2 <- p2 + geom_point(aes(color=Language))
p2 <- p2 + scale_color_manual(values=c("#9966CC","#CC6633","#FFCC33","#66CC99","#CC0033","#111111"))
p2 <- p2 + theme(axis.line.x = element_line(color="black", size = 0.5, linetype = 1),
               axis.line.y = element_line(color="black", size = 0.5, linetype = 1))
p2 <- p2 + theme(panel.background = element_blank()) 
p2 <- p2 + labs(x = "MDS1", y = "MDS2")
p2 <- p2 + theme(legend.text = element_text(size = 12), legend.title = element_text(size = 14))
p2 <- p2 + theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14))
p2 <- p2 + theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10))
p2


###############################Combine two plots#########################################################################

A <- p + ggtitle("A") + theme(plot.title = element_text(size=15, face="bold"))
B <- p2 + ggtitle("B") + theme(plot.title = element_text(size=15, face="bold"))

multiplot(A, B, layout=matrix(c(1,2), ncol=2))

