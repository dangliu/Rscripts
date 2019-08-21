# title: "PCA on GW data"
# author: "Dang Liu 16.Jan.2019"

# Last updated: 17.Jun.2019

# Use libraries
library(RColorBrewer)
library(tidyverse)
library(stringr)
library(ggrepel)


# Read eigenvalue result
eval <- read.table("/mnt/scratch/dang/Vietnam/pca/outgroup.v2/SEA.SC.TW.merged3.outgroup.v2.noFM.pruned.eval",header=F)
d <- data.frame(Eigenvalue=eval$V1) %>% mutate(PC=row_number())
d %>%
  ggplot(aes(x=PC, y=Eigenvalue)) +
  geom_histogram(stat="identity") +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10),limits=c(0.5,10.5)) +
  theme(axis.line.x = element_line(color="black", size = 0.5, linetype = 1),
          axis.line.y = element_line(color="black", size = 0.5, linetype = 1)) +
  theme(panel.background = element_blank()) +
  theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14)) +
  theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10))


# Read data
data <- read.table("/mnt/scratch/dang/Vietnam/pca/outgroup.v2/SEA.SC.TW.merged3.outgroup.v2.noFM.pruned.evec",header=F)
#info <- read.table("/mnt/scratch/dang/Vietnam/map/all.map.am.geo.info2", header=T)
info <- read_csv("/r1/people/dang_liu/Projects/Vietnam/Vietnam.metadata.csv")
pop_N <- read_delim("/mnt/scratch/dang/Vietnam/pca/outgroup.v2/pop_N.list", delim="\t", col_names=F)
# Arrange data
colnames(pop_N) <- c("Pop", "Pop_N")
colnames(data) <- c("IID","PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8","PC9","PC10","C")
data <- data[,!(colnames(data)=="C")] 
data <- info %>% left_join(data) %>% left_join(pop_N)
head(data)
data <- data[is.na(data$PC1)==F,]
data$Country <- factor(data$Country, levels=c("Ancient","Taiwan","Mongolia","China","Vietnam","Cambodia","Laos","Thailand","Myanmar","Malaysia","Indonesia","Philippines","India","France","Congo"))
#brewer.pal(n = 13, name = "Paired")

#data$Language <- "X"
#data <- mutate(data,Language = ifelse(Pop%in%c("BoY","CoLao","LaChi","Nung","Tay","Thai"),"Tai-Kadai",Language))
#data <- mutate(data,Language = ifelse(Pop%in%c("Dao","Hmong","PaThen"),"Hmong-Mien",Language))
#data <- mutate(data,Language = ifelse(Pop%in%c("Cham","Ede","Giarai"),"Austronesian",Language))
#data <- mutate(data,Language = ifelse(Pop%in%c("Cong","HaNhi","LaHu","LoLo","PhuLa","Sila"),"Sino-Tibetan",Language))
#data <- mutate(data,Language = ifelse(Pop%in%c("KhoMu","Kinh","Mang","Muong"),"Austro-Asiatic",Language))


# plot
# Pop, text
# Ref_ellipse: https://ggplot2.tidyverse.org/reference/stat_ellipse.html
# Flip PC2 to match the geography
ann_colors = c(Taiwan="#A6CEE3",Mongolia="#CC9900",China="#1F78B4",Vietnam="#B2DF8A",Cambodia="#33A02C",Laos="#FB9A99",Thailand="#E31A1C",Myanmar="#FDBF6F",Malaysia="#FF7F00",Indonesia="#CAB2D6",Philippines="#6A3D9A",India="#B15928","Austronesian"="#CC6633","Austro-Asiatic"="#9966CC","Hmong-Mien"="#FFCC33","Sino-Tibetan"="#66CC99","Tai-Kadai"="#CC0033")

p4 <- ggplot(data, aes(x=-PC1,y=-PC2,color=Country))
#p4 <- p4 + geom_point(data=data[data$Language!="X",], pch=20, aes(color=Language), size=9, alpha=1)
#p4 <- p4 + stat_ellipse(data=data[data$Language!="X",], aes(fill=Language), color="transparent", geom = "polygon", alpha=0.8)
p4 <- p4 + geom_text(data=data[data$Type!="ancient",], aes(label=Pop_N), size=4)
#p4 <- p4 + geom_point(aes(color=Country, pch=Type), size=3)
p4 <- p4 + geom_point(data=data[data$Type=="ancient",], pch=20, color="#000000", size=9, alpha=0.8)
p4 <- p4 + geom_text(data=data[data$Type=="ancient",], aes(label=Pop_N), size=4)
#p4 <- p4 + geom_text_repel(
#  data=data[data$Type=="ancient" | data$Pop%in%c("Kharia","Onge"),], 
#  aes(x=-PC1, y=-PC2,label=Pop_N),
#  size=4, color="#000000",
#  point.padding=0.25,
#  segment.alpha=0.5)
#p4 <- p4 + scale_color_manual(values=c("#A6CEE3","#1F78B4","#B2DF8A","#33A02C","#FB9A99","#E31A1C","#FDBF6F","#FF7F00","#CAB2D6","#6A3D9A","#B15928","#0000FF","#000000"))
p4 <- p4 + scale_color_manual(values=ann_colors)
#p4 <- p4 + scale_fill_manual(values=ann_colors)
p4 <- p4 + scale_shape_manual(values=c(4,19))
p4 <- p4 + theme(axis.line.x = element_line(color="black", size = 0.5, linetype = 1),
                 axis.line.y = element_line(color="black", size = 0.5, linetype = 1))
p4 <- p4 + theme(panel.background = element_blank()) 
p4 <- p4 + labs(x = "PC1", y = "PC2")
p4 <- p4 + theme(legend.text = element_text(size = 12), legend.title = element_text(size = 14))
p4 <- p4 + theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14))
p4 <- p4 + theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10))
p4

# Language, point, text
Language = c("Austronesian"="#CC6633","Austro-Asiatic"="#9966CC","Hmong-Mien"="#FFCC33","Sino-Tibetan"="#66CC99","Tai-Kadai"="#CC0033",
             "Indo-European"="#FFCCFF","Dravidian"="#CC0099","Andamanese"="#660033","Mongolic"="#0000FF","Tungusic"="#66CCFF","Ancient"="#000000")

p5 <- ggplot(data, aes(x=-PC1,y=-PC2))
#p5 <- p5 + geom_text(data=data[data$Language=="X"&data$Type!="ancient",], aes(label=Pop_N), size=4, alpha=0.1)
p5 <- p5 + geom_point(data=data[data$Country=="Vietnam"&data$Type!="ancient",], pch=20, color="#B2DF8A", size=9, alpha=0.8)
p5 <- p5 + geom_text(aes(label=Pop_N, color=Language), size=4)
p5 <- p5 + geom_point(data=data[data$Type=="ancient",], pch=20, color="#000000", size=9, alpha=0.4)
#p5 <- p5 + geom_text(data=data[data$Type=="ancient",], aes(label=Pop_N), size=4)
#p5 <- p5 + geom_point(data=data[data$Language!="X",], pch=20, aes(color=Language), size=9, alpha=0.8)
#p5 <- p5 + geom_text(data=data[data$Language!="X",], aes(label=Pop_N), size=4)
p5 <- p5 + scale_color_manual(values=Language)
p5 <- p5 + theme(axis.line.x = element_line(color="black", size = 0.5, linetype = 1),
                 axis.line.y = element_line(color="black", size = 0.5, linetype = 1))
p5 <- p5 + theme(panel.background = element_blank())
p5 <- p5 + labs(x = "PC1", y = "PC2")
p5 <- p5 + theme(legend.text = element_text(size = 12), legend.title = element_text(size = 14))
p5 <- p5 + theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14))
p5 <- p5 + theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10))
p5

# Vietnam only
#p7 <- ggplot(data[data$Language!="X",], aes(x=-PC1,y=-PC2))
p7 <- ggplot(data=data[data$Country=="Vietnam"&data$Type!="ancient",], aes(x=-PC1,y=-PC2))
p7 <- p7 + geom_text(aes(label=Pop, color=Language), size=4)
p7 <- p7 + scale_color_manual(values=Language)
p7 <- p7 + theme(axis.line.x = element_line(color="black", size = 0.5, linetype = 1),
                 axis.line.y = element_line(color="black", size = 0.5, linetype = 1))
p7 <- p7 + theme(panel.background = element_blank())
p7 <- p7 + labs(x = "PC1", y = "PC2")
p7 <- p7 + theme(legend.text = element_text(size = 12), legend.title = element_text(size = 14))
p7 <- p7 + theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14))
p7 <- p7 + theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10))
p7


#################################################################################
