# title: "PCA on GW data"
# author: "Dang Liu 16.Jan.2019"

# Last updated: 25.Feb.2019

# Use libraries
library(RColorBrewer)
library(tidyverse)
library(stringr)


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
info <- read.table("/mnt/scratch/dang/Vietnam/map/all.map.am.geo.info2", header=T)
# Arrange data
colnames(data) <- c("IID","PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8","PC9","PC10","C")
data <- data[,!(colnames(data)=="C")] 
data <- info %>% left_join(data)
head(data)
data <- data[is.na(data$PC1)==F,]
data$Country <- factor(data$Country, levels=c("Taiwan","China","Vietnam","Cambodia","Laos","Thailand","Myanmar","Malaysia","Indonesia","Philippines","India","France","Congo"))
#brewer.pal(n = 13, name = "Paired")

data$Language <- "X"
data <- mutate(data,Language = ifelse(Pop%in%c("BoY","CoLao","LaChi","Nung","Tay","Thai"),"Tai-Kadai",Language))
data <- mutate(data,Language = ifelse(Pop%in%c("Dao","Hmong","PaThen"),"Hmong-Mien",Language))
data <- mutate(data,Language = ifelse(Pop%in%c("Cham","Ede","Giarai"),"Austronesian",Language))
data <- mutate(data,Language = ifelse(Pop%in%c("Cong","HaNhi","LaHu","LoLo","PhuLa","Sila"),"Sino-Tibetan",Language))
data <- mutate(data,Language = ifelse(Pop%in%c("KhoMu","Kinh","Mang","Muong"),"Austro-Asiatic",Language))

# plot
# Country
p <- ggplot(data, aes(x=PC1,y=PC2,color=Country,pch=Region))
p <- p + geom_point(alpha=0.75)
p <- p + theme(axis.line.x = element_line(color="black", size = 0.5, linetype = 1),
               axis.line.y = element_line(color="black", size = 0.5, linetype = 1))
p <- p + theme(panel.background = element_blank())
p <- p + labs(x = "PC1", y = "PC2")
p

# Region
p2 <- ggplot(data, aes(x=PC1,y=PC2,color=Region))
p2 <- p2 + scale_color_brewer(type='qual',palette='Paired')
p2 <- p2 + geom_point(alpha=0.75)
p2 <- p2 + theme(axis.line.x = element_line(color="black", size = 0.5, linetype = 1),
               axis.line.y = element_line(color="black", size = 0.5, linetype = 1))
p2 <- p2 + theme(panel.background = element_blank())
p2 <- p2 + labs(x = "PC1", y = "PC2")
p2

# Pop, point
p3 <- ggplot(data, aes(x=PC1,y=PC2,color=Pop,pch=Type))
p3 <- p3 + geom_point(alpha=0.75)
colourCount = length(unique(data$Pop))
getPalette = colorRampPalette(brewer.pal(12, "Paired"))
p3 <- p3 + scale_color_manual(values = getPalette(colourCount))
p3 <- p3 + scale_shape_manual(values=c(0,19))
p3 <- p3 + theme(axis.line.x = element_line(color="black", size = 0.5, linetype = 1),
                 axis.line.y = element_line(color="black", size = 0.5, linetype = 1))
p3 <- p3 + theme(panel.background = element_blank()) + theme(legend.position='none')
p3 <- p3 + labs(x = "PC1", y = "PC2")
p3

# Pop, text
# Ref_ellipse: https://ggplot2.tidyverse.org/reference/stat_ellipse.html
# Flip PC2 to match the geography
p4 <- ggplot(data, aes(x=-PC1,y=PC2,color=Country))
p4 <- p4 + geom_text(aes(label=Pop), vjust = 0, nudge_y = 0.0025, size=4, check_overlap=F) #+ stat_ellipse()
p4 <- p4 + geom_point(aes(color=Country,pch=Type))
#p4 <- p4 + scale_color_brewer(palette="Paired")
p4 <- p4 + scale_color_manual(values=c("#A6CEE3","#1F78B4","#B2DF8A","#33A02C","#FB9A99","#E31A1C","#FDBF6F","#FF7F00","#CAB2D6","#6A3D9A","#B15928","#0000FF","#000000"))
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
p5 <- ggplot(data, aes(x=-PC1,y=PC2))
p5 <- p5 + geom_text(aes(label=Pop),size=4, alpha=0.1) #+ stat_ellipse()
p5 <- p5 + geom_point(data=data[data$Language!="X",], aes(color=Language), size=3) 
#colourCount = length(unique(data$Pop))
#getPalette = colorRampPalette(brewer.pal(12, "Paired"))
#p5 <- p5 + scale_color_manual(values = getPalette(colourCount))
p5 <- p5 + scale_color_manual(values=c("#CC6633","#9966CC","#FFCC33","#66CC99","#CC0033","#111111"))
p5 <- p5 + scale_shape_manual(values=c(0,4,2,3,5,19))
p5 <- p5 + theme(axis.line.x = element_line(color="black", size = 0.5, linetype = 1),
                 axis.line.y = element_line(color="black", size = 0.5, linetype = 1))
p5 <- p5 + theme(panel.background = element_blank())
p5 <- p5 + labs(x = "PC1", y = "PC2")
p5 <- p5 + theme(legend.text = element_text(size = 12), legend.title = element_text(size = 14))
p5 <- p5 + theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14))
p5 <- p5 + theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10))
p5


# Language, point, text

data$Language <- factor(data$Language, levels=c("Austronesian","Austro-Asiatic","Hmong-Mien","Sino-Tibetan","Tai-Kadai"), ordered=T)
data$Period <- factor(data$Period, levels=c("P","Hi","IA","BA","LN","N","Ho","Pa"), ordered=T)

cols <- c("Austronesian"="#CC6633","Austro-Asiatic"="#9966CC","Hmong-Mien"="#FFCC33","Sino-Tibetan"="#66CC99","Tai-Kadai"="#CC0033",
         "P"="#7FC97F","Hi"="#006633","IA"="#FF3300","BA"="#FF9933","LN"="#386CB0","N"="#F0027F","Ho"="#660000","Pa"="#666666")

p6 <- ggplot(data, aes(x=-PC1,y=PC2))
p6 <- p6 + geom_text(data=data[data$Type=="modern",],aes(label=Pop),size=4, alpha=0.1)
p6 <- p6 + geom_text(data=data[data$Type=="ancient",],aes(label=Pop, color=Period),size=4)
p6 <- p6 + geom_point(data=data[data$Language!="X",], aes(color=Language), size=3)
#colourCount = length(unique(data$Pop))
#getPalette = colorRampPalette(brewer.pal(12, "Paired"))
#p6 <- p6 + scale_color_manual(values = getPalette(colourCount))
#p6 <- p6 + scale_color_manual(values=c("#7FC97F","#006633","#FF3300","#FF9933","#386CB0","#F0027F","#660000","#666666","#CC6633","#9966CC","#FFCC33","#66CC99","#CC0033","#111111"))
p6 <- p6 + scale_color_manual(values=cols)
p6 <- p6 + scale_shape_manual(values=c(7,8,9,10,11,12,13))
p6 <- p6 + theme(axis.line.x = element_line(color="black", size = 0.5, linetype = 1),
                 axis.line.y = element_line(color="black", size = 0.5, linetype = 1))
p6 <- p6 + theme(panel.background = element_blank())
p6 <- p6 + labs(x = "PC1", y = "PC2")
p6 <- p6 + theme(legend.text = element_text(size = 12), legend.title = element_text(size = 14))
p6 <- p6 + theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14))
p6 <- p6 + theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10))
p6

#################################################################################

# Read eigenvalue result
eval <- read.table("/mnt/scratch/dang/Vietnam/pca/ancient_proj/all.Vietnam.lsqproj.eval",header=F)
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
data <- read.table("/mnt/scratch/dang/Vietnam/pca/ancient_proj/all.Vietnam.lsqproj.evec",header=F)
info <- read.table("/mnt/scratch/dang/Vietnam/map/all.map.am.geo.info2", header=T)
# Arrange data
colnames(data) <- c("IID","PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8","PC9","PC10","C")
data <- data[,!(colnames(data)=="C")] #%>% separate(FIID, c("FID", "IID"), ":", convert=T)
data <- info %>% left_join(data)
head(data)

# Subset
data <- data[is.na(data$PC1)==F,]
#data <- data[data$IID!="Ma912",]

data$Country <- factor(data$Country, levels=c("Taiwan","China","Vietnam","Cambodia","Laos","Thailand","Myanmar","Malaysia","Indonesia","Philippines"))
data$Period <- factor(data$Period, levels=c("P","Hi","IA","BA","LN","N","Ho","Pa"), ordered=T)

data$Language <- "Ancient"
data <- mutate(data,Language = ifelse(Pop%in%c("BoY","CoLao","LaChi","Nung","Tay","Thai"),"Tai-Kadai",Language))
data <- mutate(data,Language = ifelse(Pop%in%c("Dao","Hmong","PaThen"),"Hmong-Mien",Language))
data <- mutate(data,Language = ifelse(Pop%in%c("Cham","Ede","Giarai"),"Austronesian",Language))
data <- mutate(data,Language = ifelse(Pop%in%c("Cong","HaNhi","LaHu","LoLo","PhuLa","Sila"),"Sino-Tibetan",Language))
data <- mutate(data,Language = ifelse(Pop%in%c("KhoMu","Kinh","Mang","Muong"),"Austro-Asiatic",Language))




# plot
# Country
p <- ggplot(data, aes(x=PC1,y=PC2,color=Country,pch=Region))
p <- p + geom_point(alpha=0.75)
p <- p + theme(axis.line.x = element_line(color="black", size = 0.5, linetype = 1),
               axis.line.y = element_line(color="black", size = 0.5, linetype = 1))
p <- p + theme(panel.background = element_blank())
p <- p + labs(x = "PC1", y = "PC2")
p

# Region
p2 <- ggplot(data, aes(x=PC1,y=PC2,color=Region))
p2 <- p2 + scale_color_brewer(type='qual',palette='Paired')
p2 <- p2 + geom_point(alpha=0.75)
p2 <- p2 + theme(axis.line.x = element_line(color="black", size = 0.5, linetype = 1),
                 axis.line.y = element_line(color="black", size = 0.5, linetype = 1))
p2 <- p2 + theme(panel.background = element_blank())
p2 <- p2 + labs(x = "PC1", y = "PC2")
p2

# Pop, point
p3 <- ggplot(data, aes(x=PC1,y=PC2,color=Pop,pch=Type))
p3 <- p3 + geom_point(alpha=0.75)
colourCount = length(unique(data$Pop))
getPalette = colorRampPalette(brewer.pal(12, "Paired"))
p3 <- p3 + scale_color_manual(values = getPalette(colourCount))
p3 <- p3 + scale_shape_manual(values=c(0,19))
p3 <- p3 + theme(axis.line.x = element_line(color="black", size = 0.5, linetype = 1),
                 axis.line.y = element_line(color="black", size = 0.5, linetype = 1))
p3 <- p3 + theme(panel.background = element_blank()) + theme(legend.position='none')
p3 <- p3 + labs(x = "PC1", y = "PC2")
p3

# Pop, text
# Ref_ellipse: https://ggplot2.tidyverse.org/reference/stat_ellipse.html
p4 <- ggplot(data, aes(x=PC1,y=-PC2,color=Period))
p4 <- p4 + geom_text(aes(label=Pop), size=4, check_overlap=F) #+ stat_ellipse()
p4 <- p4 + scale_color_manual(values=c("#7FC97F","#006633","#FF3300","#FF9933","#386CB0","#F0027F","#660000","#666666"))
#p4 <- p4 + scale_color_brewer(palette="Paired")
#p4 <- p4 + scale_color_manual(values=c("#B2DF8A","#33A02C","#FB9A99","#E31A1C","#FDBF6F","#FF7F00","#CAB2D6"))
p4 <- p4 + theme(axis.line.x = element_line(color="black", size = 0.5, linetype = 1),
                 axis.line.y = element_line(color="black", size = 0.5, linetype = 1))
p4 <- p4 + theme(panel.background = element_blank()) 
p4 <- p4 + labs(x = "PC1", y = "PC2")
p4 <- p4 + theme(legend.text = element_text(size = 12), legend.title = element_text(size = 14))
p4 <- p4 + theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14))
p4 <- p4 + theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10))
p4

# Language, point, text
p5 <- ggplot(data, aes(x=PC1,y=-PC2,color=Language))
#p5 <- p5 + geom_point(alpha=0.75) 
p5 <- p5 + geom_text(aes(label=Pop),size=4,check_overlap=F) #+ stat_ellipse()
#colourCount = length(unique(data$Pop))
#getPalette = colorRampPalette(brewer.pal(12, "Paired"))
#p5 <- p5 + scale_color_manual(values = getPalette(colourCount))
p5 <- p5 + scale_color_manual(values=c("#111111","#9966CC","#CC6633","#FFCC33","#66CC99","#CC0033"))
#p5 <- p5 + scale_shape_manual(values=c(0,4,2,3,5,19))
p5 <- p5 + theme(axis.line.x = element_line(color="black", size = 0.5, linetype = 1),
                 axis.line.y = element_line(color="black", size = 0.5, linetype = 1))
p5 <- p5 + theme(panel.background = element_blank())
p5 <- p5 + labs(x = "PC1", y = "PC2")
p5 <- p5 + theme(legend.text = element_text(size = 12), legend.title = element_blank())
p5 <- p5 + theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14))
p5 <- p5 + theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10))
p5

# Language and Period, point, text
p6 <- ggplot(data, aes(x=PC1,y=-PC2,color=Language))
p6 <- p6 + geom_text(data=data[data$Type=="modern",], aes(label=Pop),size=4,check_overlap=F) #+ stat_ellipse()
p6 <- p6 + geom_point(data=data[data$Type=="ancient",], aes(pch=Period), alpha=0.75, size=3) 
#colourCount = length(unique(data$Pop))
#getPalette = colorRampPalette(brewer.pal(12, "Paired"))
#p5 <- p5 + scale_color_manual(values = getPalette(colourCount))
p6 <- p6 + scale_color_manual(values=c("#111111","#9966CC","#CC6633","#FFCC33","#66CC99","#CC0033"))
p6 <- p6 + scale_shape_manual(values=c(7,8,9,10,11,12,13))
#p5 <- p5 + scale_shape_manual(values=c(0,4,2,3,5,19))
p6 <- p6 + theme(axis.line.x = element_line(color="black", size = 0.5, linetype = 1),
                 axis.line.y = element_line(color="black", size = 0.5, linetype = 1))
p6 <- p6 + theme(panel.background = element_blank())
p6 <- p6 + labs(x = "PC1", y = "PC2")
p6 <- p6 + theme(legend.text = element_text(size = 12), legend.title = element_blank())
p6 <- p6 + theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14))
p6 <- p6 + theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10))
p6