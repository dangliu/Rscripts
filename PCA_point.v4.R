# title: "PCA on GW data"
# author: "Dang Liu 16.Jan.2019"

# Last updated: 25.Feb.2019

# Use libraries
library(RColorBrewer)
library(tidyverse)
library(stringr)

# Read data
data <- read.table("/mnt/scratch/dang/Vietnam/pca/ancient_proj/SEA.SC.TW.merged3.am.nodrift.lsqproj.evec",header=F)
info <- read.table("/mnt/scratch/dang/Vietnam/pca/ancient_proj/SEA.SC.TW.merged3.am.filtered.info", header=T)
# Arrange data
colnames(data) <- c("IID","PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8","PC9","PC10","C")
data <- data[,!(colnames(data)=="C")] 
data <- info %>% left_join(data)
head(data)
data <- data[data$IID!="I2497",]
data$Country <- factor(data$Country, levels=c("Taiwan","China","Vietnam","Cambodia","Laos","Thailand","Myanmar","Malaysia","Indonesia","Philippines"))
brewer.pal(n = 10, name = "Paired")

data$Language <- "Not-Available"
data <- mutate(data,Language = ifelse(Pop%in%c("BoY","CoLao","LaChi","Nung","Tay","Thai"),"Tai-Kadai",Language))
data <- mutate(data,Language = ifelse(Pop%in%c("Dao","Hmong","PaThen"),"Hmong-Mien",Language))
data <- mutate(data,Language = ifelse(Pop%in%c("Cham","Ede","Giarai"),"Austronesian",Language))
data <- mutate(data,Language = ifelse(Pop%in%c("Cong","HaNhi","LaHu","LoLo","PhuLa","Sila"),"Sino-Tibetan",Language))
data <- mutate(data,Language = ifelse(Pop%in%c("KhoMu","Kinh","Mang","Muong"),"Autro-Asiatic",Language))

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
p4 <- ggplot(data, aes(x=PC1,y=-PC2,color=Country))
p4 <- p4 + geom_text(aes(label=Pop), vjust = 0, nudge_y = 0.0025, size=4, check_overlap=F) #+ stat_ellipse()
p4 <- p4 + geom_point(aes(color=Country,pch=Type))
p4 <- p4 + scale_color_brewer(palette="Paired")
p4 <- p4 + scale_shape_manual(values=c(19,4))
p4 <- p4 + theme(axis.line.x = element_line(color="black", size = 0.5, linetype = 1),
                 axis.line.y = element_line(color="black", size = 0.5, linetype = 1))
p4 <- p4 + theme(panel.background = element_blank()) 
p4 <- p4 + labs(x = "PC1", y = "PC2")
p4 <- p4 + theme(legend.text = element_text(size = 12), legend.title = element_text(size = 14))
p4 <- p4 + theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14))
p4 <- p4 + theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10))
p4

# Language, point, text
p5 <- ggplot(data, aes(x=PC1,y=PC2,color=Language))
#p5 <- p5 + geom_point(alpha=0.75) 
p5 <- p5 + geom_text(aes(label=Pop),size=3,check_overlap=F) #+ stat_ellipse()
#colourCount = length(unique(data$Pop))
#getPalette = colorRampPalette(brewer.pal(12, "Paired"))
#p5 <- p5 + scale_color_manual(values = getPalette(colourCount))
p5 <- p5 + scale_color_manual(values=c("#CC6633","#9966CC","#FFCC33","#66CC99","#CC0033"))
#p5 <- p5 + scale_shape_manual(values=c(0,4,2,3,5,19))
p5 <- p5 + theme(axis.line.x = element_line(color="black", size = 0.5, linetype = 1),
                 axis.line.y = element_line(color="black", size = 0.5, linetype = 1))
p5 <- p5 + theme(panel.background = element_blank())
p5 <- p5 + labs(x = "PC1", y = "PC2")
p5

#################################################################################

# Read data
data <- read.table("/mnt/scratch/dang/Vietnam/pca/ancient_proj/all.Vietnam.lsqproj.evec",header=F)
info <- read.table("/mnt/scratch/dang/Vietnam/pca/ancient_proj/Vietnam.am.info", header=T)
# Arrange data
colnames(data) <- c("IID","PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8","PC9","PC10","C")
data <- data[,!(colnames(data)=="C")] #%>% separate(FIID, c("FID", "IID"), ":", convert=T)
data <- info %>% left_join(data)
head(data)

data <- data[data$IID!="I2497",]

data$Country <- factor(data$Country, levels=c("Taiwan","China","Vietnam","Cambodia","Laos","Thailand","Myanmar","Malaysia","Indonesia","Philippines"))
data$Period <- factor(data$Period, levels=c("P","Hi","IA","BA","LN","N","Ho"), ordered=T)

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
p4 <- p4 + scale_color_manual(values=c("#999999","#33A02C","#FB9A99","#663300","#CCCC33","#FF7F00","#3288BD"))
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
