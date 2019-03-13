# author: "Dang Liu 11.Mar.2019"

# Last updated: 12.Mar.2019

# Use libraries
library(tidyverse)
library(RColorBrewer)
library(gtools)
library(ggmap)
library(maps)
library(ggrepel)


# Read between data
data <- read.table("/mnt/scratch/dang/Vietnam/IBD/Merged.pop.ibd.2cM.pair.stats", header=T)
head(data)

# Pop info
info <- read.table("/mnt/scratch/dang/Vietnam/outgroup/HO.ancient.outgroup.geo.info", header=T)
info2 <- info %>% group_by(Pop) %>% 
  summarise_at(vars(Latitude,Longitude), funs(median(.))) %>% 
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
#p <- p + facet_wrap(.~Pop1, ncol=3)
p <- p + facet_wrap(.~Pop1, nrow=4)
p <- p + labs(fill="Block number mean (n)", size="Total length mean (cM)")
p <- p + theme(legend.text = element_text(size = 12), legend.title = element_text(size = 14))
p <- p + theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14))
p <- p + theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10))
p <- p + theme(strip.text.x = element_text(size = 12), strip.text.y = element_text(size=12))
p <- p + xlab("Longitude") + ylab("Latitude")
p


#######################################################################################################

# Read len data

data <- read.table("/mnt/scratch/dang/Vietnam/IBD/Merged.pop.ibd.2cM.pair.L.N", header=T)

data$Country2 <- factor(data$Country2, levels=c("Taiwan","China","Vietnam","Cambodia","Laos","Thailand","Myanmar","Malaysia","Indonesia","Philippines","India"), ordered=T)
data <- data[order(data$Country2),]

# Exclude pops here
pop_exclude <- c("Atayal1","Ami1","Paiwan","Bunun","Rukai","Tao","Mamanwa1")
data <- data[!data$Pop1%in%pop_exclude,]
data <- data[!data$Pop2%in%pop_exclude,]
data <- data[data$Country1=="Vietnam",]
data <- data[data$Pop1!=data$Pop2,]
head(data)

average <- data %>% group_by(Pop1, Pop2) %>% 
  summarise_at(vars(Length, N_ind), funs(mean(.)))
colnames(average) <- c("Pop1", "Pop2", "A_L", "A_N")
average$A_N <- round(average$A_N)
data <- data %>% left_join(average)

data$Pop1 <- factor(data$Pop1, levels=c("BoY","CoLao","LaChi","Nung","Tay","Thai","Dao","Hmong","PaThen","Cham","Ede","Giarai","Cong","HaNhi","LaHu","LoLo","PhuLa","Sila","KhoMu","Kinh","Mang","Muong"), ordered=T)

# Add language groups for Vietnam only
data$Language <- "NA"
data <- mutate(data,Language = ifelse(Pop1%in%c("BoY","CoLao","LaChi","Nung","Tay","Thai"),"Tai-Kadai",Language))
data <- mutate(data,Language = ifelse(Pop1%in%c("Dao","Hmong","PaThen"),"Hmong-Mien",Language))
data <- mutate(data,Language = ifelse(Pop1%in%c("Cham","Ede","Giarai"),"Austronesian",Language))
data <- mutate(data,Language = ifelse(Pop1%in%c("Cong","HaNhi","LaHu","LoLo","PhuLa","Sila"),"Sino-Tibetan",Language))
data <- mutate(data,Language = ifelse(Pop1%in%c("KhoMu","Kinh","Mang","Muong"),"Austro-Asiatic",Language))

# Subset data here
#data <- data[data$Language=="Tai-Kadai",]
#data <- data[data$Language=="Hmong-Mien",]
#data <- data[data$Language=="Austronesian",]
#data <- data[data$Language=="Sino-Tibetan",]
#data <- data[data$Language=="Austro-Asiatic",]


p <- ggplot(data, aes(x = Pop2, y = Length, fill=A_N))
p <- p + geom_boxplot(outlier.size=0.5, outlier.shape=16, notch=F)
p <- p + coord_cartesian(ylim=c(2,100))
p <- p + labs(x=NULL, y="Total block length (cM)", fill="Block number mean (n)")
p <- p + scale_fill_gradient(low="lightblue", high="red")
p <- p + theme(axis.line.x = element_line(color="black", size = 0.5, linetype = 1),
               axis.line.y = element_line(color="black", size = 0.5, linetype = 1))
#p <- p + facet_wrap(.~Pop1, ncol=3, scales="free_x")
p <- p + facet_wrap(.~Pop1, nrow=4, scales="free_x")
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
data <- read.table("/mnt/scratch/dang/Vietnam/IBD/Merged.pop.ibd.2cM.pair.L.N", header=T)
data <- data[data$Country1=="Vietnam",]
data <- data[data$Pop1==data$Pop2,]
head(data)

average <- data %>% group_by(Pop1, Pop2) %>% 
  summarise_at(vars(Length, N_ind), funs(mean(.)))
colnames(average) <- c("Pop1", "Pop2", "A_L", "A_N")
average$A_N <- round(average$A_N)
data <- data %>% left_join(average)


#d2 <- data[order(data$N_ind),]

p <- ggplot(data, aes(x = reorder(Pop1, Length, FUN=median), y = Length, fill=A_N))
p <- p + geom_boxplot(outlier.size=0.5, outlier.shape=16, notch=F)
sts <- boxplot.stats(data$Length)$stats
p <- p + coord_cartesian(ylim=c(min(sts)*1.25,max(sts)*1.25))
p <- p + labs(x=NULL, y="Total block length (cM)", fill="Block number mean (n)")
p <- p + scale_fill_gradient(low="lightblue", high="red")
p <- p + theme(axis.line.x = element_line(color="black", size = 0.5, linetype = 1),
               axis.line.y = element_line(color="black", size = 0.5, linetype = 1))
p <- p + theme(panel.background = element_blank())
p <- p + theme(legend.text = element_text(size = 12), legend.title = element_text(size = 12))
p <- p + theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14))
p <- p + theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))
p <- p + theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1))
#p <- p + scale_x_discrete(limits=unique(d2$Pop))
p

#######################################################################################################

# Plot within pop number vs. length to infer demography

# Add language groups for Vietnam only
average$Language <- "NA"
average <- mutate(average,Language = ifelse(Pop1%in%c("BoY","CoLao","LaChi","Nung","Tay","Thai"),"Tai-Kadai",Language))
average <- mutate(average,Language = ifelse(Pop1%in%c("Dao","Hmong","PaThen"),"Hmong-Mien",Language))
average <- mutate(average,Language = ifelse(Pop1%in%c("Cham","Ede","Giarai"),"Austronesian",Language))
average <- mutate(average,Language = ifelse(Pop1%in%c("Cong","HaNhi","LaHu","LoLo","PhuLa","Sila"),"Sino-Tibetan",Language))
average <- mutate(average,Language = ifelse(Pop1%in%c("KhoMu","Kinh","Mang","Muong"),"Austro-Asiatic",Language))


p <- ggplot(average, aes(x = A_L, y = A_N, color=Language))
p <- p + geom_text(aes(label=Pop1), vjust = 1.5, nudge_y = 0.0025, size=5)
p <- p + geom_point(size=4,alpha=0.8)
p <- p + scale_color_manual(values=c("#9966CC","#CC6633","#FFCC33","#66CC99","#CC0033"))
p <- p + labs(x="Total length mean (cM)", y="Block number mean (n)", color="Language group")
p <- p + theme(axis.line.x = element_line(color="black", size = 0.5, linetype = 1),
               axis.line.y = element_line(color="black", size = 0.5, linetype = 1))
p <- p + theme(panel.background = element_blank())
p <- p + theme(legend.text = element_text(size = 12), legend.title = element_text(size = 12))
p <- p + theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14))
p <- p + theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))
p

