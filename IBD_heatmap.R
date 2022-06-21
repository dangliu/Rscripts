# title: "Heatmap of IBD on GW data"
# author: "Dang Liu 19.Mar.2020"

# Last updated: 19.Mar.2020

# Use libraries
library(pheatmap)
library(tidyverse)
library(RColorBrewer)
library(gtools)
library(data.table)
library(ggrepel)
library(Rmisc)


# Read between data
data <- read.table("/r1/people/wibhu_kutanan/Stonekg_db/IBD/all.lPSC.Merged.least.2cM.stats", header=T, stringsAsFactors=F)
head(data)
#info <- read.table("/mnt/scratch/dang/Vietnam/map/all.map.am.geo.info3", header=T, stringsAsFactors=F)
# Prepare data set
#notHO <- unique(info[info$Platform!="HO" & !info$Pop%in%c("Borneo","Semende","Mamanwa","Ami","Atayal"),]$Pop)
Thai <- c("Hmong","IuMien","SouthernThai_Muslim","Moken","Lisu","Mussur","Karen","Lawa","Palaung","Blang","Khmu","Htin","Soa","Bru","Mon","Yuan","Khuen","Phuan","Shan","Khonmueang","Lue","BlackTai","Laotian","LaoIsan","Phutai","Nyaw","Seak","Kalueang","CentralThai","Thai","SouthernThai_Buddhisht")
#Thai <- c("Hmong","SouthernThai_Muslim","Moken","Karen","Lawa","Palaung","Blang","Khmu","Htin","Soa","Bru","Mon","Yuan","Khuen","Phuan","Shan","Khonmueang","Lue","BlackTai","Laotian","LaoIsan","Phutai","Nyaw","Seak","Kalueang","CentralThai","Thai","SouthernThai_Buddhisht")
D <- c("Brahmin_Tiwari","Gujarati","Vishwabrahmin","Lodhi","Mala","Kharia","Mamanwa","Semende","Borneo","Ami","Atayal","Mlabri","HtinMal","Thai","Cambodian","Dai","Miao","She","Lahu","Yi","Naxi","Han","Tujia","Tu","Xibo","Uygur","Mongola","Hezhen","Daur","Oroqen","Japanese")
d <- data %>% filter(Pop1%in%Thai,Pop2%in%D)

# Adjust for the boundry of maximum
n <- 0
for(i in d$Pop1){
  n <- n + 1
  d[n,]$Average <- ifelse(d[n,]$Average > 30, 30, d[n,]$Average)
}
 
# Make a matrix for heatmap function
d.matrix <- d %>% select(Pop1, Pop2, Average) %>% dcast(Pop1 ~ Pop2, value.var = "Average") %>% remove_rownames %>% column_to_rownames(var="Pop1") %>% data.matrix()
#d.matrix <- d %>% select(Pop1, Pop2, Total) %>% dcast(Pop1 ~ Pop2, value.var = "Total") %>% remove_rownames %>% column_to_rownames(var="Pop1") %>% data.matrix()
head(d.matrix)
d.matrix[is.na(d.matrix)] <- 0
head(d.matrix)
# Order the matrix
d.matrix <- d.matrix[Thai,D]


# Deal with 0 (same pops vs. each other will result in outgroup f3=0; substitute it to maximum + 0.01)
#norm_data <- d2.matrix
#n <- 0
#c <- 0
#while(n<nrow(d2.matrix)){
#  n <- n+1
#  while(c<ncol(d2.matrix)){
#    c <- c+1
#    if (norm_data[n,c] == 0){norm_data[n,c] = NA}
#    #norm_data[,n] <- 1-(norm_data[,n]+(0-min(data[,n])))/max(data[,n]+(0-min(data[,n])))
#  }
#  c <- 0
#}

# Normalizing 
#n <- 0
#while(n<ncol(norm_data)){
#  n <- n+1
#  norm_data[,n] <- 1-(norm_data[,n]+(0-min(norm_data[,n],na.rm=T)))/max(norm_data[,n]+(0-min(norm_data[,n],na.rm=T)),na.rm=T)
#}


# Transpose
t_d_matrix <- t(d.matrix)



# Heatmap by pheatmap
# Ref: https://cran.r-project.org/web/packages/pheatmap/pheatmap.pdf
# Ref2: https://www.rdocumentation.org/packages/COMPASS/versions/1.10.2/topics/pheatmap

# annotaion here
info2 <- read.table("/home/dang_liu/Projects/Thailand/ThaiCompare.info2.txt", header=T, stringsAsFactors=F)
annotation_col <- info2 %>% select(Pop, Language) %>% distinct(Pop, .keep_all = TRUE) %>% remove_rownames %>% column_to_rownames(var="Pop")
annotation_row <- info2 %>% select(Pop, Country) %>% distinct(Pop, .keep_all = TRUE) %>% remove_rownames %>% column_to_rownames(var="Pop")
# Specify colors
ann_colors = list(
  Country = c(Japan="#006633",Taiwan="#A6CEE3",Mongolia="#CC9900",China="#1F78B4",Vietnam="#B2DF8A",Cambodia="#33A02C",Thailand="#E31A1C",Indonesia="#CAB2D6",Philippines="#6A3D9A",India="#B15928"),
  Language = c(Austronesian="#CC6633","Austroasiatic"="#9966CC","Hmong-Mien"="#FFCC33","Sino-Tibetan"="#66CC99","Tai-Kadai"="#CC0033")
  #"Indo-European"="#FFCCFF",Dravidian="#CC0099",Andamanese="#660033",Mongolic="#0000FF",Tungusic="#66CCFF",Turkic="#336600",Japonic="#FF0099",Ancient="#000000")
)

# plot heatmap
res <- pheatmap(
  color= colorRampPalette(c("navy", "white", "firebrick3"))(10),
  fontsize_number=12,
  fontsize_row=12,
  fontsize_col=12,
  cellwidth=NA,
  cellheight=NA,
  as.matrix(t_d_matrix),
  annotation_col=annotation_col, 
  annotation_row=annotation_row,
  annotation_colors=ann_colors,
  legend_breaks=c(0,5,10,15,20,25,30),
  legend_labels=c("0","5","10","15","20","25",">30"),  
  angle_col=90,
  cluster_rows=FALSE, cluster_cols=FALSE
)

