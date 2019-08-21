# title: "Heatmap of outgroup f3 on GW data"
# author: "Dang Liu 25.Jul.2019"

# Last updated: 12.Aug.2019

# Use libraries
library(tidyverse)
library(pheatmap)
library(RColorBrewer)
library(gtools)
library(data.table)
library(ggrepel)
library(Rmisc)

# load data from a huge f3 comparing matrix
load("/mnt/scratch/dang/Vietnam/outgroup_f3/HO.ancient.outgroup.v2.Mbuti_f3.Rdata")
head(f3_res)
info <- read.table("/mnt/scratch/dang/Vietnam/map/all.map.am.geo.info2", header=T)
# Prepare data set for annotation and order
info <- info[!info$Pop %in% c("Mbuti","French"),] %>% select(Pop, Country)
d <- f3_res %>% select(A, B ,f3)
colnames(d) <- c("Pop", "Pop2", "f3")
d <- info %>% left_join(d)
d$Country <- factor(d$Country, levels=c("Taiwan","Mongolia","China","Vietnam","Cambodia","Laos","Thailand","Myanmar","Malaysia","Indonesia","Philippines","India"), ordered=T)
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

# Subsets
Vietnam <- c("Cham","Ede","Giarai","KhoMu","Kinh","Mang","Muong","BoY","CoLao","LaChi","Nung","Tay","Thai","Dao","Hmong","PaThen","Cong","HaNhi","LaHu","LoLo","PhuLa","Sila")
Near_pop <- c("GujaratiA","GujaratiB","GujaratiC","GujaratiD","Lodhi","Brahmin_Tiwari","Jew_Cochin","Vishwabrahmin","Mala","Kharia","Onge","Dai","She","Miao","Lahu","Naxi","Yi","Tujia","Han","Han_NC","Xibo","Hezhen","Oroqen","Tu","Daur","Mongola","Ami","Atayal","Mamanwa","Borneo","Semende","Thai_T","Mlabri","Htin_Mal","Cambodian")
Ancient <- c("P-Tianyuan","Ho-Pha_Faen","Ho-Gua_Cha","N-Gua_Cha","N-Man_Bac","N-Nam_Tun","N-Mai_Da_Dieu","N-Hon_Hai_Co_Tien","N-Tam_Pa_Ling","N-Tam_Hang","N-Oakaie","N-Loyang_Ujung","BA-Nui_Nap","IA-Vat_Komnou","IA-Long_Long_Rak","Hi-Hon_Hai_Co_Tien","Hi-Supu_Hujung","Hi-Kinabatagan")

all <- c(Vietnam, Near_pop, Ancient)

# Order the matrix
#d2.matrix <- d2.matrix[unique(d$Pop),unique(d$Pop)]
#d3.matrix <- d2.matrix[Vietnam,Near_pop]
#d3.matrix <- d2.matrix[Vietnam,Ancient]
#d3.matrix <- d2.matrix[Vietnam,Vietnam]
d3.matrix <- d2.matrix[Vietnam,all]

# Deal with 0 (same pops vs. each other will result in outgroup f3=0; substitute it to maximum + 0.01)
norm_data <- d3.matrix
n <- 0
c <- 0
while(n<nrow(d3.matrix)){
  n <- n+1
  while(c<ncol(d3.matrix)){
    c <- c+1
    if (norm_data[n,c] == 0){norm_data[n,c] = NA}
    #norm_data[,n] <- 1-(norm_data[,n]+(0-min(data[,n])))/max(data[,n]+(0-min(data[,n])))
  }
  c <- 0
}

# Normalizing by columns
#n <- 0
#while(n<ncol(norm_data)){
#  n <- n+1
#  norm_data[,n] <- (norm_data[,n]+(0-min(norm_data[,n],na.rm=T)))/max(norm_data[,n]+(0-min(norm_data[,n],na.rm=T)),na.rm=T)
#}

# Normalizing by rows
#n <- 0
#while(n<nrow(norm_data)){
#  n <- n+1
#  norm_data[n,] <- (norm_data[n,]+(0-min(norm_data[n,],na.rm=T)))/max(norm_data[n,]+(0-min(norm_data[n,],na.rm=T)),na.rm=T)
#}

# annotaion here
info2 <- read_csv("/r1/people/dang_liu/Projects/Vietnam/Vietnam.metadata.csv")
#annotation <- info[!duplicated(info), ] %>% remove_rownames %>% column_to_rownames(var="Pop")
annotation_col <- info2 %>% select(Pop, Language, Period, Country) %>% distinct(Pop, .keep_all = TRUE) %>% remove_rownames %>% column_to_rownames(var="Pop")
annotation_row <- info2 %>% select(Pop, Language) %>% distinct(Pop, .keep_all = TRUE) %>% remove_rownames %>% column_to_rownames(var="Pop")
# Specify colors
ann_colors = list(
  Country = c(Taiwan="#A6CEE3",Mongolia="#CC9900",China="#1F78B4",Vietnam="#B2DF8A",Cambodia="#33A02C",Laos="#FB9A99",Thailand="#E31A1C",Myanmar="#FDBF6F",Malaysia="#FF7F00",Indonesia="#CAB2D6",Philippines="#6A3D9A",India="#B15928"),
  Period = c(Present="#7FC97F",Historical="#006633",Iron_Age="#FF3300",Bronze_Age="#FF9933",Neolithic="#386CB0",Hoabinhian="#F0027F",Paleolithic="#660000"),
  Language = c(Austronesian="#CC6633","Austro-Asiatic"="#9966CC","Hmong-Mien"="#FFCC33","Sino-Tibetan"="#66CC99","Tai-Kadai"="#CC0033",
               "Indo-European"="#FFCCFF",Dravidian="#CC0099",Andamanese="#660033",Mongolic="#0000FF",Tungusic="#66CCFF",Ancient="#000000")
)

# plot heatmap
res <- pheatmap(
  color= colorRampPalette(c("#4575B4", "#66CCFF", "#FFEDA0", "#D73027"))(25),
  # Alternative color choice: "#FFFFCC", "#FEB24C", "#E31A1C"
  fontsize_number=14,
  fontsize_row=16,
  fontsize_col=16,
  #cellwidth=20,
  #cellheight=20,
  norm_data,
  #display_numbers=as.matrix(df.mat),
  #annotation=annotation,
  #angle_col = "45",
  gaps_col=c(22,57),
  annotation_col=annotation_col, 
  annotation_row=annotation_row,
  annotation_colors=ann_colors, 
  na_col="white",
  cluster_rows=FALSE, cluster_cols=FALSE
)
