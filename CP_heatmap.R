# Script to visualize Chromopainter painting results
# Last updated: 11.Mar.2020

# Use libraries
library(pheatmap)
library(RColorBrewer)
library(gtools)
library(data.table)
library(ggrepel)
library(Rmisc)
library(tidyverse)

# Input files
#CPI <- read.table("/mnt/scratch/dang/Thailand/Chromopainter/Thai_R/CP_Thai_R.chunklengths.out", header=T, stringsAsFactors=F)
CPI <- read.table("/mnt/scratch/dang/Thailand/Chromopainter/Thai_R/CP_Wib_Thai_R.chunklengths.out", header=T, stringsAsFactors=F)

info <- read.table("/home/dang_liu/Projects/Thailand/ThaiCompare.info2-4.txt", header=T, stringsAsFactors=F)

# list donors and reciepients
#D <- c("Brahmin_Tiwari","Gujarati","Vishwabrahmin","Lodhi","Mala","Kharia","Mamanwa","Semende","Borneo","Ami","Atayal","Cambodian","Dai","Miao","She","Lahu","Yi","Naxi","Han","Tujia","Tu","Xibo","Uygur","Mongola","Hezhen","Daur","Oroqen","Japanese")
#R <- c("Hmong","IuMien","SouthernThai_Muslim","Moken","Karen","Lisu","Mussur","Khmu","Lawa","Palaung","Blang","Htin","HtinMal","Mlabri","Soa","Bru","Mon","Khonmueang","Phutai","Laotian","LaoIsan","Yuan","Lue","Khuen","Seak","Nyaw","Kalueang","BlackTai","Phuan","CentralThai","Thai","SouthernThai_Buddhisht","Shan")
D <- c("Brahmin_Tiwari","Gujarati","Vishwabrahmin","Lodhi","Mala","Kharia","Mamanwa","Semende","Borneo","Ami","Atayal","Mlabri","HtinMal","Thai","Cambodian","Dai","Miao","She","Lahu","Yi","Naxi","Han","Tujia","Tu","Xibo","Uygur","Mongola","Hezhen","Daur","Oroqen","Japanese")
R <- c("Hmong","SouthernThai_Muslim","Moken","Karen","Khmu","Lawa","Palaung","Blang","Htin","Soa","Bru","Mon","Khonmueang","Phutai","Laotian","LaoIsan","Yuan","Lue","Khuen","Seak","Nyaw","Kalueang","BlackTai","Phuan","CentralThai","Thai","SouthernThai_Buddhisht","Shan")

# Assign pops for the rows
colnames(CPI)[1] <- "IID"
#CPI_info <- info %>% select(IID, Pop) %>% left_join(CPI) %>% arrange(desc(Pop)) %>% drop_na()
CPI_info <- info %>% select(IID, Pop, Language, Country) %>% left_join(CPI) %>% arrange(desc(Pop)) %>% drop_na() %>% filter(Pop%in%R)

# Make matrix
#d <- CPI_info[,3:ncol(CPI_info)]
d <- CPI_info[,5:ncol(CPI_info)]
rownames(d) <- CPI_info$IID
d_matrix <- as.matrix(d)

# Order it
CPI_info$Pop <- factor(CPI_info$Pop, levels=R, ordered=T)
CPI_info <- CPI_info[order(CPI_info$Pop),]

order_d_matrix <- d_matrix[CPI_info$IID, D]

# Transpose
t_d_matrix <- t(order_d_matrix)



# Heatmap by pheatmap
# Ref: https://cran.r-project.org/web/packages/pheatmap/pheatmap.pdf
# Ref2: https://www.rdocumentation.org/packages/COMPASS/versions/1.10.2/topics/pheatmap

# annotaion here
info2 <- read_csv("/r1/people/dang_liu/Projects/Thailand/ThaiCompare.info2-4.csv")
annotation_col <- info2 %>% select(IID, Language) %>% distinct(IID, .keep_all = TRUE) %>% remove_rownames %>% column_to_rownames(var="IID")
annotation_row <- info2 %>% select(Pop, Country) %>% distinct(Pop, .keep_all = TRUE) %>% remove_rownames %>% column_to_rownames(var="Pop")
# Specify colors
ann_colors = list(
  Country = c(Japan="#006633",Taiwan="#A6CEE3",Mongolia="#CC9900",China="#1F78B4",Vietnam="#B2DF8A",Cambodia="#33A02C",Thailand="#E31A1C",Indonesia="#CAB2D6",Philippines="#6A3D9A",India="#B15928"),
  Language = c(Austronesian="#CC6633","Austroasiatic"="#9966CC","Hmong-Mien"="#FFCC33","Sino-Tibetan"="#66CC99","Tai-Kadai"="#CC0033")
               #"Indo-European"="#FFCCFF",Dravidian="#CC0099",Andamanese="#660033",Mongolic="#0000FF",Tungusic="#66CCFF",Turkic="#336600",Japonic="#FF0099",Ancient="#000000")
)

# Labels for columns
u_data <- CPI_info
u_data$Pop <- as.character(CPI_info$Pop)
u_data$Pop[duplicated(u_data$Pop)] <- ""
labels_col = u_data$Pop




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
  labels_col=labels_col,
  angle_col=90,
  cluster_rows=FALSE, cluster_cols=FALSE
)
