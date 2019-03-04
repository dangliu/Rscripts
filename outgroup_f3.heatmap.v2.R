# title: "Heatmap of outgroup f3 on GW data"
# author: "Dang Liu 11.Feb.2019"

# Last updated: 01.Mar.2019

# Use libraries
library(tidyverse)
library(RColorBrewer)
library(pheatmap)
library(gtools)

# load data from a huge f3 comparing matrix
load("/mnt/scratch/dang/Vietnam/outgroup_f3/HO.ancient.outgroup.French_f3.Rdata")
head(f3_res)
info <- read.table("/mnt/scratch/dang/Vietnam/outgroup/HO.ancient.outgroup.info", header=T)

# Prepare data set for annotation and order
info <- info[!info$Pop %in% c("Mbuti","French"),] %>% select(Pop, Country)
d <- f3_res %>% select(A, B ,f3)
colnames(d) <- c("Pop", "Pop2", "f3")
d <- info %>% left_join(d)
d$Country <- factor(d$Country, levels=c("Taiwan","China","Vietnam","Cambodia","Laos","Thailand","Myanmar","Malaysia","Indonesia","Philippines","India"), ordered=T)
d <- d[order(d$Country),]
# Exclude pops here
pop_exclude <- c("Atayal1","Ami1","Paiwan","Bunun","Rukai","Tao","Mamanwa1")
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
    #norm_data[,n] <- (norm_data[,n]+(0-min(data[,n])))/max(data[,n]+(0-min(data[,n])))
  }
  c <- 0
}

# Normalizing 
#n <- 0
#while(n<ncol(norm_data)){
#  n <- n+1
#  norm_data[,n] <- (norm_data[,n]+(0-min(norm_data[,n],na.rm=T)))/max(norm_data[,n]+(0-min(norm_data[,n],na.rm=T)),na.rm=T)
#}


# Heatmap by pheatmap
# Ref: https://cran.r-project.org/web/packages/pheatmap/pheatmap.pdf
# Ref2: https://www.rdocumentation.org/packages/COMPASS/versions/1.10.2/topics/pheatmap

# annotaion here
annotation <- info[!duplicated(info), ] %>% remove_rownames %>% column_to_rownames(var="Pop")
# Specify colors
ann_colors = list(
  Country = c(Taiwan="#A6CEE3",China="#1F78B4",Vietnam="#B2DF8A",Cambodia="#33A02C",Laos="#FB9A99",Thailand="#E31A1C",Myanmar="#FDBF6F",Malaysia="#FF7F00",Indonesia="#CAB2D6",Philippines="#6A3D9A",India="#FFFF99")
)

# plot heatmap
res <- pheatmap(
  color= colorRampPalette(c("#4575B4", "#FFEDA0", "#D73027"))(10),
  # Alternative color choice: "#FFFFCC", "#FEB24C", "#E31A1C"
  fontsize_number=12,
  fontsize_row=12,
  fontsize_col=12,
  cellwidth=NA,
  cellheight=NA,
  norm_data,
  #display_numbers=as.matrix(df.mat),
  annotation=annotation,
  annotation_colors=ann_colors, 
  na_col="white",
  cluster_rows=FALSE, cluster_cols=FALSE
)
# Cutout the k clusters 
#IID.clust <- cbind(norm_data_n,cluster=cutree(res$tree_col,k=8))
#View(IID.clust)


# Heatmap by heatmap
library(gplots)
heatmap(norm_data, symm=T, col=bluered(15))
