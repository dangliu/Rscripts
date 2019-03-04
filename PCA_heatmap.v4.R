# title: "Heatmap of PCA on GW data"
# author: "Dang Liu 14.Dec.2018"

# Last updated: 25.Feb.2019

# Use libraries
library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(pheatmap)
library(gtools)


# Read data
data <- read.table("/mnt/scratch/dang/Vietnam/pca/ancient_proj/SEA.SC.TW.merged3.am.nodrift.lsqproj.evec",header=F)
info <- read.table("/mnt/scratch/dang/Vietnam/pca/ancient_proj/SEA.SC.TW.merged3.am.filtered.info", header=T)
# Arrange data
colnames(data) <- c("IID","PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8","PC9","PC10","C")
data <- data[,!(colnames(data)=="C")] 
data <- info[,!(colnames(info)=="Type")] %>% left_join(data)
head(data)
data <- data[data$IID!="I2497",]

# Sort data by country
# Ref: https://stackoverflow.com/questions/1296646/how-to-sort-a-dataframe-by-multiple-columns
# Ref2: https://www.statmethods.net/management/sorting.html
data$Country <- factor(data$Country, levels=c("Taiwan","China","Vietnam","Cambodia","Laos","Thailand","Myanmar","Malaysia","Indonesia","Philippines"))
data <- data[order(data$Country),]

# Normalizing the sample PC vectors on each PC
norm_data <- data
n <- 5
while(n<ncol(data)){
  n <- n+1
  norm_data[,n] <- (norm_data[,n]+(0-min(data[,n])))/max(data[,n]+(0-min(data[,n])))
}

# Subset the information with only IID and PCs
# Trun them into a matrix
norm_data_n <- norm_data[,6:ncol(data)]
rownames(norm_data_n) <- data$IID
norm_data_n <- as.matrix(norm_data_n)
# Transpose
# Ref: https://www.r-statistics.com/tag/transpose/
t_norm_data_n <- t(norm_data_n)

# arrange row
rn <- mixedsort(rownames(t_norm_data_n),decreasing=T)


#melt_norm_data <- melt(norm_data,id=1:5,measure=6:ncol(norm_data),variable.name="PCs",value.name="Evec")
#df <- melt_norm_data[,c("IID","PCs","Evec")]
#df.mat <- dcast(df, PCs~IID)
#rownames(df.mat) <- df.mat[,"PCs"]
#df.mat$PCs <- NULL
#df.mat <- as.matrix(df.mat)


# Heatmap by pheatmap
# Ref: https://cran.r-project.org/web/packages/pheatmap/pheatmap.pdf
# Ref2: https://www.rdocumentation.org/packages/COMPASS/versions/1.10.2/topics/pheatmap

# annotaion here
annotation <- norm_data %>% select(Country)
rownames(annotation) <- rownames(norm_data_n)
# Specify colors
ann_colors = list(
  Country = c(Taiwan="#A6CEE3",China="#1F78B4",Vietnam="#B2DF8A",Cambodia="#33A02C",Laos="#FB9A99",Thailand="#E31A1C",Myanmar="#FDBF6F",Malaysia="#FF7F00",Indonesia="#CAB2D6",Philippines="#6A3D9A")
)

# plot heatmap
res <- pheatmap(
  color= colorRampPalette(c("navy", "white", "firebrick3"))(10),
  fontsize_number=12,
  fontsize_row=12,
  fontsize_col=6,
  cellwidth=NA,
  cellheight=NA,
  as.matrix(t_norm_data_n),
  #display_numbers=as.matrix(df.mat),
  annotation=annotation,
  annotation_colors=ann_colors, 
  cluster_rows=FALSE, cluster_cols=FALSE
)
# Cutout the k clusters 
IID.clust <- cbind(norm_data_n,cluster=cutree(res$tree_col,k=8))
View(IID.clust)

# Heatmap by ggplot
#p <- ggplot(, aes(IID, PCs)) + geom_tile(aes(fill = Evec))
#p

# Heatmap by heatmap
df_heatmap <- heatmap(norm_data_n, Rowv=NA, Colv=NA, col = heat.colors(256), scale="column", margins=c(5,10))

###############################################################################

# Read data
data <- read.table("/mnt/scratch/dang/Vietnam/pca/ancient_proj/all.Vietnam.lsqproj.evec",header=F)
info <- read.table("/mnt/scratch/dang/Vietnam/pca/ancient_proj/Vietnam.am.info", header=T)
# Arrange data
colnames(data) <- c("IID","PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8","PC9","PC10","C")
data <- data[,!(colnames(data)=="C")] 
data <- info[,!(colnames(info)=="Type")] %>% left_join(data)
head(data)
data <- data[data$IID!="I2497",]

# Sort data by country
# Ref: https://stackoverflow.com/questions/1296646/how-to-sort-a-dataframe-by-multiple-columns
# Ref2: https://www.statmethods.net/management/sorting.html
data$Period <- factor(data$Period, levels=c("P","Hi","IA","BA","LN","N","Ho"))
data <- data[order(data$Period),]

# Normalizing the sample PC vectors on each PC
norm_data <- data
n <- 7
while(n<ncol(data)){
  n <- n+1
  norm_data[,n] <- (norm_data[,n]+(0-min(data[,n])))/max(data[,n]+(0-min(data[,n])))
}

# Subset the information with only IID and PCs
# Trun them into a matrix
norm_data_n <- norm_data[,8:ncol(data)]
rownames(norm_data_n) <- data$IID
norm_data_n <- as.matrix(norm_data_n)
# Transpose
# Ref: https://www.r-statistics.com/tag/transpose/
t_norm_data_n <- t(norm_data_n)

# arrange row
rn <- mixedsort(rownames(t_norm_data_n),decreasing=T)


# Heatmap by pheatmap
# Ref: https://cran.r-project.org/web/packages/pheatmap/pheatmap.pdf
# Ref2: https://www.rdocumentation.org/packages/COMPASS/versions/1.10.2/topics/pheatmap

# annotaion here
annotation <- norm_data %>% select(Period)
rownames(annotation) <- rownames(norm_data_n)
# Specify colors
ann_colors = list(
  Period = c(P="#999999",Hi="#33A02C",IA="#FB9A99",BA="#663300",LN="#CCCC33",N="#FF7F00",Ho="#3288BD")
)

# plot heatmap
res <- pheatmap(
  color= colorRampPalette(c("navy", "white", "firebrick3"))(10),
  fontsize_number=12,
  fontsize_row=12,
  fontsize_col=6,
  cellwidth=NA,
  cellheight=NA,
  as.matrix(t_norm_data_n),
  #display_numbers=as.matrix(df.mat),
  annotation=annotation,
  annotation_colors=ann_colors, 
  cluster_rows=FALSE, cluster_cols=FALSE
)
