# title: "ADMIXTURE on GW data"
# author: "Dang Liu 19.Dec.2018"

# Last updated: 25.Feb.2019

# Use libraries
library(tidyverse)
library(reshape2)

# Determine the best K based on cross-validation error rate (the K with the lowest CV) 

# read CV.log
cv <- read.table("/mnt/scratch/dang/Vietnam/admixture/nodrift.SEA.SC.TW.merged3.am.pruned.CV_1-10.log")
colnames(cv) <- c("K", "error")
head(cv)
cv$K <- as.character(cv$K)
cv$K <- factor(cv$K, levels=c("1","2","3","4","5","6","7","8","9","10"))
# plot 
p0 <- ggplot(cv, aes(K, error)) + geom_boxplot()
p0 <- p0 + geom_hline(yintercept=min(cv$error), linetype="dashed", col="grey")
#p0 <- p0 + scale_y_continuous(breaks = c(min(cv$error),median(cv$error),max(cv$error)))
p0 <- p0 + theme(axis.line.x = element_line(color="black", size = 0.5, linetype = 1),
               axis.line.y = element_line(color="black", size = 0.5, linetype = 1))
p0 <- p0 + theme(panel.background = element_blank())
p0 <- p0 + labs(x="K", y="Cross-validation error")
p0 <- p0 + theme(legend.text = element_text(size = 12), legend.title = element_text(size = 14))
p0 <- p0 + theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14))
p0 <- p0 + theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10))
p0

min(cv$error)

# Visualization of ADMIXTURE

# Read the Q file from ADMIXTURE output
# Here K=4 is the best K estimation
#tbl <- read.table("/mnt/scratch/dang/Vietnam/admixture/modern.SST.L2.L.M.filtered.v2.pruned.5.Q")
#data <- read.table("/mnt/scratch/dang/Vietnam/admixture/fastNGSadmix/Merged.nodrift.3.txt",header=T,stringsAsFactors=F)
data <- read.table("/mnt/scratch/dang/Vietnam/admixture/fastNGSadmix/Merged.modern.5.txt",header=T,stringsAsFactors=F)
#colnames(tbl) <- c("A1","A2","A3","A4","A5")
#colnames(tbl) <- c("A1","A2","A3")
#barplot(t(as.matrix(tbl)), col=rainbow(4), xlab="Individual #", ylab="Ancestry", border=NA)

# Link the value to their individual ID as they are in the same order with PLINK.fam file
#fm <- read.table("/mnt/scratch/dang/Vietnam/admixture/modern.SST.L2.L.M.filtered.v2.pruned.fam")
#fm <- read.table("/mnt/scratch/dang/Vietnam/admixture/nodrift.SST.L2.L.M.filtered.v2.pruned.fam")
#colnames(fm) <- c("FID","IID","V1","V2","V3","V4")
#data <- cbind(fm[,1:2],tbl)

# Read file with population information
#info <- read.table("/mnt/scratch/dang/Vietnam/admixture/modern.SEA.SC.TW.am.filtered.v2.info",header=T)
#info <- read.table("/mnt/scratch/dang/Vietnam/admixture/nodrift.SEA.SC.TW.am.filtered.v2.info",header=T)
info <- read.table("/mnt/scratch/dang/Vietnam/admixture/fastNGSadmix/SEA.SC.TW.merged3.am.filtered.info",header=T,stringsAsFactors=F)
# Join the data
data <- info %>% left_join(data)
data[data$Type=="ancient",]$Country <- paste("a_", data[data$Type=="ancient",]$Country, sep="")
data <- data[data$IID!="I2497",]
# Order it by country
#data$Country <- factor(data$Country, levels=c("Taiwan","China","Vietnam","Cambodia","Thailand","Malaysia","Indonesia","Philippines"))
data$Country <- factor(data$Country, levels=c("Taiwan","China","Vietnam","a_Vietnam","Cambodia","a_Cambodia","a_Laos","Thailand","a_Thailand","a_Myanmar","Malaysia","a_Malaysia","Indonesia","a_Indonesia","Philippines"))
# Gather the Q value into one column called P and their original column names become to another column called ANC  
g_data <- data %>% gather(ANC,P,A1:A5)
#g_data <- data %>% gather(ANC,P,A1:A3)

# plot
# facet by country
p <- ggplot(g_data, aes(x=IID, y=P, fill=ANC))
p <- p + geom_bar(stat='identity') 
#p <- p + scale_fill_brewer(palette="Paired")
p <- p + scale_fill_manual(values=c("#33A02C","#1F78B4","#B2DF8A","#A6CEE3","#FB9A99"))
p <- p + facet_grid(~Country, scales="free", space="free_x")
p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8))
p <- p + scale_x_discrete(breaks=g_data$IID,labels=g_data$Pop)
p
