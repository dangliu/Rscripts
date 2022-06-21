# title: "ADMIXTURE on TW data"
# author: "Dang Liu 20.May.2021"

# Last updated: 20.May.2021

# Use libraries
library(tidyverse)
library(reshape2)

# Determine the best K based on cross-validation error rate (the K with the lowest CV) 

# read CV.log
cv <- read.table("/mnt/scratch/dang/Taiwan/admixture/CV_2-15.log")
colnames(cv) <- c("K", "error")
head(cv)
cv$K <- as.character(cv$K)
cv$K <- factor(cv$K, levels=c("2","3","4","5","6","7","8","9","10","11","12","13","14","15"))
# plot 
p0 <- ggplot(cv, aes(K, error)) + geom_boxplot()
p0 <- p0 + geom_hline(yintercept=min(cv$error), linetype="dashed", col="grey")
#p0 <- p0 + scale_y_continuous(breaks = c(min(cv$error),median(cv$error),max(cv$error)))
p0 <- p0 + theme(axis.line.x = element_line(color="black", size = 0.5, linetype = 1),
                 axis.line.y = element_line(color="black", size = 0.5, linetype = 1))
p0 <- p0 + theme(panel.background = element_blank())
p0 <- p0 + labs(x="K", y="Cross-validation error")
p0 <- p0 + theme(legend.text = element_text(size = 12), legend.title = element_text(size = 14))
p0 <- p0 + theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16))
p0 <- p0 + theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14))
p0
