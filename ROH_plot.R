# author: "Dang Liu 19.Mar.2019"

# Last updated: 19.Mar.2019

# Use libraries
library(tidyverse)

# Read between data
data <- read.table("/mnt/scratch/dang/Vietnam/ROH/default.50snp.1.5Mb.hom.indiv", header=T)
head(data)

# Pop info
info <- read.table("/mnt/genotyping/SNPChipData/Vietnam/dataset/HO.ancient.info", header=T)
head(info)

# Combine
d <- data %>% left_join(info)

# Add language groups for Vietnam only
d$Language <- "NA"
d <- mutate(d,Language = ifelse(Pop%in%c("BoY","CoLao","LaChi","Nung","Tay","Thai"),"Tai-Kadai",Language))
d <- mutate(d,Language = ifelse(Pop%in%c("Dao","Hmong","PaThen"),"Hmong-Mien",Language))
d <- mutate(d,Language = ifelse(Pop%in%c("Cham","Ede","Giarai"),"Austronesian",Language))
d <- mutate(d,Language = ifelse(Pop%in%c("Cong","HaNhi","LaHu","LoLo","PhuLa","Sila"),"Sino-Tibetan",Language))
d <- mutate(d,Language = ifelse(Pop%in%c("KhoMu","Kinh","Mang","Muong"),"Austro-Asiatic",Language))

# Subset
d <- d[d$Language!="NA",]

# Average NROH by Pop
average <- d %>% group_by(Pop) %>% 
  summarise_at(vars(NSEG, KB), funs(mean(.)))
colnames(average) <- c("Pop", "NROH", "SROH")
average$NROH <- round(average$NROH)

# Add language groups for Vietnam only
average$Language <- "NA"
average <- mutate(average,Language = ifelse(Pop%in%c("BoY","CoLao","LaChi","Nung","Tay","Thai"),"Tai-Kadai",Language))
average <- mutate(average,Language = ifelse(Pop%in%c("Dao","Hmong","PaThen"),"Hmong-Mien",Language))
average <- mutate(average,Language = ifelse(Pop%in%c("Cham","Ede","Giarai"),"Austronesian",Language))
average <- mutate(average,Language = ifelse(Pop%in%c("Cong","HaNhi","LaHu","LoLo","PhuLa","Sila"),"Sino-Tibetan",Language))
average <- mutate(average,Language = ifelse(Pop%in%c("KhoMu","Kinh","Mang","Muong"),"Austro-Asiatic",Language))

# Plot within pop number vs. length to infer demography
p <- ggplot(average, aes(x = SROH/1000, y = NROH, color=Language))
p <- p + geom_text(aes(label=Pop), vjust = 1.5, nudge_y = 0.0025, size=5)
p <- p + geom_point(size=4,alpha=0.8)
p <- p + scale_color_manual(values=c("#9966CC","#CC6633","#FFCC33","#66CC99","#CC0033"))
p <- p + labs(x="Mean SROH (Mb)", y="Mean NROH (n)", color="Language group")
p <- p + theme(axis.line.x = element_line(color="black", size = 0.5, linetype = 1),
               axis.line.y = element_line(color="black", size = 0.5, linetype = 1))
p <- p + theme(panel.background = element_blank())
p <- p + theme(legend.text = element_text(size = 12), legend.title = element_text(size = 12))
p <- p + theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14))
p <- p + theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))
p