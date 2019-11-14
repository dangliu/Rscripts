# author: "Dang Liu 08.Apr.2019"

# Last updated: 21.Oct.2019

# Use libraries
library(tidyverse)
library(RColorBrewer)

# Read IBD.NE data
data <- read.table("/mnt/scratch/dang/Vietnam/IBD/IBD_Ne/all.tag.ne", header=T)
head(data)

# Extract within 100 gen
d <- data[data$GEN <= 50,]
#d <- d[d$POP == "BoY",]

# Add language groups for Vietnam only
d$Language <- "NA"
d <- mutate(d,Language = ifelse(POP%in%c("BoY","CoLao","LaChi","Nung","Tay","Thai"),"Tai-Kadai",Language))
d <- mutate(d,Language = ifelse(POP%in%c("Dao","Hmong","PaThen"),"Hmong-Mien",Language))
d <- mutate(d,Language = ifelse(POP%in%c("Cham","Ede","Giarai"),"Austronesian",Language))
d <- mutate(d,Language = ifelse(POP%in%c("Cong","HaNhi","LaHu","LoLo","PhuLa","Sila"),"Sino-Tibetan",Language))
d <- mutate(d,Language = ifelse(POP%in%c("KhoMu","Kinh","Mang","Muong"),"Austro-Asiatic",Language))

# Order by Language groups
d$POP <- factor(d$POP, levels=c("BoY","CoLao","LaChi","Nung","Tay","Thai","Dao","Hmong","PaThen","Cham","Ede","Giarai","Cong","HaNhi","LaHu","LoLo","PhuLa","Sila","KhoMu","Kinh","Mang","Muong"), ordered=T)


# Subset
#d <- d[d$Language == "Austronesian",]
#d <- d[d$Language == "Austro-Asiatic",]
#d <- d[d$Language == "Hmong-Mien",]
#d <- d[d$Language == "Tai-Kadai",]
#d <- d[d$Language == "Sino-Tibetan",]

# Plot 
p <- ggplot(d, aes(x = GEN, y = NE))
#p <- ggplot(d[!d$POP%in%c("Kinh", "Muong"),], aes(x = GEN, y = NE))
p <- p + geom_line()
p <- p + geom_ribbon(aes(ymin=LWR.95.CI,ymax=UPR.95.CI),alpha=0.3)
p <- p + labs(x="Generations before present", y="Effective population size")
p <- p + theme(axis.line.x = element_line(color="black", size = 0.5, linetype = 1),
               axis.line.y = element_line(color="black", size = 0.5, linetype = 1))
#p <- p + facet_wrap(.~POP, nrow=2)
p <- p + facet_wrap(.~POP, nrow=4)
#p <- p + theme(panel.background = element_blank())
p <- p + theme_bw()
p <- p + theme(legend.text = element_text(size = 12), legend.title = element_text(size = 12))
p <- p + theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14))
p <- p + theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))
p <- p + theme(strip.text.x = element_text(size = 12), strip.text.y = element_text(size=12))
#p <- p + scale_x_continuous(breaks = c(0, 20, 40, 60, 80, 100))
p <- p + scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50))
p <- p + scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x), 
                       labels = scales::trans_format("log10", scales::math_format(10^.x))) + coord_cartesian(ylim=c(10, 1e11))
#p <- p + scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x), 
#                       labels = scales::trans_format("log10", scales::math_format(10^.x)))
p
