# author: "Dang Liu 19.Mar.2019"

# Last updated: 19.Mar.2019

library(tidyverse)

# Get chromosomes in the right order
data = read.table('/mnt/scratch/dang/Vietnam/IBD/Dist/Merged.BoY.SNP.dist', header = T)

# Order data
data$type <- factor(data$type, levels=c("Beagle","Beagle_ref","Shapeit","ROH", "SNP"), ordered=T)

# Assign positions
data$positions <- 1
data <- mutate(data,positions = ifelse(type%in%c("Beagle"),5,positions))
data <- mutate(data,positions = ifelse(type%in%c("Beagle_ref"),4,positions))
data <- mutate(data,positions = ifelse(type%in%c("Shapeit"),3,positions))
data <- mutate(data,positions = ifelse(type%in%c("ROH"),2,positions))


data$positions <- as.numeric(data$positions)

# hg19
chrom_sizes <- data.frame(chrom = c(1:22,'X'), size = c(249250621, 243199373, 198022430, 191154276, 180915260, 171115067, 159138663, 146364022, 141213431, 135534747, 135006516, 133851895, 115169878, 107349540, 102531392, 90354753, 81195210, 78077248, 59128983, 63025520, 48129895, 51304566, 155270560))

# Only autosomes
chrom_sizes <- chrom_sizes[chrom_sizes$chrom!='X',]

# Only SNPs
snp <- data[data$type=="SNP",]

data %>% 
  ggplot() + 
  
  # add chromosomes
  geom_rect(data = chrom_sizes, aes(ymin = 5.1, 
                                    ymax = -0.1, 
                                    xmax = size/1000000, xmin = 0), 
            colour="black", fill = "white") +
  
  
  # Add segments
  geom_rect(aes(xmin = start/1000000, xmax = end/1000000, ymin = positions -0.95, ymax = positions-0.05, fill=type)) +
  
  # Add SNPs
  geom_point(data = snp, aes(x = start/1000000, y = 0.5), size=0.25, alpha=0.25) +
  
  
  theme_bw(base_size = 30) +
  facet_grid(chrom~., switch = 'y') +
  theme(strip.background = element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        strip.text.y = element_text(angle = 180),
        panel.spacing.y=unit(0, "lines")) +
  scale_fill_manual(values = c("#0066CC", "#009900", "#FF9966", "#FF0033", '#000000')) +
  labs(x="Position (Mb)", y=NULL, fill=NULL)
  