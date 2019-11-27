## Network visualization of IBD sharing between populations with their geo-coordinates on a map
# (inspired by) Ref: https://chrischizinski.github.io/rstats/igraph-ggplotll/

# author: "Dang Liu 25.Nov.2019"

# Last updated: 26.Nov.2019

# Use libraries
library(tidyverse)
library(RColorBrewer)
library(gtools)
library(ggmap)
library(maps)
library(ggrepel)

# Read the IBD sharing input for network visualization (Edge file format for Cytoscpae)
IBD <- read_delim("/home/dang_liu/Projects/Thailand/IBD/all.tag.edge.txt",delim="\t",col_names=F)
colnames(IBD) <- c("Pop1", "Pop2", "Avg.sum.IBD.L", "Range")

# Read info file
info <- read_delim("/home/dang_liu/Projects/Thailand/ThaiCompare.info2.txt",delim="\t")
#info <- read_csv("/r1/people/dang_liu/Projects/Vietnam/Vietnam.metadata.csv")
head(info)

# Get the median of sample geo-coordinates for each pop, and make a new info table at population-level
#info2 <- info %>% filter(Pop%in%c(IBD$Pop1,IBD$Pop2)) %>% group_by(Pop) %>% 
#  summarise_at(vars(Latitude,Longitude), funs(median(.))) %>% 
#  left_join(select(info,-(FID:IID),-(Latitude:Longitude))) %>%
#  distinct(Pop, .keep_all = TRUE)

# Filter with lat and long
info2 <- info %>% filter(Pop%in%c(IBD$Pop1,IBD$Pop2) & Latitude<=21 & Latitude>=5 & Longitude <=108 | Country=="Thailand") %>% group_by(Pop) %>% 
  summarise_at(vars(Latitude,Longitude), funs(median(.))) %>% 
  left_join(select(info,-(FID:IID),-(Latitude:Longitude))) %>%
  distinct(Pop, .keep_all = TRUE)


# Combine info and IBD, and make a new data frame with geo-coordinate links
IBD$from.long <- info2$Longitude[match(IBD$Pop1, info2$Pop)]
IBD$from.lat <- info2$Latitude[match(IBD$Pop1, info2$Pop)]
IBD$to.long <- info2$Longitude[match(IBD$Pop2, info2$Pop)]
IBD$to.lat <- info2$Latitude[match(IBD$Pop2, info2$Pop)]


# Remove NA segments
IBD <- IBD %>% drop_na()


# Colored by Language familiy
Language = c("Austronesian"="#CC6633","Austroasiatic"="#9966CC","Hmong-Mien"="#FFCC33","Sino-Tibetan"="#66CC99","Tai-Kadai"="#CC0033",
             "Indo-European"="#FFCCFF","Dravidian"="#CC0099","Andamanese"="#660033","Mongolic"="#0000FF","Tungusic"="#66CCFF","Ancient"="#000000")

# Get hte map
map.world <- map_data(map="world")

# Set seed for jitter
set.seed(10)

# Plot!

# The map
p <- ggplot()
p <- p + theme()
p <- p + geom_map(data=map.world, map=map.world, aes(map_id=region, x=long, y=lat), fill="white", colour="grey", size=0.15)
p <- p + coord_quickmap(ylim=c(min(info2$Latitude),max(info2$Latitude)), xlim=c(min(info2$Longitude),max(info2$Longitude)))
jitter <- position_jitter(width = 3, height = 3)

# The egdes
p <- p + geom_segment(data=IBD, aes(x=from.long, xend=to.long, y=from.lat, yend=to.lat, color=Avg.sum.IBD.L), alpha=0.5, size=1)
#p <- p + scale_colour_gradientn(colors=c("#4575B4", "#66CCFF", "#FFEDA0", "#D73027"), breaks=c(0,40,80,120))
p <- p + scale_colour_gradientn(colors=c("#4575B4", "#66CCFF", "#D73027"), breaks=c(0,20,40,60,80))

# New facet label names for range
range.labs <- c("1 to 5 cM (90 generations ago)", "5 to 10 cM (23 generations ago)", "Over 10 cM (7.5 generations ago)")
names(range.labs) <- c("1to5cM", "5to10cM", "over10cM")
p <- p + facet_wrap(.~Range, ncol=3, labeller = labeller(Range=range.labs))

# The pops
p <- p + geom_point(data=info2, aes(x=Longitude, y=Latitude, fill=Language), pch=21, size=6, alpha=0.75) 
p <- p + scale_fill_manual(values=Language)
#p <- p + geom_text(data=info2, aes(label=Pop, x=Longitude, y=Latitude))
p <- p + geom_text_repel(
  data=info2, 
  aes(x=Longitude, y=Latitude,label=Pop),
  size=4,
  point.padding=0.25,
  segment.alpha=0.5)

# The styles
p <- p + theme(legend.text = element_text(size = 12), legend.title = element_text(size = 14))
p <- p + theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14))
p <- p + theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10))
p <- p + theme(strip.text.x = element_text(size = 12), strip.text.y = element_text(size=12))
p <- p + labs(x="Longitude", y="Latitude", color="Mean of summed IBD length (cM)", fill="Language family")
p


