# Script to visualize alder dates
# Last updated: 11.Mar.2020

# Libraries
library(tidyverse)

# Input
d <- read_csv("/r1/people/dang_liu/Projects/Thailand/alder/alder.dates.csv") 
d$Target <- factor(d$Target, levels=c("Mon","CentralThai","SouthernThai_Buddhisht","SouthernThai_Muslim","Moken"), ordered=T)

d %>%
  ggplot(aes(x=Target, y=Date*30, ymin=(Date-SE)*30, ymax=(Date+SE)*30)) + 
  geom_point(aes(pch=Test), size=3) + 
  geom_errorbar(width=0.4) +
  facet_wrap(~Source1*Source2, ncol=6) +
  coord_flip() +
  scale_y_continuous(breaks=c(0, 250, 500, 750, 1000, 1250), limits=c(0,1250)) +
  scale_shape_manual(values = c("S"=19, "F"=1)) +
  theme(axis.line.x = element_line(color="black", size = 0.5, linetype = 1),
        axis.line.y = element_line(color="black", size = 0.5, linetype = 1)) +
  theme(panel.background = element_blank()) +
  labs(x=NULL, y="Date (years ago)", pch="Test") +
  theme(legend.text = element_text(size = 12), legend.title = element_text(size = 12)) +
  theme(axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12)) + 
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12)) +
  theme(strip.text.x = element_text(size = 12), strip.text.y = element_text(size = 12))
