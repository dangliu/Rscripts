# Script to visualize GLOBETROTTER results (source proportion and date)
# Last updated: 13.Mar.2020

# Libraries
library(tidyverse)

# Read source proportion file
s <- read_csv("/r1/people/dang_liu/Projects/Thailand/GT/1-DATE_fit_sources_PC1.csv")
# Calculate and weight the major and minor proportions
s_m_M <- s %>% mutate_at(vars(Mamanwa_m:Gujarati_m), funs(.*Minor)) %>% mutate_at(vars(Atayal:Cambodian), funs(.*Major)) %>% select(-Minor,-Major) 
# A table for just major and minor proportions in total
m_M <- s %>% select(Target,Minor,Major)
# Data transpose for bar plot
t_s_m_M <- s_m_M %>% gather(Source, Proportion, -Target)
t_s_m_M$Source <- factor(t_s_m_M$Source, levels=colnames(s_m_M)[-1], ordered=T)
t_s_m_M$Target <- factor(t_s_m_M$Target, levels=c("Mon","CentralThai","SouthernThai_Buddhisht","SouthernThai_Muslim","Moken"), ordered=T)
t_s_m_M %>% ggplot() +
  geom_bar(aes(x=Target, y=Proportion, fill=Source), stat="identity") +
  geom_errorbar(data=m_M, aes(x=Target, ymax=Major, ymin=Major), size=1, linetype="solid") + 
  theme(axis.line.x = element_line(color="black", size = 0.5, linetype = 1),
      axis.line.y = element_line(color="black", size = 0.5, linetype = 1)) +
  theme(panel.background = element_blank()) +
  labs(x="Target", y="Proportion", fill="Source") +
  theme(legend.text = element_text(size = 12), legend.title = element_text(size = 12)) +
  theme(axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12)) + 
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12)) +
  theme(strip.text.x = element_text(size = 12), strip.text.y = element_text(size = 12))

#################################################

# Read the bootstrapped dates
d <- read_delim("/r1/people/dang_liu/Projects/Thailand/GT/DATES_boot.txt",delim='\t',col_names=F)
colnames(d) <- c("Date", "Target")

d$Target <- factor(d$Target, levels=c("Moken","SouthernThai_Muslim","SouthernThai_Buddhisht","CentralThai","Mon"), ordered=T)
# Density
d %>% ggplot() +
  geom_density(aes(Date*30), fill="grey", alpha = 0.8) +
  facet_wrap(.~Target, nrow=5) +
  scale_x_continuous(breaks=c(0, 250, 500, 750, 1000, 1250, 1500), limits=c(0,1500)) +
  theme(axis.line.x = element_line(color="black", size = 0.5, linetype = 1),
        axis.line.y = element_line(color="black", size = 0.5, linetype = 1)) +
  theme(panel.background = element_blank()) +
  labs(x="Date (years ago)", y="Density") +
  theme(legend.text = element_text(size = 12), legend.title = element_text(size = 12)) +
  theme(axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12)) + 
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12)) +
  theme(strip.text.x = element_text(size = 12), strip.text.y = element_text(size = 12))


d$Target <- factor(d$Target, levels=c("Mon","CentralThai","SouthernThai_Buddhisht","SouthernThai_Muslim","Moken"), ordered=T)
# Boxplot  
d %>% ggplot() +
  geom_boxplot(aes(Target,Date*30), width=0.2, fill="grey") +
  coord_flip() +
  scale_y_continuous(breaks=c(0, 250, 500, 750, 1000, 1250, 1500), limits=c(0,1500)) +
  theme(axis.line.x = element_line(color="black", size = 0.5, linetype = 1),
        axis.line.y = element_line(color="black", size = 0.5, linetype = 1)) +
  theme(panel.background = element_blank()) +
  labs(y="Date (years ago)", x=NULL) +
  theme(legend.text = element_text(size = 12), legend.title = element_text(size = 12)) +
  theme(axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12)) + 
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12)) +
  theme(strip.text.x = element_text(size = 12), strip.text.y = element_text(size = 12))
