# Script to visualize malder and GT dates
# Last updated: 23.May.2022

# Libraries
library(tidyverse)

# Groups
Collingwood_Bay <- c("Northern", "Wanigela", "Airara")
W_Mas <- c("Fergusson", "Normanby", "Mainland_Eastern_Tip")
N_Mas <- c("Trobriand", "Gawa", "Woodlark", "Laughlan")
S_Mas <- c("Misima", "Western_Calvados", "Eastern_Calvados", "Sudest", "Rossel")

Massim <- c(Collingwood_Bay, W_Mas, N_Mas, S_Mas)

# MALDER input
d_a <- read_csv("/r1/people/dang_liu/Projects/Kula/ALDER/malder.dates.csv") %>% filter(SourceB!="Rossel") %>% filter(SourceA!="Bellona_Rennell")
#d$Target <- factor(d$Target, levels=rev(Massim), ordered=T)

# GLOBETROTTER input
info <- read_csv("/r1/people/dang_liu/Projects/Kula/Kula.meta.info.csv") %>% filter(Filter=="PASS")
#Target_Label <- info %>% select(Pop, Label) %>% rename(Target="Pop") %>% distinct(.keep_all = T)
d <- read_delim("/r1/people/dang_liu/Projects/Kula/GT/DATES_boot.listV2.txt",delim='\t',col_names=T)
#d <- d %>% left_join(Target_Label) %>% select(-Target) %>% rename(Target="Label")
d_m_se <- d %>% group_by(Target) %>%  
  summarise(T = mean(Date, na.rm=T), sd = sd(Date, na.rm=T), n = n()) %>%
  mutate(se = sd / sqrt(n), T_CI_low = T - qt(1 - (0.05 / 2), n - 1) * se, T_CI_up = T + qt(1 - (0.05 / 2), n - 1) * se) %>% 
  select(Target, T, sd)

#Combine
d_A <- d_a %>% select(Target, Test, Date_2ref, SE_2ref) %>% 
  rename("Date"=Date_2ref, "SE"=SE_2ref) %>% 
  mutate(Method="MALDER")
d_G <- d_m_se %>% rename("Date"=T, "SE"=sd) %>% 
  mutate(Test="S", Method="GLOBETROTTER")

d_AG <- d_A %>% bind_rows(d_G) %>%
  mutate(Label_col=case_when(
    Target %in% Collingwood_Bay ~ "#8DD3C7",
    Target %in% W_Mas ~ "#BEBADA",
    Target %in% N_Mas ~ "#2e2967",
    Target %in% S_Mas ~ "#b87a41"
  ))

d_AG$Target <- factor(d_AG$Target, levels=rev(Massim), ordered=T)

#Plot it!
d_AG %>%
  ggplot(aes(x=Target, y=Date*30, ymin=(Date-SE)*30, ymax=(Date+SE)*30)) + 
  #geom_point(aes(pch=Test, color=Method), size=3) +
  geom_point(aes(color=Method), size=3, pch=19) + 
  geom_errorbar(aes(color=Method), width=0.4) +
  coord_flip() +
  scale_y_continuous(breaks=c(0, 500, 1000, 1500, 2000, 2500, 3000, 3500, 4000), limits=c(0,4000)) +
  #scale_shape_manual(values = c("S"=19, "F"=1)) +
  scale_color_manual(values = c("MALDER"="black", "GLOBETROTTER"="red")) +
  theme(axis.line.x = element_line(color="black", size = 0.5, linetype = 1),
        axis.line.y = element_line(color="black", size = 0.5, linetype = 1)) +
  #theme(panel.background = element_blank()) +
  theme_bw() +
  labs(x=NULL, y="Date (years ago)", pch="Test") +
  theme(legend.text = element_text(size = 14), legend.title = element_text(size = 16)) +
  theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16)) + 
  theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14, color = rev(d_AG[d_AG$Method=="MALDER",]$Label_col)))


