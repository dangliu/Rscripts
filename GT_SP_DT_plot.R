# Script to visualize GLOBETROTTER results (source proportion and date)
# Last updated: 15.Sep.2020

# Libraries
library(tidyverse)
library(RColorBrewer)

# Read source proportion file and info file
s <- read_csv("/r1/people/dang_liu/Projects/Kula/GT/1-DATE_fit_source_PC1.csv")
info <- read_csv("/r1/people/dang_liu/Projects/Kula/Kula.meta.info.csv") %>% filter(Filter=="PASS")


# Define groups
Collingwood_Bay <- c("Wanigela", "Airara")
W_Mas <- c("Fergusson", "Normanby", "Mainland_Eastern_Tip")
N_Mas <- c("Trobriand", "Gawa", "Woodlark", "Laughlan")
S_Mas <- c("Misima", "Western_Calvados", "Eastern_Calvados", "Sudest", "Rossel")

Massim <- c(Collingwood_Bay, W_Mas, N_Mas, S_Mas)
Outgroup <- c("Africa", "WestEurasia")
Near_Out <- c("EastAsia", "EastAsia_AN", "Australia")
PNG_H <- c("S_Papuan", "Southern_Highlands", "Enga", "Western_Highlands", "Chimbu", "Eastern_Highlands", "Gulf", "Madang_Highland")
PNG_L <- c("Northern", "Madang_Lowland", "Morobe", "East_Sepik", "Western", "Central")
Bismark <- c("Manus_New_Ireland", "West_New_Britain", "East_New_Britain")
Solomon <- c("Bougainville", "Vella_Lavella", "Malaita", "Santa_Cruz", "Bellona_Rennell", "Tikopia")

# For color by Area
info[info$Label=="Africa",]$Area <- "Africa"
info[info$Label=="WestEurasia",]$Area <- "Europe"
info[info$Label=="EastAsia",]$Area <- "EastAsia"
info[info$Label=="EastAsia_AN",]$Area <- "EastAsia_AN"



# Get Label group info for Target and Source
#Target_Label <- info %>% select(Pop, Label) %>% rename(Target="Pop") %>% distinct(.keep_all = T)
#Source_Label <- info %>% select(Pop, Label) %>% rename(Source="Pop", Label2="Label") %>% distinct(.keep_all = T)

# For color by Area
Target_Label <- info %>% select(Pop, Label) %>% rename(Target="Pop") %>% distinct(.keep_all = T)
Source_Label <- info %>% select(Pop, Area) %>% rename(Source="Pop", Label2="Area") %>% distinct(.keep_all = T)


s_Label <- s %>% left_join(Target_Label) %>% left_join(Source_Label)

s_Label <- s_Label %>% mutate(Proportion3=Proportion*Proportion2)

s_m_M <- s_Label %>% select(Label,Label2,Proportion,Proportion2,Proportion3,Type) %>% rename(Target="Label", Source="Label2") %>% 
  group_by(Target,Source,Type) %>%
  mutate(Proportion4=sum(Proportion3)) %>%
  select(-Proportion2,-Proportion3) %>%
  distinct(.keep_all = T)

s_m_M[s_m_M$Type=="m",]$Source <- paste0(s_m_M[s_m_M$Type=="m",]$Source, "_m")

#s_m_M$Source <- factor(s_m_M$Source, levels=c("Africa_m","WestEurasia_m","EastAsia_m","EastAsia_AN_m",
#                                              "Southern_Highlands_m","Enga_m","Western_Highlands_m","Eastern_Highlands_m","Gulf_m",
#                                              "Northern_m","Madang_Lowland_m","Morobe_m","East_Sepik_m","Central_m",
#                                              "West_New_Britain_m","East_New_Britain_m",
#                                              "Malaita_m","Vella_Lavella_m","Santa_Cruz_m","Bellona_Rennell_m","Tikopia_m",
#                                              "Africa","WestEurasia","EastAsia","EastAsia_AN",
#                                              "Southern_Highlands","Enga","Western_Highlands","Chimbu","Eastern_Highlands","Gulf",
#                                              "Northern","Madang_Lowland","Morobe","East_Sepik","Western","Central",
#                                              "West_New_Britain","East_New_Britain",
#                                              "Vella_Lavella","Malaita","Santa_Cruz","Bellona_Rennell","Tikopia"), ordered=T)

# For color by Area
s_m_M$Source <- factor(s_m_M$Source, levels=c("PNG_Highland_m", "PNG_Lowland_m", "Bismark_Arch_m", "Massim_m", "Solomon_Islands_m", 
                                              "Australia_m", "EastAsia_AN_m", "EastAsia_m", "Europe_m", "Africa_m",
                                              "PNG_Highland", "PNG_Lowland", "Bismark_Arch", "Massim", "Solomon_Islands", 
                                              "Australia", "EastAsia_AN", "EastAsia", "Europe", "Africa"), ordered=T)



s_m_M$Target <- factor(s_m_M$Target, levels=Massim, ordered=T)

#colourCount = length(unique(s_m_M$Source))
#getPalette = colorRampPalette(brewer.pal(12, "Paired"))

# For color by group
#Group_color = c("Africa_m"="#A6CEE3","WestEurasia_m"="#62A3CB","EastAsia_m"="#1F78B4","EastAsia_AN_m"="#68AB9F",
#                "Southern_Highlands_m"="#B2DF8A","Enga_m"="#72BF5A","Western_Highlands_m"="#33A02C","Eastern_Highlands_m"="#969D62","Gulf_m"="#FB9A99",
#                "Northern_m"="#EF595A","Madang_Lowland_m"="#E31A1C","Morobe_m"="#F06C45","East_Sepik_m"="#FDBF6F","Central_m"="#FE9E37",
#                "West_New_Britain_m"="#FF7F00","East_New_Britain_m"="#E4986B",
#                "Malaita_m"="#CAB2D6","Vella_Lavella_m"="#9A77B8","Santa_Cruz_m"="#6A3D9A","Bellona_Rennell_m"="#B49D99","Tikopia_m"="#FFFF99",
#                "Africa"="#A6CEE3","WestEurasia"="#62A3CB","EastAsia"="#1F78B4","EastAsia_AN"="#68AB9F",
#                "Southern_Highlands"="#B2DF8A","Enga"="#72BF5A","Western_Highlands"="#33A02C","Chimbu"="#D8AC60","Eastern_Highlands"="#969D62","Gulf"="#FB9A99",
#                "Northern"="#EF595A","Madang_Lowland"="#E31A1C","Morobe"="#F06C45","East_Sepik"="#FDBF6F","Western"="#B15928","Central"="#FE9E37",
#                "West_New_Britain"="#FF7F00","East_New_Britain"="#E4986B",
#                "Malaita"="#CAB2D6","Vella_Lavella"="#9A77B8","Santa_Cruz"="#6A3D9A","Bellona_Rennell"="#B49D99","Tikopia"="#FFFF99")

# For color by Area
Area_color = c("PNG_Highland_m"="#A6CEE3","PNG_Lowland_m"="#1F78B4","Bismark_Arch_m"="#B2DF8A", 
               "Massim_m"="#33A02C","Solomon_Islands_m"="#FB9A99","Australia_m"="#E31A1C",
               "EastAsia_AN_m"="#FDBF6F","EastAsia_m"="#FF7F00","Europe_m"="#CAB2D6","Africa_m"="#6A3D9A",
               "PNG_Highland"="#A6CEE3","PNG_Lowland"="#1F78B4","Bismark_Arch"="#B2DF8A", 
               "Massim"="#33A02C","Solomon_Islands"="#FB9A99","Australia"="#E31A1C",
               "EastAsia_AN"="#FDBF6F","EastAsia"="#FF7F00","Europe"="#CAB2D6","Africa"="#6A3D9A")

s_m_M %>% ggplot() +
  geom_bar(aes(x=Target, y=Proportion4, fill=Source), stat="identity") +
  geom_errorbar(data=s_m_M[s_m_M$Type=="M",],aes(x=Target, ymax=Proportion, ymin=Proportion), size=1, linetype="solid") + 
  #scale_fill_manual(values = Group_color) + # For color by Group
  scale_fill_manual(values = Area_color) + # For color by Area
  theme(axis.line.x = element_line(color="black", size = 0.5, linetype = 1),
        axis.line.y = element_line(color="black", size = 0.5, linetype = 1)) +
  theme(panel.background = element_blank()) +
  labs(x="Target", y="Proportion", fill="Source") +
  guides(fill = guide_legend(ncol=2)) +
  theme(legend.text = element_text(size = 12), legend.title = element_text(size = 12)) +
  theme(axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12)) + 
  theme(axis.text.x = element_text(size = 12, angle=90, vjust=0.4, hjust=0.95), axis.text.y = element_text(size = 12)) +
  theme(strip.text.x = element_text(size = 12), strip.text.y = element_text(size = 12))



##################################################
info[info$Label%in%Collingwood_Bay,]$Region <- "Collingwood_Bay"
info[info$Label%in%W_Mas,]$Region <- "Western_Massim"
info[info$Label%in%N_Mas,]$Region <- "Northern_Massim"
info[info$Label%in%S_Mas,]$Region <- "Southern_Massim"

Target_Label <- info %>% select(Pop, Label, Region) %>% rename(Target="Pop") %>% distinct(.keep_all = T)

# Read the bootstrapped dates
d <- read_delim("/r1/people/dang_liu/Projects/Kula/GT/DATES_boot.txt",delim='\t',col_names=T)
d <- d %>% left_join(Target_Label) %>% select(-Target) %>% rename(Target="Label")

Region_col = c("Collingwood_Bay"="#8DD3C7","Western_Massim"="#BEBADA","Northern_Massim"="#FB8072","Southern_Massim"="#80B1D3")


d$Target <- factor(d$Target, levels=Massim, ordered=T)
# Density
d %>% ggplot() +
  geom_density(aes(Date*30, fill=Region), alpha = 0.8) +
  facet_wrap(.~Target, nrow=5) +
  scale_x_continuous(breaks=c(0, 500, 1000, 1500, 2000, 2500, 3000, 3500), limits=c(0,3500)) +
  scale_fill_manual(values=Region_col) +
  theme(axis.line.x = element_line(color="black", size = 0.5, linetype = 1),
        axis.line.y = element_line(color="black", size = 0.5, linetype = 1)) +
  theme(panel.background = element_blank()) +
  labs(x="Date (years ago)", y="Density") +
  theme(legend.text = element_text(size = 12), legend.title = element_text(size = 12)) +
  theme(axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12)) + 
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12)) +
  theme(strip.text.x = element_text(size = 12), strip.text.y = element_text(size = 12))


d$Target <- factor(d$Target, levels=rev(Massim), ordered=T)
# Boxplot  
d %>% ggplot() +
  geom_boxplot(aes(Target,Date*30, fill=Region), width=0.2) +
  coord_flip() +
  scale_y_continuous(breaks=c(0, 500, 1000, 1500, 2000, 2500, 3000, 3500), limits=c(0,3500)) +
  scale_fill_manual(values=Region_col) +
  theme(axis.line.x = element_line(color="black", size = 0.5, linetype = 1),
        axis.line.y = element_line(color="black", size = 0.5, linetype = 1)) +
  theme(panel.background = element_blank()) +
  labs(y="Date (years ago)", x=NULL) +
  theme(legend.text = element_text(size = 12), legend.title = element_text(size = 12)) +
  theme(axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12)) + 
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12)) +
  theme(strip.text.x = element_text(size = 12), strip.text.y = element_text(size = 12))



