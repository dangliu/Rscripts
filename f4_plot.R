# Script to plot f4 results
# Last updated: 24.Apr.2019

# Libraries
library(tidyverse)

# load data from a f4 output
load("/mnt/scratch/dang/Vietnam/f4/HO.ancient.outgroup.v2.V_V_All_M_f4.allsnp.Rdata")

# combine with pop info
info <- read.table("/mnt/scratch/dang/Vietnam/outgroup/HO.ancient.outgroup.v2.geo.info", header=T)
colnames(info)[3] <- "Y"

data <- f4_res %>% left_join(select(info, Y, Country, Type), by="Y")

# Pop define
Austronesian <- c("Cham","Ede","Giarai")
AustroAsiatic <- c("KhoMu","Kinh","Mang","Muong")
SinoTibetan <- c("Cong","HaNhi","LaHu","LoLo","PhuLa","Sila")
TaiKadai <- c("BoY","CoLao","LaChi","Nung","Tay","Thai")
HmongMien <- c("Dao","Hmong","PaThen")
SChina <- c("Han","Tujia","Miao","Dai","Lahu","She","Naxi","Yi")
NChina <- c("Oroqen","Daur","Mongola","Hezhen","Xibo","Han_NC","Tu")
Taiwan <- c("Atayal","Paiwan","Rukai","Bunun","Tao","Ami","Minnan","Pingpu","Hakka")
ISEA <- c("Borneo","Semende","Mamanwa")
MSEA <- c("Thai_T","Mlabri","Htin_Mal","Cambodian")
India <- c("Onge","Mala","Brahmin_Tiwari","Kharia","Lodhi","Vishwabrahmin","GujaratiD","GujaratiB","GujaratiA","GujaratiC","Jew_Cochin")
Ancient <- c("Hon_Hai_Co_Tien","Kinabatagan","Supu_Hujung","Long_Long_Rak","Vat_Komnou","Nui_Nap","Loyang_Ujung","Mai_Da_Dieu","Nam_Tun","Oakaiel","Tam_Pa_Ping","Tam_Hang","Gua_Cha_N","Man_Bac","Gua_Cha_H","Pha_Faen","Tianyuan")
Vietnam <- c("BoY","CoLao","LaChi","Nung","Tay","Thai","Dao","Hmong","PaThen","Cham","Ede","Giarai","Cong","HaNhi","LaHu","LoLo","PhuLa","Sila","KhoMu","Kinh","Mang","Muong")

# Subset data
#d <- data %>% filter(Type!="ancient")
d <- data


d$X <- factor(d$X, levels=c("BoY","CoLao","LaChi","Nung","Tay","Thai","Dao","Hmong","PaThen","Cham","Ede","Giarai","Cong","HaNhi","LaHu","LoLo","PhuLa","Sila","KhoMu","Kinh","Mang","Muong"))
d$Y <- factor(d$Y, levels=c("Hon_Hai_Co_Tien","Kinabatagan","Supu_Hujung","Long_Long_Rak","Vat_Komnou","Nui_Nap","Loyang_Ujung","Mai_Da_Dieu","Nam_Tun","Oakaiel","Tam_Pa_Ping","Tam_Hang","Gua_Cha_N","Man_Bac","Gua_Cha_H","Pha_Faen","Tianyuan"))

# X tick 90 angle
d %>%
  mutate(Significant=as.factor(abs(Zscore)>3)) %>%
  ggplot(aes(x=Y, y=f4, ymin=f4-3*stderr, ymax=f4+3*stderr, color=Significant)) + 
  geom_point() + 
  geom_errorbar() +
  geom_hline(yintercept=0) +
  facet_wrap(~W*X) +
  scale_color_manual(values = c("#999999", "#CC0033")) +
  theme(axis.line.x = element_line(color="black", size = 0.5, linetype = 1),
        axis.line.y = element_line(color="black", size = 0.5, linetype = 1)) +
  theme(panel.background = element_blank()) +
  labs(x=NULL, color="|Z| > 3") +
  theme(legend.text = element_text(size = 12), legend.title = element_text(size = 12)) +
  theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14)) + 
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12)) +
  theme(strip.text.x = element_text(size = 12), strip.text.y = element_text(size=12)) +
  theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1))

# Change siginificant threshold from 3 to 2, standard error range from 3 times to 2 times
d %>%
  mutate(Significant=as.factor(abs(Zscore)>2)) %>%
  ggplot(aes(x=Y, y=f4, ymin=f4-2*stderr, ymax=f4+2*stderr, color=Significant)) + 
  geom_point() + 
  geom_errorbar() +
  geom_hline(yintercept=0) +
  facet_wrap(~W*X) +
  scale_color_manual(values = c("#999999", "#CC0033")) +
  theme(axis.line.x = element_line(color="black", size = 0.5, linetype = 1),
        axis.line.y = element_line(color="black", size = 0.5, linetype = 1)) +
  theme(panel.background = element_blank()) +
  labs(x=NULL, color="|Z| > 2") +
  theme(legend.text = element_text(size = 12), legend.title = element_text(size = 12)) +
  theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14)) + 
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12)) +
  theme(strip.text.x = element_text(size = 12), strip.text.y = element_text(size=12)) +
  theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1))

# qqplot or dodge points

d$Language <- "NA"
d <- mutate(d,Language = ifelse(X%in%c("BoY","CoLao","LaChi","Nung","Tay","Thai"),"Tai-Kadai",Language))
d <- mutate(d,Language = ifelse(X%in%c("Dao","Hmong","PaThen"),"Hmong-Mien",Language))
d <- mutate(d,Language = ifelse(X%in%c("Cham","Ede","Giarai"),"Austronesian",Language))
d <- mutate(d,Language = ifelse(X%in%c("Cong","HaNhi","LaHu","LoLo","PhuLa","Sila"),"Sino-Tibetan",Language))
d <- mutate(d,Language = ifelse(X%in%c("KhoMu","Kinh","Mang","Muong"),"Austro-Asiatic",Language))


d <- d %>% mutate(Country = ifelse(Country=="India", "India", Country)) %>% 
  mutate(Country = ifelse(Y%in%c(AustroAsiatic,Austronesian,HmongMien,TaiKadai,SinoTibetan),"Vietnam", Country)) %>%
  mutate(Country = ifelse(Y%in%SChina, "SChina", Country)) %>%
  mutate(Country = ifelse(Y%in%NChina, "NChina", Country)) %>%
  mutate(Country = ifelse(Y%in%ISEA, "ISEA", Country)) %>%
  mutate(Country = ifelse(Y%in%MSEA, "MSEA", Country)) %>%
  mutate(Country = ifelse(Y%in%Taiwan, "Taiwan", Country)) %>%
  mutate(Country = ifelse(Y%in%Ancient, "Ancient", Country))

#d <- d %>% mutate(X = ifelse(X%in%AustroAsiatic, "Austro-Asiatic", X)) %>% 
#  mutate(X = ifelse(X%in%Austronesian, "Austronesian", X)) %>%
#  mutate(X = ifelse(X%in%HmongMien, "Hmong-Mien", X)) %>%
#  mutate(X = ifelse(X%in%TaiKadai, "Tai-Kadai", X)) %>%
#  mutate(X = ifelse(X%in%SinoTibetan, "Sino-Tibetan", X))

d <- d %>% mutate(W = ifelse(W%in%AustroAsiatic, "Austro-Asiatic", W)) %>% 
  mutate(W = ifelse(W%in%Austronesian, "Austronesian", W)) %>%
  mutate(W = ifelse(W%in%HmongMien, "Hmong-Mien", W)) %>%
  mutate(W = ifelse(W%in%TaiKadai, "Tai-Kadai", W)) %>%
  mutate(W = ifelse(W%in%SinoTibetan, "Sino-Tibetan", W))
  
# qq

d %>% 
  ggplot(aes(sample=Zscore, color=Language)) +
  stat_qq(alpha=0.7,size=1) +
  stat_qq_line() +
  facet_wrap(~Country*X, ncol=5) +
  scale_color_manual(values=c("#9966CC","#CC6633","#FFCC33","#66CC99","#CC0033")) +
  theme(axis.line.x = element_line(color="black", size = 0.5, linetype = 1),
        axis.line.y = element_line(color="black", size = 0.5, linetype = 1)) +
  theme(panel.background = element_blank()) +
  theme(legend.text = element_text(size = 12), legend.title = element_text(size = 12)) +
  theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14)) + 
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12)) +
  theme(strip.text.x = element_text(size = 12), strip.text.y = element_text(size = 12)) +
  labs(x="Expected Z-score",y="Observed Z-score")

# dodge

d %>% 
  ggplot(aes(x=Country,y=Zscore,color=Language)) +
  geom_hline(yintercept=0) +
  geom_point(position = position_dodge(0.6), alpha=0.8) +
  #geom_jitter(width = 0.2, height = 0.1, alpha=0.5) +
  facet_wrap(~W, ncol=3) +
  scale_color_manual(values=c("#9966CC","#CC6633","#FFCC33","#66CC99","#CC0033")) +
  theme(axis.line.x = element_line(color="black", size = 0.5, linetype = 1),
        axis.line.y = element_line(color="black", size = 0.5, linetype = 1)) +
  theme(panel.background = element_blank()) +
  theme(legend.text = element_text(size = 12), legend.title = element_text(size = 12)) +
  theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14)) + 
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12)) +
  theme(strip.text.x = element_text(size = 12), strip.text.y = element_text(size = 12)) +
  theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1)) +
  labs(x="Area",y="Z-score")


