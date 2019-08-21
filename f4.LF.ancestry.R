# Script to run f3 and f4 on Vietnam data to test Language ancestry
# Last updated: 27.Jun.2019

# Libraries
library(tidyverse)
library(admixr)

# read data
data <- eigenstrat('/mnt/scratch/dang/Vietnam/f4/HO.ancient.outgroup.v2')
info <- read_csv('~/Projects/Vietnam/Vietnam.metadata.csv')

# pop
ind <- read_ind(data)
table(ind$label)
pops <- unique(ind$label)
pops

# Test pops
Vietnam <- pops[pops%in% c("Cham","Ede","Giarai","KhoMu","Kinh","Mang","Muong","BoY","CoLao","LaChi","Nung","Tay","Thai","Dao","Hmong","PaThen","Cong","HaNhi","LaHu","LoLo","PhuLa","Sila")]
Ancient <- pops[pops%in% c("P-Tianyuan","Ho-Pha_Faen","Ho-Gua_Cha","N-Gua_Cha","N-Man_Bac","N-Nam_Tun","N-Mai_Da_Dieu","N-Hon_Hai_Co_Tien","N-Tam_Pa_Ling","N-Tam_Hang","N-Oakaie","N-Loyang_Ujung","BA-Nui_Nap","IA-Vat_Komnou","IA-Long_Long_Rak","Hi-Hon_Hai_Co_Tien","Hi-Supu_Hujung","Hi-Kinabatagan")]

Test_Pop <- c(Vietnam, Ancient)

###############################Modern Language ancestry#######################################

result <- f4(W = c("Htin_Mal", "Atayal", "Miao", "Dai", "Naxi"), X = "Han", Y = Vietnam, Z = "Mbuti", data = data)
#result <- f4(W = c("Htin_Mal", "Atayal", "Miao", "Dai", "Lahu"), X = "Onge", Y = Vietnam, Z = "Mbuti", data = data)
#result <- f4(W = c("Htin_Mal", "Atayal", "Miao", "Dai", "Lahu"), X = "N-Tam_Pa_Ling", Y = Vietnam, Z = "Mbuti", data = data)
#result <- f4(W = c("Htin_Mal", "Atayal", "Miao", "Dai", "Naxi"), X = "N-Man_Bac", Y = Vietnam, Z = "Mbuti", data = data)
#result <- f4(W = c("Htin_Mal", "Atayal", "Miao", "Dai", "Lahu"), X = "BA-Nui_Nap", Y = Vietnam, Z = "Mbuti", data = data)
#result <- f4(W = c("Htin_Mal", "Atayal", "Miao", "Dai", "Lahu"), X = "IA-Long_Long_Rak", Y = Vietnam, Z = "Mbuti", data = data)
#result <- f4(W = c("Htin_Mal", "Atayal", "Miao", "Dai", "Lahu"), X = "Hi-Hon_Hai_Co_Tien", Y = Vietnam, Z = "Mbuti", data = data)

d <- result
colnames(info)[3] <- "Y"
d <- d %>% left_join(select(info, c("Y", "Language")))
#d$Y <- factor(d$Y, levels=c("P-Tianyuan","Ho-Pha_Faen","Ho-Gua_Cha","N-Gua_Cha","N-Man_Bac","N-Nam_Tun","N-Mai_Da_Dieu","N-Hon_Hai_Co_Tien","N-Tam_Pa_Ling","N-Tam_Hang","N-Oakaie","N-Loyang_Ujung","BA-Nui_Nap","IA-Vat_Komnou","IA-Long_Long_Rak","Hi-Hon_Hai_Co_Tien","Hi-Supu_Hujung","Hi-Kinabatagan",
#                            "Cham","Ede","Giarai","KhoMu","Kinh","Mang","Muong","BoY","CoLao","LaChi","Nung","Tay","Thai","Dao","Hmong","PaThen","Cong","HaNhi","LaHu","LoLo","PhuLa","Sila"), ordered=T)

d$Y <- factor(d$Y, levels=c("Cham","Ede","Giarai","KhoMu","Kinh","Mang","Muong","BoY","CoLao","LaChi","Nung","Tay","Thai","Dao","Hmong","PaThen","Cong","HaNhi","LaHu","LoLo","PhuLa","Sila"), ordered=T)

d$W <- factor(d$W, levels=c("Htin_Mal", "Atayal", "Miao", "Dai", "Lahu"), ordered=T)
#d$W <- factor(d$W, levels=c("Htin_Mal", "Atayal", "Miao", "Dai", "Naxi"), ordered=T)


Language = c("Austronesian"="#CC6633","Austro-Asiatic"="#9966CC","Hmong-Mien"="#FFCC33","Sino-Tibetan"="#66CC99","Tai-Kadai"="#CC0033",
             "Indo-European"="#FFCCFF","Dravidian"="#CC0099","Andamanese"="#660033","Mongolic"="#0000FF","Tungusic"="#66CCFF","Ancient"="#000000")

d %>%
  mutate(Significant=as.factor(abs(Zscore)>3)) %>%
  ggplot(aes(y=f4, x=Y, ymin=f4-3*stderr, ymax=f4+3*stderr, color=Language)) + 
  #geom_hline(data=d.max, aes(yintercept=MAX), color="grey", linetype="dashed") +
  geom_hline(yintercept=0, color="grey") +
  geom_point(aes(pch=Significant), size=2) + 
  geom_errorbar() +
  facet_wrap(~W, ncol=6) +
  coord_flip() +
  #scale_color_manual(values = c("FALSE"="#999999", "TRUE"="#CC0033")) +
  scale_color_manual(values = Language) +
  scale_shape_manual(values = c("FALSE"=4, "TRUE"=19)) +
  theme(axis.line.x = element_line(color="black", size = 0.5, linetype = 1),
        axis.line.y = element_line(color="black", size = 0.5, linetype = 1)) +
  theme(panel.background = element_blank()) +
  labs(x=NULL, color=NULL, pch="|Z| > 3") +
  theme(legend.text = element_text(size = 12), legend.title = element_text(size = 12)) +
  theme(axis.title.x = element_text(size = 14, face = "italic"), axis.title.y = element_text(size = 14)) + 
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12)) +
  theme(strip.text.x = element_text(size = 12), strip.text.y = element_text(size = 12))

############################################Ancient ancestry###########################################################

# Count the transversions only snps
count <- data %>% transversions_only() %>% count_snps()

#result <- data %>% f4(W = c("Onge", "Ho-Pha_Faen", "N-Tam_Pa_Ling", "N-Oakaie", "Hi-Hon_Hai_Co_Tien"), X = "Han", Y = Vietnam, Z = "Mbuti")
result <- data %>% f4(W = c("Onge", "Ho-Gua_Cha", "N-Gua_Cha", "N-Hon_Hai_Co_Tien", "Hi-Hon_Hai_Co_Tien"), X = "Han", Y = Vietnam, Z = "Mbuti")
#result <- data %>% transversions_only() %>% f4(W = c("P-Tianyuan", "Ho-Pha_Faen", "Ho-Gua_Cha", "Onge"), X = "Han", Y = Vietnam, Z = "Mbuti")
#result <- data %>% transversions_only() %>% f4(W = c("N-Gua_Cha", "N-Man_Bac", "N-Tam_Pa_Ling", "N-Oakaie"), X = "Han", Y = Vietnam, Z = "Mbuti")
#result <- data %>% transversions_only() %>% f4(W = c("BA-Nui_Nap","IA-Long_Long_Rak", "IA-Vat_Komnou"), X = "Han", Y = Vietnam, Z = "Mbuti")
#result <- data %>% transversions_only() %>% f4(W = c("Hi-Hon_Hai_Co_Tien","Hi-Kinabatagan"), X = "Han", Y = Vietnam, Z = "Mbuti")
#result <- data %>% f4(W = c("Lahu", "Naxi", "Yi"), X = "Miao", Y = Vietnam, Z = "Mbuti")
#result <- data %>% f4(W = c("Htin_Mal", "Mlabri"), X = "Onge", Y = Vietnam, Z = "Mbuti")

d <- result
colnames(info)[3] <- "Y"
d <- d %>% left_join(select(info, c("Y", "Language")))

d$Y <- factor(d$Y, levels=c("Cham","Ede","Giarai","KhoMu","Kinh","Mang","Muong","BoY","CoLao","LaChi","Nung","Tay","Thai","Dao","Hmong","PaThen","Cong","HaNhi","LaHu","LoLo","PhuLa","Sila"), ordered=T)

#d$W <- factor(d$W, levels=c("Onge", "Ho-Pha_Faen", "N-Tam_Pa_Ling", "N-Oakaie", "Hi-Hon_Hai_Co_Tien"), ordered=T)
d$W <- factor(d$W, levels=c("Onge", "Ho-Gua_Cha", "N-Gua_Cha", "N-Hon_Hai_Co_Tien", "Hi-Hon_Hai_Co_Tien"), ordered=T)
#d$W <- factor(d$W, levels=c("P-Tianyuan", "Ho-Pha_Faen", "Ho-Gua_Cha", "Onge"), ordered=T)
#d$W <- factor(d$W, levels=c("N-Gua_Cha", "N-Man_Bac", "N-Tam_Pa_Ling", "N-Oakaie"), ordered=T)
#d$W <- factor(d$W, levels=c("BA-Nui_Nap","IA-Long_Long_Rak", "IA-Vat_Komnou"), ordered=T)
#d$W <- factor(d$W, levels=c("Hi-Hon_Hai_Co_Tien","Hi-Kinabatagan"), ordered=T)
#d$W <- factor(d$W, levels=c("Lahu", "Naxi", "Yi"), ordered=T)
#d$W <- factor(d$W, levels=c("Htin_Mal", "Mlabri"), ordered=T)


Language = c("Austronesian"="#CC6633","Austro-Asiatic"="#9966CC","Hmong-Mien"="#FFCC33","Sino-Tibetan"="#66CC99","Tai-Kadai"="#CC0033",
             "Indo-European"="#FFCCFF","Dravidian"="#CC0099","Andamanese"="#660033","Mongolic"="#0000FF","Tungusic"="#66CCFF","Ancient"="#000000")

d %>%
  mutate(Significant=as.factor(abs(Zscore)>3)) %>%
  ggplot(aes(y=f4, x=Y, ymin=f4-3*stderr, ymax=f4+3*stderr, color=Language)) + 
  #geom_hline(data=d.max, aes(yintercept=MAX), color="grey", linetype="dashed") +
  geom_hline(yintercept=0, color="grey") +
  geom_point(aes(pch=Significant), size=2) + 
  geom_errorbar() +
  facet_wrap(~W, ncol=6) +
  coord_flip() +
  #scale_color_manual(values = c("FALSE"="#999999", "TRUE"="#CC0033")) +
  scale_color_manual(values = Language) +
  scale_shape_manual(values = c("FALSE"=4, "TRUE"=19)) +
  theme(axis.line.x = element_line(color="black", size = 0.5, linetype = 1),
        axis.line.y = element_line(color="black", size = 0.5, linetype = 1)) +
  theme(panel.background = element_blank()) +
  labs(x=NULL, color=NULL, pch="|Z| > 3") +
  theme(legend.text = element_text(size = 12), legend.title = element_text(size = 12)) +
  theme(axis.title.x = element_text(size = 14, face = "italic"), axis.title.y = element_text(size = 14)) + 
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12)) +
  theme(strip.text.x = element_text(size = 12), strip.text.y = element_text(size = 12))

###################Test for ancient samples during period transitions##################################
relabel <- data %>% relabel(AA_AN=c("Mlabri","Htin_Mal","Atayal","Borneo","Ami","Mamamwa"),TK_HM_ST=c("Dai","Miao","Lahu","Han","Han_NC"))
result <- relabel %>% f4(W="TK_HM_ST",X="AA_AN",Y=Ancient,Z="Mbuti")
#result <- relabel %>% transversions_only() %>% f4(W="TK_HM_ST",X="AA_AN",Y=Ancient,Z="Mbuti")
d <- result
d$Y <- factor(d$Y, levels=c("P-Tianyuan","Ho-Pha_Faen","Ho-Gua_Cha","N-Gua_Cha","N-Man_Bac","N-Nam_Tun","N-Mai_Da_Dieu","N-Hon_Hai_Co_Tien","N-Tam_Pa_Ling","N-Tam_Hang","N-Oakaie","N-Loyang_Ujung","BA-Nui_Nap","IA-Vat_Komnou","IA-Long_Long_Rak","Hi-Hon_Hai_Co_Tien","Hi-Supu_Hujung","Hi-Kinabatagan"))
d %>%
  +   mutate(Significant=as.factor(abs(Zscore)>3)) %>%
  +   ggplot(aes(y=f4, x=Y, ymin=f4-3*stderr, ymax=f4+3*stderr)) + 
  +   #geom_hline(data=d.max, aes(yintercept=MAX), color="grey", linetype="dashed") +
  +   geom_hline(yintercept=0, color="grey") +
  +   geom_point(aes(pch=Significant), size=2) + 
  +   geom_errorbar() +
  +   coord_flip() +
  +   #scale_color_manual(values = c("FALSE"="#999999", "TRUE"="#CC0033")) +
  +   scale_shape_manual(values = c("FALSE"=4, "TRUE"=19)) +
  +   theme(axis.line.x = element_line(color="black", size = 0.5, linetype = 1),
            +         axis.line.y = element_line(color="black", size = 0.5, linetype = 1)) +
  +   theme(panel.background = element_blank()) +
  +   labs(x=NULL, color=NULL, pch="|Z| > 3") +
  +   theme(legend.text = element_text(size = 12), legend.title = element_text(size = 12)) +
  +   theme(axis.title.x = element_text(size = 14, face = "italic"), axis.title.y = element_text(size = 14)) + 
  +   theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12)) +
  +   theme(strip.text.x = element_text(size = 12), strip.text.y = element_text(size = 12))

