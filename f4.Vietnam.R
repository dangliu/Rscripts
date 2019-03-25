# Script to run f4
# Last updated: 21.Mar.2019

# Libraries
library(tidyverse)
library(admixr)

# read data
data <- eigenstrat('/mnt/scratch/dang/Vietnam/f4/HO.ancient.outgroup')

# pop
ind <- read_ind(data)
table(ind$label)
pops <- unique(ind$label)
pops

# Test pops
Vietnam_noAN <- pops[pops%in% c("KhoMu","Kinh","Mang","Muong","BoY","CoLao","LaChi","Nung","Tay","Thai","Dao","Hmong","PaThen","Cong","HaNhi","LaHu","LoLo","PhuLa","Sila")]
Vietnam_noAA <- pops[pops%in% c("Cham","Ede","Giarai","BoY","CoLao","LaChi","Nung","Tay","Thai","Dao","Hmong","PaThen","Cong","HaNhi","LaHu","LoLo","PhuLa","Sila")]
Austronesian <- pops[pops%in% c("Cham","Ede","Giarai")]
AustroAsiatic <- pops[pops%in% c("KhoMu","Kinh","Mang","Muong")]
SinoTibetan <- pops[pops%in% c("Cong","HaNhi","LaHu","LoLo","PhuLa","Sila")]
TaiKadai <- pops[pops%in% c("BoY","CoLao","LaChi","Nung","Tay","Thai")]
HmongMien <- pops[pops%in% c("Dao","Hmong","PaThen")]
Ancient <- pops[pops%in% c("Man_Bac","Vat_Komnou","Nui_Nap","Oakaiel","Loyang_Ujung","Tam_Pa_Ping","Pha_Faen","Tam_Hang","Supu_Hujung","Kinabatagan","Gua_Cha","Long_Long_Rak","Hon_Hai_Co_Tien","Mai_Da_Dieu","Nam_Tun","Tianyuan")]
Vietnam_noAA_noAN <- pops[pops%in% c("BoY","CoLao","LaChi","Nung","Tay","Thai","Dao","Hmong","PaThen","Cong","HaNhi","LaHu","LoLo","PhuLa","Sila")]
Vietnam_noAA_noST <- pops[pops%in% c("BoY","CoLao","LaChi","Nung","Tay","Thai","Dao","Hmong","PaThen","Cham","Ede","Giarai")]
Vietnam_noAN_noTK <- pops[pops%in% c("Dao","Hmong","PaThen","Cong","HaNhi","LaHu","LoLo","PhuLa","Sila","KhoMu","Kinh","Mang","Muong")]
Vietnam_noHM_noTK <- pops[pops%in% c("Cham","Ede","Giarai","Cong","HaNhi","LaHu","LoLo","PhuLa","Sila","KhoMu","Kinh","Mang","Muong")]
Taiwan <- pops[pops%in% c("Atayal","Paiwan","Ami")]
MSEA <- pops[pops%in% c("Thai1","Mlabri","Htin_Mal","Cambodian")]
ISEA <- pops[pops%in% c("Borneo","Semende","Mamanwa")]


# re-label pops
#modif_d <- relabel(
#  data,
#  Taiwan = c("Atayal", "Paiwan", "Ami"),
#  MSEA = c("Mlabri", "Htin_Mal", "Cambodian"),
#  ISEA = c("Borneo", "Semende")
#)

# f4

out_path <- '/mnt/scratch/dang/Vietnam/f4/'

# Test Austronesian pops close to Taiwan, MESA or ISEA
result1 <- f4(W = MSEA, X = Taiwan, Y = Austronesian, Z = "Mbuti", data = data)
result2 <- f4(W = ISEA, X = Taiwan, Y = Austronesian, Z = "Mbuti", data = data)
result3 <- f4(W = ISEA, X = MSEA, Y = Austronesian, Z = "Mbuti", data = data)

write.table(result1, paste(out_path, "MSEAvsTaiwan.AN.txt", sep=""), sep="\t", quote=F, row.names=F)
write.table(result2, paste(out_path, "ISEAvsTaiwan.AN.txt", sep=""), sep="\t", quote=F, row.names=F)
write.table(result3, paste(out_path, "ISEAvsMSEA.AN.txt", sep=""), sep="\t", quote=F, row.names=F)

# Test if Austrnesian/AustroAsiatic are close to Ancient samples than other Vietnam ethnic groups
result4 <- f4(W = Vietnam_noAN, X = Austronesian, Y = Ancient, Z = "Mbuti", data = data)
result5 <- f4(W = Vietnam_noAA, X = AustroAsiatic, Y = Ancient, Z = "Mbuti", data = data)

write.table(result4, paste(out_path, "V.AN.Ancient.txt", sep=""), sep="\t", quote=F, row.names=F)
write.table(result5, paste(out_path, "V.AA.Ancient.txt", sep=""), sep="\t", quote=F, row.names=F)

# Test if Austronesian are close to AustroAsiatic than other Vietnam ethnic groups
result6 <- f4(W = Vietnam_noAA_noAN, X = AustroAsiatic, Y = Austronesian, Z = "Mbuti", data = data)

write.table(result6, paste(out_path, "V.AA.AN.txt", sep=""), sep="\t", quote=F, row.names=F)


# Swith W and X to see if there is potential gene flow from outgroup
result7 <- f4(W = Vietnam_noAA_noAN, X = Austronesian, Y = AustroAsiatic, Z = "Mbuti", data = data)

write.table(result7, paste(out_path, "V.AN.AA.txt", sep=""), sep="\t", quote=F, row.names=F)


# Test if AustroAsiatic are close to SinoTibetan than other Vietnam ethnic groups
result8 <- f4(W = Vietnam_noAA_noST, X = AustroAsiatic, Y = SinoTibetan, Z = "Mbuti", data = data)

write.table(result8, paste(out_path, "V.AA.ST.txt", sep=""), sep="\t", quote=F, row.names=F)

# Test if Austronesian have gene flow with TaiKadai groups
result9 <- f4(W = Vietnam_noAN_noTK, X = TaiKadai, Y = Austronesian, Z = "Mbuti", data = data)

write.table(result9, paste(out_path, "V.TK.AN.txt", sep=""), sep="\t", quote=F, row.names=F)

# Test if HmongMien have gene flow with TaiKadai groups
result10 <- f4(W = Vietnam_noHM_noTK, X = TaiKadai, Y = HmongMien, Z = "Mbuti", data = data)

write.table(result10, paste(out_path, "V.TK.HM.txt", sep=""), sep="\t", quote=F, row.names=F)


# Visualization

result1 %>%
  mutate(Significant=as.factor(abs(Zscore)>3)) %>%
  ggplot(aes(x=Y, y=f4, ymin=f4-3*stderr, ymax=f4+3*stderr, color=Significant)) + 
  geom_point() + 
  geom_errorbar() +
  geom_hline(yintercept=0) +
  facet_wrap(~W*X) +
  scale_color_manual(values = c("#999999", "#CC0033")) +
  labs(x=NULL) +
  theme(axis.line.x = element_line(color="black", size = 0.5, linetype = 1),
        axis.line.y = element_line(color="black", size = 0.5, linetype = 1)) +
  theme(panel.background = element_blank()) +
  theme(legend.text = element_text(size = 12), legend.title = element_text(size = 12)) +
  theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14)) + 
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12)) +
  theme(strip.text.x = element_text(size = 12), strip.text.y = element_text(size=12))





# Order the ancient samples by period
result4$Y <- factor(result4$Y, levels=c("Hon_Hai_Co_Tien","Kinabatagan","Supu_Hujung","Long_Long_Rak","Vat_Komnou","Nui_Nap","Loyang_Ujung","Mai_Da_Dieu","Nam_Tun","Oakaiel","Tam_Pa_Ping","Man_Bac","Tam_Hang","Gua_Cha","Pha_Faen","Tianyuan"))
result5$Y <- factor(result5$Y, levels=c("Hon_Hai_Co_Tien","Kinabatagan","Supu_Hujung","Long_Long_Rak","Vat_Komnou","Nui_Nap","Loyang_Ujung","Mai_Da_Dieu","Nam_Tun","Oakaiel","Tam_Pa_Ping","Man_Bac","Tam_Hang","Gua_Cha","Pha_Faen","Tianyuan"))

# Order vietnam groups
result6$W <- factor(result6$W, levels=c("BoY","CoLao","LaChi","Nung","Tay","Thai","Dao","Hmong","PaThen","Cham","Ede","Giarai","Cong","HaNhi","LaHu","LoLo","PhuLa","Sila","KhoMu","Kinh","Mang","Muong"))
result7$W <- factor(result7$W, levels=c("BoY","CoLao","LaChi","Nung","Tay","Thai","Dao","Hmong","PaThen","Cham","Ede","Giarai","Cong","HaNhi","LaHu","LoLo","PhuLa","Sila","KhoMu","Kinh","Mang","Muong"))
result8$W <- factor(result8$W, levels=c("BoY","CoLao","LaChi","Nung","Tay","Thai","Dao","Hmong","PaThen","Cham","Ede","Giarai","Cong","HaNhi","LaHu","LoLo","PhuLa","Sila","KhoMu","Kinh","Mang","Muong"))
result9$W <- factor(result9$W, levels=c("BoY","CoLao","LaChi","Nung","Tay","Thai","Dao","Hmong","PaThen","Cham","Ede","Giarai","Cong","HaNhi","LaHu","LoLo","PhuLa","Sila","KhoMu","Kinh","Mang","Muong"))
result10$W <- factor(result10$W, levels=c("BoY","CoLao","LaChi","Nung","Tay","Thai","Dao","Hmong","PaThen","Cham","Ede","Giarai","Cong","HaNhi","LaHu","LoLo","PhuLa","Sila","KhoMu","Kinh","Mang","Muong"))


# X tick 90 angle
result10 %>%
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
  labs(x=NULL) +
  theme(legend.text = element_text(size = 12), legend.title = element_text(size = 12)) +
  theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14)) + 
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12)) +
  theme(strip.text.x = element_text(size = 12), strip.text.y = element_text(size=12)) +
  theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1))



# Read result from table.txt
result <- read.table("/mnt/scratch/dang/Vietnam/f4/V.TK.HM.txt", header=T)

# Order the ancient samples by period
result$Y <- factor(result$Y, levels=c("Hon_Hai_Co_Tien","Kinabatagan","Supu_Hujung","Long_Long_Rak","Vat_Komnou","Nui_Nap","Loyang_Ujung","Mai_Da_Dieu","Nam_Tun","Oakaiel","Tam_Pa_Ping","Man_Bac","Tam_Hang","Gua_Cha","Pha_Faen","Tianyuan"))

# Order vietnam groups
result$W <- factor(result$W, levels=c("BoY","CoLao","LaChi","Nung","Tay","Thai","Dao","Hmong","PaThen","Cham","Ede","Giarai","Cong","HaNhi","LaHu","LoLo","PhuLa","Sila","KhoMu","Kinh","Mang","Muong"))



# X tick 90 angle
result %>%
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
  labs(x=NULL) +
  theme(legend.text = element_text(size = 12), legend.title = element_text(size = 12)) +
  theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14)) + 
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12)) +
  theme(strip.text.x = element_text(size = 12), strip.text.y = element_text(size=12)) +
  theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1))

