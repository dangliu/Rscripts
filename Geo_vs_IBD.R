# Geo vs. IBD

# author: "Dang Liu 01.Sep.2020"

# Last updated: 16.Sep.2020

# Libraries
library(data.table)
library(geosphere)
library(RColorBrewer)
library(magrittr)
library(tidyverse)
library(ade4)
library(vegan)

IBD_stats <- function(IBD,Info,Measure="L",Chr_exclude="",min=0,max=Inf){
  # Process the IBD table
  IBD <- IBD %>% filter(LEN > min & LEN <= max & CHR!=Chr_exclude)
  # Process the info table 
  IID_Pop_1 <- Info %>% select(IID,Label) %>% rename(IND1="IID",POP1="Label")
  IID_Pop_2 <- Info %>% select(IID,Label) %>% rename(IND2="IID",POP2="Label")
  Pop_Size <- Info %>% select(Label) %>% count(Label)
  # Combine Pop info for ind in IBD table
  IBD_pop <- IBD %>% left_join(IID_Pop_1) %>% left_join(IID_Pop_2)
  
  # Sum up length and copy number by each ind pair
  IBD_pop <- IBD_pop %>% mutate(N=1) %>% group_by(IND1,IND2) %>% mutate(Summed_L = sum(LEN), Summed_N = sum(N)) %>% 
    select(IND1,POP1,IND2,POP2,Summed_L,Summed_N) %>% distinct(.keep_all = T)
  
  # Sum up length and copy number by each pop pair
  IBD_pop <- IBD_pop %>% group_by(POP1,POP2)%>% mutate(Summed_L = sum(Summed_L), Summed_N = sum(Summed_N)) %>% 
    select(POP1,POP2,Summed_L,Summed_N) %>% distinct(.keep_all = T)
  # Get the average L and N for each pop pair considering their sample size pair
  Pop_Size_1 <- Pop_Size %>% rename(POP1="Label",SIZE1="n")
  Pop_Size_2 <- Pop_Size %>% rename(POP2="Label",SIZE2="n")
  
  IBD_pop <- IBD_pop %>% left_join(Pop_Size_1) %>% left_join(Pop_Size_2) %>% mutate(Avg_Sum_L = Summed_L / (SIZE1 * SIZE2), Avg_Sum_N = Summed_N / (SIZE1 * SIZE2))
  # Make IBD matrix L or N for all pop pair beginning with 0
  Pop_Labels <- Info %>% select(Label) %>% distinct(.keep_all = T)
  IBD_M <- matrix(0, nrow=length(Pop_Labels$Label), ncol=length(Pop_Labels$Label))
  rownames(IBD_M) <- Pop_Labels$Label
  colnames(IBD_M) <- Pop_Labels$Label
  
  if (Measure=="L"){
    # Fill in the IBD info
    for (i in (1:nrow(IBD_pop))){
      POP1 <- IBD_pop[i,]$POP1
      POP2 <- IBD_pop[i,]$POP2
      Avg_Sum_L <- IBD_pop[i,]$Avg_Sum_L
      IBD_M[POP1,POP2] = IBD_M[POP1,POP2] + Avg_Sum_L
      IBD_M[POP2,POP1] = IBD_M[POP2,POP1] + Avg_Sum_L
    }
  }
  if (Measure=="N"){
    # Fill in the IBD info
    for (i in (1:nrow(IBD_pop))){
      POP1 <- IBD_pop[i,]$POP1
      POP2 <- IBD_pop[i,]$POP2
      Avg_Sum_N <- IBD_pop[i,]$Avg_Sum_N
      IBD_M[POP1,POP2] = IBD_M[POP1,POP2] + Avg_Sum_N
      IBD_M[POP2,POP1] = IBD_M[POP2,POP1] + Avg_Sum_N
    }
  }
  return(IBD_M)
}

## Make geo-distance matrix
# Read info table
info <- read.table("/mnt/scratch/dang/Kula/Kula.meta.info", header=T)
head(info)

# Keep good quality Massim sample with geo-coordinates
info2 <- info %>% filter(Filter=="PASS" & is.na(Area)==FALSE & is.na(Longitude)==FALSE) %>% filter(Area=="Massim")

# Use the median of sample coordinates
info2 <- info2 %>% group_by(Label) %>% 
  summarise_at(vars(Latitude,Longitude), funs(median(.))) %>% 
  left_join(select(info,-(FID:IID),-(Latitude:Longitude))) %>%
  distinct(Label, .keep_all = TRUE)

## Geo matrix in kilometers
geo_m <- distm(cbind(info2$Longitude, info2$Latitude)*(1/1000))

rownames(geo_m) <- info2$Label
colnames(geo_m) <- info2$Label

# Define groups
Collingwood_Bay <- c("Northern", "Wanigela", "Airara")
W_Mas <- c("Fergusson", "Normanby", "Mainland_Eastern_Tip")
N_Mas <- c("Trobriand", "Gawa", "Woodlark", "Laughlan")
S_Mas <- c("Misima", "Western_Calvados", "Eastern_Calvados", "Sudest", "Rossel")

Massim <- c(Collingwood_Bay, W_Mas, N_Mas, S_Mas)

Kula <- c("Mainland_Eastern_Tip","Normanby","Fergusson","Trobriand", "Gawa", "Woodlark")
Half_Kula <- c("Laughlan","Misima")
Non_Kula <- c("Northern", "Wanigela", "Airara", "Western_Calvados", "Eastern_Calvados", "Sudest", "Rossel")


# Order the matrix
geo_m_order <- geo_m[Massim,Massim]
Massim_geo_m <- geo_m_order[Massim, Massim]
Massim_eff_geo_m <- Massim_geo_m
Kula_geo_m <- geo_m[Kula,Kula]
Half_Kula_geo_m <- geo_m[c(Half_Kula, Kula), c(Half_Kula, Kula)]
for (n in rownames(Kula_geo_m)){
  for (k in colnames(Kula_geo_m)){
    Half_Kula_geo_m[n,k] <- NA
  } 
}

Non_Kula_geo_m <- Massim_geo_m
for (n in rownames(Half_Kula_geo_m)){
  for (k in colnames(Half_Kula_geo_m)){
    Non_Kula_geo_m[n,k] <- NA
  } 
}

#write.table(geo_m_order,"/r1/people/dang_liu/Projects/Kula/IBD/geo_m_order.csv",sep=",")
#write.table(Kula_geo_m,"/r1/people/dang_liu/Projects/Kula/IBD/Kula_geo_m.csv",sep=",")

## Effctive geo distance (Kula trade distance)
# Read Kula effective geo csv

Kula_all_eff_geo <- read_csv("/r1/people/dang_liu/Projects/Kula/IBD/Kula_effective_geo_m.csv")
Kula_all_eff_geo_m <- as.matrix(Kula_all_eff_geo[,2:9])
rownames(Kula_all_eff_geo_m) <- Kula_all_eff_geo$X1

Kula_eff_geo_m <- Kula_all_eff_geo_m[Kula, Kula]


for (n in rownames(Kula_eff_geo_m)){
  for (k in colnames(Kula_eff_geo_m)){
    Massim_eff_geo_m[n,k] <- Kula_eff_geo_m[n,k]
  } 
}

Half_Kula_eff_geo_m <- Kula_all_eff_geo_m[c(Half_Kula, Kula), c(Half_Kula, Kula)]
for (n in rownames(Kula_eff_geo_m)){
  for (k in colnames(Kula_eff_geo_m)){
    Half_Kula_eff_geo_m[n,k] <- NA
  } 
}

Non_Kula_eff_geo_m <- Massim_geo_m
for (n in rownames(Half_Kula_eff_geo_m)){
  for (k in colnames(Half_Kula_eff_geo_m)){
    Non_Kula_eff_geo_m[n,k] <- NA
  } 
}

## Make IBD similarity matrix
# Read the IBD sharing input and info file
IBD_path = "/mnt/scratch/dang/Kula/IBD/conHap/all.lPSC.Merged"
Info_path = "/mnt/scratch/dang/Kula/Kula.meta.info"

IBD <- read_delim(IBD_path,delim="\t",col_names=F)
colnames(IBD) <- c("IND1","HAP1","IND2","HAP2","CHR","BEGIN","END","LOD","LEN")
Info <- read_delim(Info_path,delim="\t",col_names=T) %>% filter(Filter=="PASS")

IBD_M <- IBD_stats(IBD,Info)

# Add range
#IBD_M <- IBD_stats(IBD,Info,min=1,max=5)
#IBD_M <- IBD_stats(IBD,Info,min=5,max=10)
#IBD_M <- IBD_stats(IBD,Info,min=10)

# Order the Matrix and excluded those are not used as all populations
S_IBD_M <- IBD_M
# Calculate the normalized similarity value by dividing the between pop sharing with the average of each within pop sharing
for (i in rownames(S_IBD_M)){
  for(j in colnames(S_IBD_M)) {
    S_IBD_M[i,j] <- (2 * IBD_M[i,j]) / (IBD_M[i,i] + IBD_M[j,j])
    #S_IBD_M[i,j] <- 1 - (2 * IBD_M[i,j]) / (IBD_M[i,i] + IBD_M[j,j])
  }
}

Massim_IBD_m <- S_IBD_M[Massim,Massim]
Kula_IBD_m <- S_IBD_M[Kula,Kula]
Half_Kula_IBD_m <- S_IBD_M[c(Half_Kula, Kula), c(Half_Kula, Kula)]
for (n in rownames(Kula_eff_geo_m)){
  for (k in colnames(Kula_eff_geo_m)){
    Half_Kula_IBD_m[n,k] <- NA
  } 
}

Non_Kula_IBD_m <- S_IBD_M[Massim,Massim]
for (n in rownames(Half_Kula_IBD_m)){
  for (k in colnames(Half_Kula_IBD_m)){
    Non_Kula_IBD_m[n,k] <- NA
  } 
}

## Mantel test for correlation between matrices 
set.seed(10000)
# Massim
Massim_IBD_m.dist <- as.dist(Massim_IBD_m)
Massim_geo_m.dist <- as.dist(Massim_geo_m)
Massim_eff_geo_m.dist <- as.dist(Massim_eff_geo_m)
#r1 <- mantel.rtest(Massim_IBD_m.dist, Massim_geo_m.dist, nrepet=10000)
#r1
#1 - r1$pvalue
#plot(r1)
#r1 <- mantel(Massim_IBD_m.dist, Massim_geo_m.dist, method="spearman", permutations=10000, na.rm=T)
r1 <- mantel(Massim_IBD_m.dist, Massim_geo_m.dist, method="pearson", permutations=10000, na.rm=T)
r1
1 - r1$signif
#r2 <- mantel.rtest(Massim_IBD_m.dist, Massim_eff_geo_m.dist, nrepet=10000)
#r2
#1 - r2$pvalue
#plot(r2)
#r2 <- mantel(Massim_IBD_m.dist, Massim_eff_geo_m.dist, method="spearman", permutations=10000, na.rm=T)
r2 <- mantel(Massim_IBD_m.dist, Massim_eff_geo_m.dist, method="pearson", permutations=10000, na.rm=T)
r2
1 - r2$signif
# Kula only
Kula_IBD_m.dist <- as.dist(Kula_IBD_m)
Kula_geo_m.dist <- as.dist(Kula_geo_m)
Kula_eff_geo_m.dist <- as.dist(Kula_eff_geo_m)
#r1 <- mantel.rtest(Kula_IBD_m.dist, Kula_geo_m.dist, nrepet=10000)
#r1
#1 - r1$pvalue
#plot(r1)
#r1 <- mantel(Kula_IBD_m.dist, Kula_geo_m.dist, method="spearman", permutations=10000, na.rm=T)
r1 <- mantel(Kula_IBD_m.dist, Kula_geo_m.dist, method="pearson", permutations=10000, na.rm=T)
r1
1 - r1$signif
#r2 <- mantel.rtest(Kula_IBD_m.dist, Kula_eff_geo_m.dist, nrepet=10000)
#r2
#1 - r2$pvalue
#plot(r2)
#r2 <- mantel(Kula_IBD_m.dist, Kula_eff_geo_m.dist, method="spearman", permutations=10000, na.rm=T)
r2 <- mantel(Kula_IBD_m.dist, Kula_eff_geo_m.dist, method="pearson", permutations=10000, na.rm=T)
r2
1 - r2$signif
# Half Kula only
Half_Kula_IBD_m.dist <- as.dist(Half_Kula_IBD_m)
Half_Kula_geo_m.dist <- as.dist(Half_Kula_geo_m)
Half_Kula_eff_geo_m.dist <- as.dist(Half_Kula_eff_geo_m)
#r1 <- mantel(Half_Kula_IBD_m.dist, Half_Kula_geo_m.dist, method="spearman", permutations=10000, na.rm=T)
r1 <- mantel(Half_Kula_IBD_m.dist, Half_Kula_geo_m.dist, method="pearson", permutations=10000, na.rm=T)
r1
1 - r1$signif
#r2 <- mantel(Half_Kula_IBD_m.dist, Half_Kula_eff_geo_m.dist, method="spearman", permutations=10000, na.rm=T)
r2 <- mantel(Half_Kula_IBD_m.dist, Half_Kula_eff_geo_m.dist, method="pearson", permutations=10000, na.rm=T)
r2
1 - r2$signif

# Non Kula
Non_Kula_IBD_m.dist <- as.dist(Non_Kula_IBD_m)
Non_Kula_geo_m.dist <- as.dist(Non_Kula_geo_m)
Non_Kula_eff_geo_m.dist <- as.dist(Non_Kula_eff_geo_m)
#r1 <- mantel(Non_Kula_IBD_m.dist, Non_Kula_geo_m.dist, method="spearman", permutations=10000, na.rm=T)
r1 <- mantel(Non_Kula_IBD_m.dist, Non_Kula_geo_m.dist, method="pearson", permutations=10000, na.rm=T)
#mantel(Non_Kula_IBD_m.dist, Non_Kula_geo_m.dist, method="spearman", permutations=10000, na.rm=T)
r1
1 - r1$signif





# For adding range
#Kula_IBD_m_1_5 <- S_IBD_M[Kula,Kula]
#Kula_IBD_m_5_10 <- S_IBD_M[Kula,Kula]
#Kula_IBD_m_over10 <- S_IBD_M[Kula,Kula]

#Kula_IBD_m_1_5 <- S_IBD_M[Massim,Massim]
#Kula_IBD_m_5_10 <- S_IBD_M[Massim,Massim]
#Kula_IBD_m_over10 <- S_IBD_M[Massim,Massim]


## Convert both matrix into dataframe and combine them
# don't count the within-pop sharing
#geo_d <- Kula_geo_m %>% melt() %>% na.omit() %>% arrange(., Var1) %>% setNames(c('Pop1', 'Pop2', 'Geo')) %>% mutate(Type="Shortest")
#eff_geo_d <- Kula_eff_geo_m %>% melt() %>% na.omit() %>% arrange(., Var1) %>% setNames(c('Pop1', 'Pop2', 'Geo')) %>% mutate(Type="Kula")
#ibd_d <- Kula_IBD_m %>% melt() %>% na.omit() %>% arrange(., Var1) %>% setNames(c('Pop1', 'Pop2', 'IBD'))

geo_d <- Massim_geo_m %>% melt() %>% na.omit() %>% arrange(., Var1) %>% setNames(c('Pop1', 'Pop2', 'Geo')) %>% mutate(Type="Shortest")
eff_geo_d <- Massim_eff_geo_m %>% melt() %>% na.omit() %>% arrange(., Var1) %>% setNames(c('Pop1', 'Pop2', 'Geo')) %>% mutate(Type="Kula")
ibd_d <- Massim_IBD_m %>% melt() %>% na.omit() %>% arrange(., Var1) %>% setNames(c('Pop1', 'Pop2', 'IBD'))




# Add range
#ibd_d_1_5 <- Kula_IBD_m_1_5 %>% melt() %>% na.omit() %>% arrange(., Var1) %>% setNames(c('Pop1', 'Pop2', 'IBD')) %>% mutate(Range="1-5")
#ibd_d_5_10 <- Kula_IBD_m_5_10 %>% melt() %>% na.omit() %>% arrange(., Var1) %>% setNames(c('Pop1', 'Pop2', 'IBD')) %>% mutate(Range="5-10")
#ibd_d_over10 <- Kula_IBD_m_over10 %>% melt() %>% na.omit() %>% arrange(., Var1) %>% setNames(c('Pop1', 'Pop2', 'IBD')) %>% mutate(Range="over10")


All_d <- ibd_d %>% left_join(rbind(geo_d, eff_geo_d)) %>% 
  mutate(Type2="Non_Kula") %>% 
  mutate(Type2=replace(Type2, Pop1%in%Kula & Pop2%in%Kula, "Kula")) %>%
  mutate(Type2=replace(Type2, Pop1%in%Half_Kula & Pop2%in%Half_Kula, "Half_Kula")) %>%
  mutate(Type2=replace(Type2, Pop1%in%Kula & Pop2%in%Half_Kula, "Half_Kula")) %>%
  mutate(Type2=replace(Type2, Pop1%in%Half_Kula & Pop2%in%Kula, "Half_Kula"))


# Add IBD range
#All_d <- rbind(ibd_d_1_5, ibd_d_5_10, ibd_d_over10) %>% left_join(rbind(geo_d, eff_geo_d)) %>%
#  mutate(Type2="Non_Kula") %>% 
#  mutate(Type2=replace(Type2, Pop1%in%Kula & Pop2%in%Kula, "Kula")) %>%
#  mutate(Type2=replace(Type2, Pop1%in%Half_Kula & Pop2%in%Half_Kula, "Half_Kula")) %>%
#  mutate(Type2=replace(Type2, Pop1%in%Kula & Pop2%in%Half_Kula, "Half_Kula")) %>%
#  mutate(Type2=replace(Type2, Pop1%in%Half_Kula & Pop2%in%Kula, "Half_Kula"))

#range.labs <- c("1 to 5 cM", "5 to 10 cM", "Over 10 cM")
#names(range.labs) <- c("1-5", "5-10", "over10")

All_d$Type <- factor(All_d$Type, levels=c("Shortest", "Kula"), ordered=T)
All_d$Type2 <- factor(All_d$Type2, levels=c("Kula", "Half_Kula", "Non_Kula"), ordered=T)

Group_color = c("Kula"="#6A3884", "Half_Kula"="#21908C", "Non_Kula"="#C4B286")

## Plot
jitter <- position_jitter(width = 0.025, height = 0.025)
All_d %>% ggplot(aes(x = Geo, y = IBD, fill=Type2)) +
  geom_point(size=4, pch=21, position=jitter) +
  #geom_smooth(method = lm) +
  labs(x = "Geo distance (km)", y = "IBD similarity", fill="Group") +
  facet_wrap(.~Type, nrow=1) +
  scale_fill_manual(values = Group_color) +
  #scale_color_manual(values = c("red","black")) +
  #scale_shape_manual(values = c(19, 1)) +
  #facet_wrap(.~Type*Range, nrow=2, labeller = labeller(Range=range.labs)) +
  theme_bw() +
  theme(legend.text = element_text(size = 12), legend.title = element_text(size = 12)) +
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12)) +
  theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14)) +
  theme(strip.text.x = element_text(size = 12), strip.text.y = element_text(size = 12)) +
  theme(legend.background = element_rect(color="black", 
                                         linetype="solid"))


