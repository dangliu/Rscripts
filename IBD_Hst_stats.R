## IBD Hst stats

# author: "Dang Liu 30.Aug.2020"

# Last updated: 17.Sep.2020

# Use libraries
library(tidyverse)
library(RColorBrewer)
library(gtools)
library(data.table)
library(ggrepel)

### Define functions
# IBD_stats function
# Read IBD and info files, filtering the IBD result by min, max, and chr
# Can decide for calulating L or N
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

# Hst function
Hst <- function(IBD_M, all, within, within_exclude=""){
  # Order the Matrix and excluded those are not used as all populations
  IBD_M <- IBD_M[all,all]
  S_IBD_M <- IBD_M[all,all]
  # Calculate the normalized similarity value by dividing the between pop sharing with the average of each within pop sharing
  for (i in rownames(S_IBD_M)){
    for(j in colnames(S_IBD_M)) {
      S_IBD_M[i,j] <- (2 * IBD_M[i,j]) / (IBD_M[i,i] + IBD_M[j,j])
      #S_IBD_M[i,j] <- (2 * IBD_M[i,j]) / (IBD_M[i,i] + IBD_M[j,j])
    }
  }
  # Hst = (Sw - Sa) / (1 - Sa)
  S_IBD_M_within <- S_IBD_M[within,within]
  if (within_exclude!=""){
    for (r in within_exclude){
      for (c in within_exclude){
        S_IBD_M_within[r,c] <- NA
      }
    }
  }
  S_IBD_M_all <- S_IBD_M
  d_Hst = (mean(S_IBD_M_within[upper.tri(S_IBD_M_within)], na.rm=T) - mean(S_IBD_M_all[upper.tri(S_IBD_M_all)], na.rm=T)) / (1 - mean(S_IBD_M_all[upper.tri(S_IBD_M_all)], na.rm=T))
  return(d_Hst)
}

# Std function
std <- function(x) sd(x)/sqrt(length(x))

# Hst_mean_std function
# By jackknife each of the chr 
Hst_mean_std <- function(IBD, Info, Measure="L", min=0, max=Inf, all, within, within_exclude=""){
  Hst_list <- sapply(1:22, function(x) Hst(IBD_M=IBD_stats(IBD,Info,Measure=Measure,min=min,max=max,Chr_exclude=x),
                               all=all,
                               within=within,
                               within_exclude=within_exclude))
  Hst_mean = mean(Hst_list)
  Hst_std = std(Hst_list)
  Hst_mean_std <- c(Hst_mean, Hst_std)
  return(Hst_mean_std)
}

# plot_all function
plot_all <- function(d_all, Nrow=3) {
  d_all %>% ggplot() +
    geom_point(aes(x = End, y = Hst_L_mean, color="black"), size = 2.5) +
    geom_errorbar(aes(x = End, y = Hst_L_mean, ymin = Hst_L_mean - 3 * Hst_L_std, ymax = Hst_L_mean + 3 * Hst_L_std), width=.5, color="black") +
    geom_point(aes(x = End, y = Hst_N_mean, color="red"), size = 2.5) +
    geom_errorbar(aes(x = End, y = Hst_N_mean, ymin = Hst_N_mean - 3 * Hst_N_std, ymax = Hst_N_mean + 3 * Hst_N_std), width=.5, color="red") +
    #geom_errorbar(aes(ymin = Hst_N_mean - 3 * Hst_N_std, ymax = Hst_N_mean + 3 * Hst_N_std, color="red")) +
    scale_color_identity(name = "Calculation Type",
                         breaks = c("black", "red"),
                         labels = c("Avg. L", "Avg. N"),
                         guide = "legend") +
    #geom_line(aes(x = End, y = Hst_L), linetype="dotted") +
    #geom_line(aes(x = End, y = Hst_N), linetype="dotted", color="red") + 
    geom_smooth(aes(x = End, y = Hst_L_mean), linetype="dotted", color="black",  se =F, size=0.75) +
    geom_smooth(aes(x = End, y = Hst_N_mean), linetype="dotted", color="red",  se=F, size=0.75) +
    scale_x_continuous(breaks = c(5, 6, 7, 8, 9, 10, 12), 
                       labels = c("5","6","7","8","9","10","Over 10"),
                       sec.axis = dup_axis(name = "Lower bound of IBD block length (cM)",
                                           labels = c("1","2","3","4","5","6","10"))) +
    labs(x = "Upper bound of IBD block length (cM)", y = "Fst-like stats on IBD matrix") +
    facet_wrap(.~Group, nrow=Nrow) +
    theme_bw() +
    theme(legend.text = element_text(size = 12), legend.title = element_text(size = 12)) +
    theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10)) +
    theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14)) +
    #theme(legend.position = c(0.75, 0.1)) +
    theme(legend.background = element_rect(color="black", 
                                           linetype="solid"))
  
}



#############

# Read the IBD sharing input and info file
IBD_path = "/mnt/scratch/dang/Kula/IBD/conHap/all.lPSC.Merged"
Info_path = "/mnt/scratch/dang/Kula/Kula.meta.info"

IBD <- read_delim(IBD_path,delim="\t",col_names=F)
colnames(IBD) <- c("IND1","HAP1","IND2","HAP2","CHR","BEGIN","END","LOD","LEN")
Info <- read_delim(Info_path,delim="\t",col_names=T) %>% filter(Filter=="PASS")

# Define groups
Collingwood_Bay <- c("Northern", "Wanigela", "Airara")
W_Mas <- c("Fergusson", "Normanby", "Mainland_Eastern_Tip")
N_Mas <- c("Trobriand", "Gawa", "Woodlark", "Laughlan")
S_Mas <- c("Misima", "Western_Calvados", "Eastern_Calvados", "Sudest", "Rossel")

Massim <- c(Collingwood_Bay, W_Mas, N_Mas, S_Mas)
Outgroup <- c("Africa", "WestEurasia")
Near_Out <- c("EastAsia", "EastAsia_AN", "Australia")
PNG_H <- c("S_Papuan", "Southern_Highlands", "Enga", "Western_Highlands", "Chimbu", "Eastern_Highlands", "Gulf", "Madang_Highland")
PNG_L <- c("Madang_Lowland", "Morobe", "East_Sepik", "Western", "Central")
Bismark <- c("Manus_New_Ireland", "West_New_Britain", "East_New_Britain")
Solomon <- c("Bougainville", "Vella_Lavella", "Malaita", "Santa_Cruz", "Bellona_Rennell", "Tikopia")

Kula <- c("Mainland_Eastern_Tip","Normanby","Fergusson","Trobriand", "Gawa", "Woodlark")
Half_Kula <- c("Laughlan","Misima")
Non_Kula <- c("Northern", "Wanigela", "Airara", "Western_Calvados", "Eastern_Calvados", "Sudest", "Rossel")



D <- c(PNG_H, PNG_L, Bismark, Solomon, Massim)


###########

# Regions within PNG

# Make a dataframe to run the functions
Hst_d <- tibble(Group=rep(c("PNG_H","PNG_L","Bismark","Solomon","Massim"),7),
                Begin=c(rep(1,5),rep(2,5),rep(3,5),rep(4,5),rep(5,5),rep(6,5),rep(10,5)),
                End=c(rep(5,5),rep(6,5),rep(7,5),rep(8,5),rep(9,5),rep(10,5),rep(Inf,5)),
                Hst_L_mean=rep(0,35),
                Hst_L_std=rep(0,35),
                Hst_N_mean=rep(0,35),
                Hst_N_std=rep(0,35),
                )

# Assign group to group
assign("PNG_H", PNG_H)
assign("PNG_L", PNG_L)
assign("Bismark", Bismark)
assign("Solomon", Solomon)
assign("Massim", Massim)

# Run functions for each group and length range
for (i in 1:nrow(Hst_d)){
  Begin = Hst_d$Begin[i]
  End = Hst_d$End[i]
  Group = Hst_d$Group[i]
  Hst_mean_std_L <- Hst_mean_std(IBD, Info, min=Begin, max=End, all=D, within=eval(as.symbol(Group)))
  Hst_d$Hst_L_mean[i] = Hst_mean_std_L[1]
  Hst_d$Hst_L_std[i] = Hst_mean_std_L[2]
  Hst_mean_std_N <- Hst_mean_std(IBD, Info, Measure="N", min=Begin, max=End, all=D, within=eval(as.symbol(Group)))
  Hst_d$Hst_N_mean[i] = Hst_mean_std_N[1]
  Hst_d$Hst_N_std[i] = Hst_mean_std_N[2]
}


#Hst_d <- Hst_d %>% group_by(Group,Begin,End) %>% 
#  mutate(Hst_L_mean = Hst_mean_std(IBD, Info, min=Begin, max=End, all=D, within=eval(as.symbol(Group)))[1]) %>%
#  mutate(Hst_L_std = Hst_mean_std(IBD, Info, min=Begin, max=End, all=D, within=eval(as.symbol(Group)))[2]) %>%
#  mutate(Hst_N_mean = Hst_mean_std(IBD, Info, Measure="N", min=Begin, max=End, all=D, within=eval(as.symbol(Group)))[1]) %>%
#  mutate(Hst_N_std = Hst_mean_std(IBD, Info, Measure="N", min=Begin, max=End, all=D, within=eval(as.symbol(Group)))[2])

# Plot the result
Hst_d[Hst_d$Begin==10,]$End <- 12
plot_all(Hst_d)


# Kula groups within PNG 
# Assign group to group
assign("Fergusson-Trobriand", c("Fergusson","Trobriand"))
assign("Trobriand-Gawa", c("Trobriand","Gawa"))
assign("Woodlark-Misima", c("Woodlark","Misima"))
assign("Laughlan-Misima", c("Laughlan","Misima"))
assign("Misima-Mainland_Eastern_Tip", c("Misima","Mainland_Eastern_Tip"))
assign("Gawa-Woodlark", c("Gawa","Woodlark"))
assign("Woodlark-Laughlan", c("Woodlark", "Laughlan"))
assign("Misima-Rossel", c("Misima", "Rossel"))
assign("Mainland_Eastern_Tip-Normanby", c("Mainland_Eastern_Tip", "Normanby"))
assign("Normanby-Fergusson", c("Normanby", "Fergusson"))

# Make a dataframe to run the functions
Hst_d <- tibble(Group=rep(c("Fergusson-Trobriand","Trobriand-Gawa","Woodlark-Misima","Laughlan-Misima","Misima-Mainland_Eastern_Tip",
                            "Gawa-Woodlark","Woodlark-Laughlan","Misima-Rossel","Mainland_Eastern_Tip-Normanby","Normanby-Fergusson"),7),
                Begin=c(rep(1,10),rep(2,10),rep(3,10),rep(4,10),rep(5,10),rep(6,10),rep(10,10)),
                End=c(rep(5,10),rep(6,10),rep(7,10),rep(8,10),rep(9,10),rep(10,10),rep(Inf,10)),
                Hst_L_mean=rep(0,70),
                Hst_L_std=rep(0,70),
                Hst_N_mean=rep(0,70),
                Hst_N_std=rep(0,70),
)
# Run functions for each group and length range
for (i in 1:nrow(Hst_d)){
  Begin = Hst_d$Begin[i]
  End = Hst_d$End[i]
  Group = Hst_d$Group[i]
  Hst_mean_std_L <- Hst_mean_std(IBD, Info, min=Begin, max=End, all=D, within=eval(as.symbol(Group)))
  Hst_d$Hst_L_mean[i] = Hst_mean_std_L[1]
  Hst_d$Hst_L_std[i] = Hst_mean_std_L[2]
  Hst_mean_std_N <- Hst_mean_std(IBD, Info, Measure="N", min=Begin, max=End, all=D, within=eval(as.symbol(Group)))
  Hst_d$Hst_N_mean[i] = Hst_mean_std_N[1]
  Hst_d$Hst_N_std[i] = Hst_mean_std_N[2]
}

# Plot the result
Hst_d$Group <- factor(Hst_d$Group, levels=c("Mainland_Eastern_Tip-Normanby","Normanby-Fergusson","Fergusson-Trobriand","Trobriand-Gawa","Gawa-Woodlark","Woodlark-Laughlan",
                                            "Woodlark-Misima","Laughlan-Misima","Misima-Mainland_Eastern_Tip","Misima-Rossel"), ordered=T)
                                            
Hst_d[Hst_d$Begin==10,]$End <- 12
plot_all(Hst_d)


# Kula groups within Massim
# Make a dataframe to run the functions
Hst_d <- tibble(Group=rep(c("Fergusson-Trobriand","Trobriand-Gawa","Woodlark-Misima","Laughlan-Misima","Misima-Mainland_Eastern_Tip",
                            "Gawa-Woodlark","Woodlark-Laughlan","Misima-Rossel","Mainland_Eastern_Tip-Normanby","Normanby-Fergusson"),7),
                Begin=c(rep(1,10),rep(2,10),rep(3,10),rep(4,10),rep(5,10),rep(6,10),rep(10,10)),
                End=c(rep(5,10),rep(6,10),rep(7,10),rep(8,10),rep(9,10),rep(10,10),rep(Inf,10)),
                Hst_L_mean=rep(0,70),
                Hst_L_std=rep(0,70),
                Hst_N_mean=rep(0,70),
                Hst_N_std=rep(0,70),
)
# Run functions for each group and length range
for (i in 1:nrow(Hst_d)){
  Begin = Hst_d$Begin[i]
  End = Hst_d$End[i]
  Group = Hst_d$Group[i]
  Hst_mean_std_L <- Hst_mean_std(IBD, Info, min=Begin, max=End, all=Massim, within=eval(as.symbol(Group)))
  Hst_d$Hst_L_mean[i] = Hst_mean_std_L[1]
  Hst_d$Hst_L_std[i] = Hst_mean_std_L[2]
  Hst_mean_std_N <- Hst_mean_std(IBD, Info, Measure="N", min=Begin, max=End, all=Massim, within=eval(as.symbol(Group)))
  Hst_d$Hst_N_mean[i] = Hst_mean_std_N[1]
  Hst_d$Hst_N_std[i] = Hst_mean_std_N[2]
}

# Plot the result
Hst_d$Group <- factor(Hst_d$Group, levels=c("Mainland_Eastern_Tip-Normanby","Normanby-Fergusson","Fergusson-Trobriand","Trobriand-Gawa","Gawa-Woodlark","Woodlark-Laughlan",
                                            "Woodlark-Misima","Laughlan-Misima","Misima-Mainland_Eastern_Tip","Misima-Rossel"), ordered=T)

Hst_d[Hst_d$Begin==10,]$End <- 12
plot_all(Hst_d)



# Solomon within PNG
# Assign group to group
assign("Bougainville-Vella_Lavella", c("Bougainville", "Vella_Lavella"))
assign("Vella_Lavella-Malaita", c("Vella_Lavella", "Malaita"))
assign("Bellona_Rennell-Tikopia", c("Bellona_Rennell", "Tikopia"))
assign("Santa_Cruz-Bellona_Rennell", c("Santa_Cruz", "Bellona_Rennell"))
assign("Bougainville-Santa_Cruz", c("Bougainville", "Santa_Cruz"))


# Make a dataframe to run the functions
Hst_d <- tibble(Group=rep(c("Bougainville-Vella_Lavella","Vella_Lavella-Malaita","Bellona_Rennell-Tikopia","Santa_Cruz-Bellona_Rennell","Bougainville-Santa_Cruz"),7),
                Begin=c(rep(1,5),rep(2,5),rep(3,5),rep(4,5),rep(5,5),rep(6,5),rep(10,5)),
                End=c(rep(5,5),rep(6,5),rep(7,5),rep(8,5),rep(9,5),rep(10,5),rep(Inf,5)),
                Hst_L_mean=rep(0,35),
                Hst_L_std=rep(0,35),
                Hst_N_mean=rep(0,35),
                Hst_N_std=rep(0,35),
)

# Run functions for each group and length range
for (i in 1:nrow(Hst_d)){
  Begin = Hst_d$Begin[i]
  End = Hst_d$End[i]
  Group = Hst_d$Group[i]
  Hst_mean_std_L <- Hst_mean_std(IBD, Info, min=Begin, max=End, all=D, within=eval(as.symbol(Group)))
  Hst_d$Hst_L_mean[i] = Hst_mean_std_L[1]
  Hst_d$Hst_L_std[i] = Hst_mean_std_L[2]
  Hst_mean_std_N <- Hst_mean_std(IBD, Info, Measure="N", min=Begin, max=End, all=D, within=eval(as.symbol(Group)))
  Hst_d$Hst_N_mean[i] = Hst_mean_std_N[1]
  Hst_d$Hst_N_std[i] = Hst_mean_std_N[2]
}

# Plot the result
Hst_d$Group <- factor(Hst_d$Group, levels=c("Bougainville-Vella_Lavella","Vella_Lavella-Malaita","Bellona_Rennell-Tikopia","Santa_Cruz-Bellona_Rennell","Bougainville-Santa_Cruz"), ordered=T)

Hst_d[Hst_d$Begin==10,]$End <- 12
plot_all(Hst_d)


# Solomon within Solomon
# Make a dataframe to run the functions
Hst_d <- tibble(Group=rep(c("Bougainville-Vella_Lavella","Vella_Lavella-Malaita","Bellona_Rennell-Tikopia","Santa_Cruz-Bellona_Rennell","Bougainville-Santa_Cruz"),7),
                Begin=c(rep(1,5),rep(2,5),rep(3,5),rep(4,5),rep(5,5),rep(6,5),rep(10,5)),
                End=c(rep(5,5),rep(6,5),rep(7,5),rep(8,5),rep(9,5),rep(10,5),rep(Inf,5)),
                Hst_L_mean=rep(0,35),
                Hst_L_std=rep(0,35),
                Hst_N_mean=rep(0,35),
                Hst_N_std=rep(0,35),
)

# Run functions for each group and length range
for (i in 1:nrow(Hst_d)){
  Begin = Hst_d$Begin[i]
  End = Hst_d$End[i]
  Group = Hst_d$Group[i]
  Hst_mean_std_L <- Hst_mean_std(IBD, Info, min=Begin, max=End, all=Solomon, within=eval(as.symbol(Group)))
  Hst_d$Hst_L_mean[i] = Hst_mean_std_L[1]
  Hst_d$Hst_L_std[i] = Hst_mean_std_L[2]
  Hst_mean_std_N <- Hst_mean_std(IBD, Info, Measure="N", min=Begin, max=End, all=Solomon, within=eval(as.symbol(Group)))
  Hst_d$Hst_N_mean[i] = Hst_mean_std_N[1]
  Hst_d$Hst_N_std[i] = Hst_mean_std_N[2]
}

# Plot the result
Hst_d$Group <- factor(Hst_d$Group, levels=c("Bougainville-Vella_Lavella","Vella_Lavella-Malaita","Bellona_Rennell-Tikopia","Santa_Cruz-Bellona_Rennell","Bougainville-Santa_Cruz"), ordered=T)

Hst_d[Hst_d$Begin==10,]$End <- 12
plot_all(Hst_d)



# Bismark within PNG
# Assign group to group
assign("Manus_New_Ireland-West_New_Britain", c("Manus_New_Ireland", "West_New_Britain"))
assign("West_New_Britain-East_New_Britain", c("West_New_Britain", "East_New_Britain"))
assign("Manus_New_Ireland-East_New_Britain", c("Manus_New_Ireland","East_New_Britain"))


# Make a dataframe to run the functions
Hst_d <- tibble(Group=rep(c("Manus_New_Ireland-West_New_Britain","West_New_Britain-East_New_Britain","Manus_New_Ireland-East_New_Britain"),7),
                Begin=c(rep(1,3),rep(2,3),rep(3,3),rep(4,3),rep(5,3),rep(6,3),rep(10,3)),
                End=c(rep(5,3),rep(6,3),rep(7,3),rep(8,3),rep(9,3),rep(10,3),rep(Inf,3)),
                Hst_L_mean=rep(0,21),
                Hst_L_std=rep(0,21),
                Hst_N_mean=rep(0,21),
                Hst_N_std=rep(0,21),
)

# Run functions for each group and length range
for (i in 1:nrow(Hst_d)){
  Begin = Hst_d$Begin[i]
  End = Hst_d$End[i]
  Group = Hst_d$Group[i]
  Hst_mean_std_L <- Hst_mean_std(IBD, Info, min=Begin, max=End, all=D, within=eval(as.symbol(Group)))
  Hst_d$Hst_L_mean[i] = Hst_mean_std_L[1]
  Hst_d$Hst_L_std[i] = Hst_mean_std_L[2]
  Hst_mean_std_N <- Hst_mean_std(IBD, Info, Measure="N", min=Begin, max=End, all=D, within=eval(as.symbol(Group)))
  Hst_d$Hst_N_mean[i] = Hst_mean_std_N[1]
  Hst_d$Hst_N_std[i] = Hst_mean_std_N[2]
}

# Plot the result
Hst_d$Group <- factor(Hst_d$Group, levels=c("Manus_New_Ireland-West_New_Britain","West_New_Britain-East_New_Britain","Manus_New_Ireland-East_New_Britain"), ordered=T)

Hst_d[Hst_d$Begin==10,]$End <- 12
plot_all(Hst_d)


# Bismark within Bismark
# Make a dataframe to run the functions
Hst_d <- tibble(Group=rep(c("Manus_New_Ireland-West_New_Britain","West_New_Britain-East_New_Britain","Manus_New_Ireland-East_New_Britain"),7),
                Begin=c(rep(1,3),rep(2,3),rep(3,3),rep(4,3),rep(5,3),rep(6,3),rep(10,3)),
                End=c(rep(5,3),rep(6,3),rep(7,3),rep(8,3),rep(9,3),rep(10,3),rep(Inf,3)),
                Hst_L_mean=rep(0,21),
                Hst_L_std=rep(0,21),
                Hst_N_mean=rep(0,21),
                Hst_N_std=rep(0,21),
)

# Run functions for each group and length range
for (i in 1:nrow(Hst_d)){
  Begin = Hst_d$Begin[i]
  End = Hst_d$End[i]
  Group = Hst_d$Group[i]
  Hst_mean_std_L <- Hst_mean_std(IBD, Info, min=Begin, max=End, all=Bismark, within=eval(as.symbol(Group)))
  Hst_d$Hst_L_mean[i] = Hst_mean_std_L[1]
  Hst_d$Hst_L_std[i] = Hst_mean_std_L[2]
  Hst_mean_std_N <- Hst_mean_std(IBD, Info, Measure="N", min=Begin, max=End, all=Bismark, within=eval(as.symbol(Group)))
  Hst_d$Hst_N_mean[i] = Hst_mean_std_N[1]
  Hst_d$Hst_N_std[i] = Hst_mean_std_N[2]
}

# Plot the result
Hst_d$Group <- factor(Hst_d$Group, levels=c("Manus_New_Ireland-West_New_Britain","West_New_Britain-East_New_Britain","Manus_New_Ireland-East_New_Britain"), ordered=T)

Hst_d[Hst_d$Begin==10,]$End <- 12
plot_all(Hst_d)



# Kula/Half_Kula/Non_Kula within Oce
# Assign group to group
assign("Kula", Kula)
assign("Half_Kula", c(Kula, Half_Kula))
assign("Non_Kula", Massim)
assign("None", "")

# Make a dataframe to run the functions
Hst_d <- tibble(Group=rep(c("Kula","Half_Kula","Non_Kula"),7),
                Exclude=rep(c("None","Kula","Half_Kula"),7),
                Begin=c(rep(1,3),rep(2,3),rep(3,3),rep(4,3),rep(5,3),rep(6,3),rep(10,3)),
                End=c(rep(5,3),rep(6,3),rep(7,3),rep(8,3),rep(9,3),rep(10,3),rep(Inf,3)),
                Hst_L_mean=rep(0,21),
                Hst_L_std=rep(0,21),
                Hst_N_mean=rep(0,21),
                Hst_N_std=rep(0,21),
)

# Run functions for each group and length range
for (i in 1:nrow(Hst_d)){
  Begin = Hst_d$Begin[i]
  End = Hst_d$End[i]
  Group = Hst_d$Group[i]
  Exclude = Hst_d$Exclude[i]
  Hst_mean_std_L <- Hst_mean_std(IBD, Info, min=Begin, max=End, all=D, within=eval(as.symbol(Group)), within_exclude=eval(as.symbol(Exclude)))
  Hst_d$Hst_L_mean[i] = Hst_mean_std_L[1]
  Hst_d$Hst_L_std[i] = Hst_mean_std_L[2]
  Hst_mean_std_N <- Hst_mean_std(IBD, Info, Measure="N", min=Begin, max=End, all=D, within=eval(as.symbol(Group)), within_exclude=eval(as.symbol(Exclude)))
  Hst_d$Hst_N_mean[i] = Hst_mean_std_N[1]
  Hst_d$Hst_N_std[i] = Hst_mean_std_N[2]
}

# Plot the result
Hst_d$Group <- factor(Hst_d$Group, levels=c("Kula","Half_Kula","Non_Kula"), ordered=T)

Hst_d[Hst_d$Begin==10,]$End <- 12
plot_all(Hst_d)

Group_color = c("Kula"="#6A3884", "Half_Kula"="#21908C", "Non_Kula"="#C4B286")
Hst_d %>% ggplot() +
  geom_point(aes(x = End, y = Hst_L_mean, color=Group), size = 4) +
  geom_errorbar(aes(x = End, y = Hst_L_mean, ymin = Hst_L_mean - 3 * Hst_L_std, ymax = Hst_L_mean + 3 * Hst_L_std, color=Group), width=.5) +
  geom_smooth(aes(x = End, y = Hst_L_mean, color = Group), linetype="dotted",  se =F, size=0.75) +
  scale_x_continuous(breaks = c(5, 6, 7, 8, 9, 10, 12), 
                     labels = c("5","6","7","8","9","10","Over 10"),
                     sec.axis = dup_axis(name = "Lower bound of IBD block length (cM)",
                                         labels = c("1","2","3","4","5","6","10"))) +
  labs(x = "Upper bound of IBD block length (cM)", y = "Fst-like stats on IBD matrix", color = "Group") +
  scale_color_manual(values = Group_color) +
  #facet_wrap(.~Group, nrow=Nrow) +
  theme_bw() +
  theme(legend.text = element_text(size = 12), legend.title = element_text(size = 12)) +
  theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10)) +
  theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14)) +
  #theme(legend.position = c(0.75, 0.1)) +
  theme(legend.background = element_rect(color="black", 
                                         linetype="solid"))

# Kula/Half_Kula/None_Kula within Massim
# Make a dataframe to run the functions
Hst_d <- tibble(Group=rep(c("Kula","Half_Kula","Non_Kula"),7),
                Exclude=rep(c("None","Kula","Half_Kula"),7),
                Begin=c(rep(1,3),rep(2,3),rep(3,3),rep(4,3),rep(5,3),rep(6,3),rep(10,3)),
                End=c(rep(5,3),rep(6,3),rep(7,3),rep(8,3),rep(9,3),rep(10,3),rep(Inf,3)),
                Hst_L_mean=rep(0,21),
                Hst_L_std=rep(0,21),
                Hst_N_mean=rep(0,21),
                Hst_N_std=rep(0,21),
)

# Run functions for each group and length range
for (i in 1:nrow(Hst_d)){
  Begin = Hst_d$Begin[i]
  End = Hst_d$End[i]
  Group = Hst_d$Group[i]
  Exclude = Hst_d$Exclude[i]
  Hst_mean_std_L <- Hst_mean_std(IBD, Info, min=Begin, max=End, all=Massim, within=eval(as.symbol(Group)), within_exclude=eval(as.symbol(Exclude)))
  Hst_d$Hst_L_mean[i] = Hst_mean_std_L[1]
  Hst_d$Hst_L_std[i] = Hst_mean_std_L[2]
  Hst_mean_std_N <- Hst_mean_std(IBD, Info, Measure="N", min=Begin, max=End, all=Massim, within=eval(as.symbol(Group)), within_exclude=eval(as.symbol(Exclude)))
  Hst_d$Hst_N_mean[i] = Hst_mean_std_N[1]
  Hst_d$Hst_N_std[i] = Hst_mean_std_N[2]
}

# Plot the result
Hst_d$Group <- factor(Hst_d$Group, levels=c("Kula","Half_Kula","Non_Kula"), ordered=T)

Hst_d[Hst_d$Begin==10,]$End <- 12
plot_all(Hst_d, Nrow=1)

Group_color = c("Kula"="#6A3884", "Half_Kula"="#21908C", "Non_Kula"="#C4B286")
Hst_d %>% ggplot() +
  geom_point(aes(x = End, y = Hst_L_mean, color=Group), size = 4) +
  geom_errorbar(aes(x = End, y = Hst_L_mean, ymin = Hst_L_mean - 3 * Hst_L_std, ymax = Hst_L_mean + 3 * Hst_L_std, color=Group), width=.5) +
  geom_smooth(aes(x = End, y = Hst_L_mean, color = Group), linetype="dotted",  se =F, size=0.75) +
  scale_x_continuous(breaks = c(5, 6, 7, 8, 9, 10, 12), 
                     labels = c("5","6","7","8","9","10","Over 10"),
                     sec.axis = dup_axis(name = "Lower bound of IBD block length (cM)",
                                         labels = c("1","2","3","4","5","6","10"))) +
  scale_color_manual(values = Group_color) +
  labs(x = "Upper bound of IBD block length (cM)", y = "Fst-like stats on IBD matrix", color = "Group") +
  #facet_wrap(.~Group, nrow=Nrow) +
  theme_bw() +
  theme(legend.text = element_text(size = 12), legend.title = element_text(size = 12)) +
  theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10)) +
  theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14)) +
  #theme(legend.position = c(0.75, 0.1)) +
  theme(legend.background = element_rect(color="black", 
                                         linetype="solid"))
