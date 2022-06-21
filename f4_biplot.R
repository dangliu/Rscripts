# Script to run f4 on Kula data to test AN/PNG ancestry
# Last updated: 16.Jan.2022

# Libraries
library(tidyverse)
library(admixr)
library(ggrepel)

# read data
data <- eigenstrat('/mnt/scratch/dang/Kula/f4/Kula.meta.filtered')
info <- read_csv('/r1/people/dang_liu/Projects/Kula/Kula.meta.info.csv')

# pop
ind <- read_ind(data)
table(ind$label)
pops <- unique(ind$label)
pops

# Groups
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


#f4_y <- f4(W = Massim, X = "EastAsia", Y = c(PNG_L,Bismark,Solomon), Z = "Southern_Highlands", data = data)
#f4_x <- f4(W = Massim, X = "Southern_Highlands", Y = "EastAsia_AN", Z = "EastAsia", data = data)

#save(f4_y, file="/mnt/scratch/dang/Kula/f4/f4_y.Rdata")
#save(f4_x, file="/mnt/scratch/dang/Kula/f4/f4_x.Rdata")
load("/mnt/scratch/dang/Kula/f4/f4_y.Rdata")
#load("/mnt/scratch/dang/Kula/f4/f4_x.Rdata")
load("/mnt/scratch/dang/Kula/f4/f4_x.Australia.Rdata")

d_x <- f4_x %>% select(W, f4, stderr, Zscore) %>% rename("f4_x"=f4, "stderr_x"=stderr, "Zscore_x"=Zscore)
d_y <- f4_y %>% select(W, Y, f4, stderr, Zscore) %>% rename("f4_y"=f4, "stderr_y"=stderr, "Zscore_y"=Zscore)
info2 <- info %>% select(Label, Region, Area) %>% distinct(.keep_all=T) %>% rename("W"=Label)
d <- d_x %>% left_join(d_y) %>% left_join(info2)

d[d$W%in%Collingwood_Bay,]$Region <- "Collingwood_Bay"
d[d$W%in%W_Mas,]$Region <- "Western_Massim"
d[d$W%in%N_Mas,]$Region <- "Northern_Massim"
d[d$W%in%S_Mas,]$Region <- "Southern_Massim"

Region_col = c("Collingwood_Bay"="#8DD3C7","Western_Massim"="#BEBADA","Northern_Massim"="#2e2967","Southern_Massim"="#b87a41")

d$Y <- factor(d$Y, levels=c(PNG_L,Bismark,Solomon), ordered=T)


d %>% 
  #mutate(Significant=as.factor(abs(Zscore_y)>3)) %>%
  ggplot() + 
  geom_smooth(aes(y=f4_y, x=f4_x), method='lm', formula=y~x, se=F, color="black", size=0.2) +
  #geom_point(aes(y=f4_y, x=f4_x, color=Region, pch=Significant)) + 
  geom_point(aes(y=f4_y, x=f4_x, color=Region), size=2.5, pch=1) +
  geom_errorbar(aes(y=f4_y, x=f4_x, ymin=f4_y-3*stderr_y, ymax=f4_y+3*stderr_y, color=Region)) +
  geom_errorbar(aes(y=f4_y, x=f4_x, xmin=f4_x-3*stderr_x, xmax=f4_x+3*stderr_x, color=Region)) +
  facet_wrap(~Y, nrow=3) +
  scale_color_manual(values = Region_col) +
  #scale_shape_manual(values = c("TRUE"=19, "FALSE"=4)) +
  theme(axis.line.x = element_line(color="black", size = 0.5, linetype = 1),
        axis.line.y = element_line(color="black", size = 0.5, linetype = 1)) +
  theme(panel.background = element_blank()) +
  theme(legend.position = c(0.9, 0.1)) +
  #labs(x="f4(X, Southern_Highland; East_Asia_AN, East_Asia)", y="f4(X, East_Asia; Y, Southern_Highland)", color="Region") +
  labs(x="f4(X, Southern_Highland; East_Asia_AN, Australia)", y="f4(X, East_Asia; Y, Southern_Highland)", color="Region") +
  theme(legend.text = element_text(size = 12), legend.title = element_text(size = 14)) +
  theme(axis.title.x = element_text(size = 14, face = "italic"), axis.title.y = element_text(size = 14, face = "italic")) + 
  theme(axis.text.x = element_text(size = 12, angle=45, vjust=0.5, hjust=0.5, margin = margin(t = 5)), axis.text.y = element_text(size = 12)) +
  theme(strip.text.x = element_text(size = 12), strip.text.y = element_text(size = 12))



# Focus on single Y
d %>% filter(Y=="Central") %>%
  #mutate(Significant=as.factor(abs(Zscore_y)>3)) %>%
  ggplot() + 
  geom_smooth(aes(y=f4_y, x=f4_x), method='lm', formula=y~x, se=F, color="black", size=0.2) +
  #geom_point(aes(y=f4_y, x=f4_x, color=Region, pch=Significant)) + 
  geom_point(aes(y=f4_y, x=f4_x, color=Region), size=2.5, pch=1) +
  geom_errorbar(aes(y=f4_y, x=f4_x, ymin=f4_y-3*stderr_y, ymax=f4_y+3*stderr_y, color=Region)) +
  geom_errorbar(aes(y=f4_y, x=f4_x, xmin=f4_x-3*stderr_x, xmax=f4_x+3*stderr_x, color=Region)) +
  #facet_wrap(~Y, nrow=3) +
  geom_text_repel(
    aes(y=f4_y, x=f4_x, label=W, color=Region), 
    size=4, 
    segment.alpha=0.5,
    show.legend=F) +
  scale_color_manual(values = Region_col) +
  #scale_shape_manual(values = c("TRUE"=19, "FALSE"=4)) +
  theme(axis.line.x = element_line(color="black", size = 0.5, linetype = 1),
        axis.line.y = element_line(color="black", size = 0.5, linetype = 1)) +
  theme(panel.background = element_blank()) +
  #theme(legend.position = c(0.9, 0.1)) +
  #labs(x="f4(X, Southern_Highland; East_Asia_AN, East_Asia)", y="f4(X, East_Asia; Central, Southern_Highland)", color="Region") +
  labs(x="f4(X, Southern_Highland; East_Asia_AN, Australia)", y="f4(X, East_Asia; Central, Southern_Highland)", color="Region") +
  theme(legend.text = element_text(size = 12), legend.title = element_text(size = 14)) +
  theme(axis.title.x = element_text(size = 14, face = "italic"), axis.title.y = element_text(size = 14, face = "italic")) + 
  theme(axis.text.x = element_text(size = 12, angle=45, vjust=0.5, hjust=0.5, margin = margin(t = 5)), axis.text.y = element_text(size = 12)) +
  theme(strip.text.x = element_text(size = 12), strip.text.y = element_text(size = 12))


# Test if Rossel has higher highlander ancestry
#r1 <- f4(W = "Rossel", X = Massim[!Massim%in%c("Rossel")], Y = PNG_H, Z = "Africa", data = data)
#save(r1, file="/mnt/scratch/dang/Kula/f4/f4_Rossel.Rdata")

load("/mnt/scratch/dang/Kula/f4/f4_Rossel.Rdata")

info2 <- info %>% select(Label, Region, Area) %>% distinct(.keep_all=T) %>% rename("X"=Label)
d <- r1 %>% left_join(info2)

d[d$X%in%Collingwood_Bay,]$Region <- "Collingwood_Bay"
d[d$X%in%W_Mas,]$Region <- "Western_Massim"
d[d$X%in%N_Mas,]$Region <- "Northern_Massim"
d[d$X%in%S_Mas,]$Region <- "Southern_Massim"

d$X <- factor(d$X, levels=rev(Massim), ordered=T)
d$Y <- factor(d$Y, levels=PNG_H, ordered=T)


Region_col = c("Collingwood_Bay"="#8DD3C7","Western_Massim"="#BEBADA","Northern_Massim"="#2e2967","Southern_Massim"="#b87a41")

d %>%
  mutate(Significant=as.factor(abs(Zscore)>3)) %>%
  ggplot(aes(y=f4, x=X, ymin=f4-3*stderr, ymax=f4+3*stderr, color=Region)) + 
  geom_hline(yintercept=0, color="grey") +
  geom_point(aes(pch=Significant), size=2) + 
  geom_errorbar() +
  facet_wrap(~Y, nrow=2) +
  coord_flip() +
  scale_color_manual(values = Region_col) +
  scale_shape_manual(values = c("TRUE"=19, "FALSE"=1)) +
  theme(axis.line.x = element_line(color="black", size = 0.5, linetype = 1),
        axis.line.y = element_line(color="black", size = 0.5, linetype = 1)) +
  theme(panel.background = element_blank()) +
  labs(y="f4(Rossel, X; Y, Africa)", x=NULL, color=NULL, pch="|Z| > 3") +
  theme(legend.text = element_text(size = 12), legend.title = element_text(size = 14)) +
  theme(axis.title.x = element_text(size = 14, face = "italic"), axis.title.y = element_text(size = 14)) + 
  theme(axis.text.x = element_text(size = 12, angle=45, vjust=0.5, hjust=0.5, margin = margin(t = 5)), axis.text.y = element_text(size = 12)) +
  theme(strip.text.x = element_text(size = 12), strip.text.y = element_text(size = 12))

# Test if Gawa has higher AN ancestry
#r2 <- f4(W = "Gawa", X = Massim[!Massim%in%c("Gawa")], Y = c("EastAsia_AN", Solomon), Z = "Africa", data = data)
#save(r2, file="/mnt/scratch/dang/Kula/f4/f4_Gawa.Rdata")

load("/mnt/scratch/dang/Kula/f4/f4_Gawa.Rdata")

info2 <- info %>% select(Label, Region, Area) %>% distinct(.keep_all=T) %>% rename("X"=Label)
d <- r2 %>% left_join(info2)

d[d$X%in%Collingwood_Bay,]$Region <- "Collingwood_Bay"
d[d$X%in%W_Mas,]$Region <- "Western_Massim"
d[d$X%in%N_Mas,]$Region <- "Northern_Massim"
d[d$X%in%S_Mas,]$Region <- "Southern_Massim"

d$X <- factor(d$X, levels=rev(Massim), ordered=T)
d$Y <- factor(d$Y, levels=c("EastAsia_AN", Solomon), ordered=T)


Region_col = c("Collingwood_Bay"="#8DD3C7","Western_Massim"="#BEBADA","Northern_Massim"="#2e2967","Southern_Massim"="#b87a41")

d %>%
  mutate(Significant=as.factor(abs(Zscore)>3)) %>%
  ggplot(aes(y=f4, x=X, ymin=f4-3*stderr, ymax=f4+3*stderr, color=Region)) + 
  geom_hline(yintercept=0, color="grey") +
  geom_point(aes(pch=Significant), size=2) + 
  geom_errorbar() +
  facet_wrap(~Y, nrow=2) +
  coord_flip() +
  scale_color_manual(values = Region_col) +
  scale_shape_manual(values = c("TRUE"=19, "FALSE"=1)) +
  theme(axis.line.x = element_line(color="black", size = 0.5, linetype = 1),
        axis.line.y = element_line(color="black", size = 0.5, linetype = 1)) +
  theme(panel.background = element_blank()) +
  labs(y="f4(Gawa, X; Y, Africa)", x=NULL, color=NULL, pch="|Z| > 3") +
  theme(legend.text = element_text(size = 12), legend.title = element_text(size = 14)) +
  theme(axis.title.x = element_text(size = 14, face = "italic"), axis.title.y = element_text(size = 14)) + 
  theme(axis.text.x = element_text(size = 12, angle=45, vjust=0.5, hjust=0.5, margin = margin(t = 5)), axis.text.y = element_text(size = 12)) +
  theme(strip.text.x = element_text(size = 12), strip.text.y = element_text(size = 12))


## Update 27.Aug.2021
# Chimbu vs. S_highland <-> AN
#f4_y_1 <- f4(W = c(Massim, PNG_L), X = "EastAsia_AN", Y = "Chimbu", Z = "Southern_Highlands", data = data)
#f4_y_2 <- f4(W = c(Bismark, Solomon), X = "EastAsia_AN", Y = "Chimbu", Z = "Southern_Highlands", data = data)
#f4_y <- f4_y_1 %>% bind_rows(f4_y_2)
#save(f4_y, file="/mnt/scratch/dang/Kula/f4/f4.X.AN.Chimbu.S_Highland.Rdata")
load("/mnt/scratch/dang/Kula/f4/f4.X.AN.Chimbu.S_Highland.Rdata")
# Bougainville vs. S_highland <-> AN
#f4_y_1 <- f4(W = c(Massim, PNG_L), X = "EastAsia_AN", Y = "Bougainville", Z = "Southern_Highlands", data = data)
#f4_y_2 <- f4(W = c(Bismark, Solomon), X = "EastAsia_AN", Y = "Bougainville", Z = "Southern_Highlands", data = data)
#f4_y <- f4_y_1 %>% bind_rows(f4_y_2)
#save(f4_y, file="/mnt/scratch/dang/Kula/f4/f4.X.AN.BGV.S_Highland.Rdata")
#load("/mnt/scratch/dang/Kula/f4/f4.X.AN.BGV.S_Highland.Rdata")
# Rossel vs. S_highland <-> AN
#f4_y_1 <- f4(W = c(Massim, PNG_L), X = "EastAsia_AN", Y = "Rossel", Z = "Southern_Highlands", data = data)
#f4_y_2 <- f4(W = c(Bismark, Solomon), X = "EastAsia_AN", Y = "Rossel", Z = "Southern_Highlands", data = data)
#f4_y <- f4_y_1 %>% bind_rows(f4_y_2)
#save(f4_y, file="/mnt/scratch/dang/Kula/f4/f4.X.AN.Rossel.S_Highland.Rdata")
#load("/mnt/scratch/dang/Kula/f4/f4.X.AN.Rossel.S_Highland.Rdata")
# EA_AN vs. RO_AN <-> AN
#f4_y_1 <- f4(W = c(Massim, PNG_L), X = "Southern_Highlands", Y = "EastAsia_AN", Z = "Bellona_Rennell", data = data)
#f4_y_2 <- f4(W = c(Bismark, Solomon), X = "Southern_Highlands", Y = "EastAsia_AN", Z = "Bellona_Rennell", data = data)
#f4_y <- f4_y_1 %>% bind_rows(f4_y_2)
#save(f4_y, file="/mnt/scratch/dang/Kula/f4/f4.X.S_Highland.AN.BelRen.Rdata")
#load("/mnt/scratch/dang/Kula/f4/f4.X.S_Highland.AN.BelRen.Rdata")
#f4_y <- f4_y %>% filter(W!="Tikopia")


# East Asian
#f4_x_1 <- f4(W = c(Massim, PNG_L), X = "Southern_Highlands", Y = "EastAsia_AN", Z = "Australia", data = data)
#f4_x_2 <- f4(W = c(Bismark, Solomon), X = "Southern_Highlands", Y = "EastAsia_AN", Z = "Australia", data = data)
#f4_x <- f4_x_1 %>% bind_rows(f4_x_2)
#save(f4_x, file="/mnt/scratch/dang/Kula/f4/f4.X.S_Highland.AN.Australia.Rdata")
load("/mnt/scratch/dang/Kula/f4/f4.X.S_Highland.AN.Australia.Rdata")
# East Asian AN
#f4_x_1 <- f4(W = c(Massim, PNG_L), X = "Southern_Highlands", Y = "EastAsia_AN", Z = "EastAsia", data = data)
#f4_x_2 <- f4(W = c(Bismark, Solomon), X = "Southern_Highlands", Y = "EastAsia_AN", Z = "EastAsia", data = data)
#f4_x <- f4_x_1 %>% bind_rows(f4_x_2)
#save(f4_x, file="/mnt/scratch/dang/Kula/f4/f4.X.S_Highland.AN.EastAsia.Rdata")
#load("/mnt/scratch/dang/Kula/f4/f4.X.S_Highland.AN.EastAsia.Rdata")



d_x <- f4_x %>% select(W, f4, stderr, Zscore) %>% rename("f4_x"=f4, "stderr_x"=stderr, "Zscore_x"=Zscore)
d_y <- f4_y %>% select(W, Y, f4, stderr, Zscore) %>% rename("f4_y"=f4, "stderr_y"=stderr, "Zscore_y"=Zscore)
info2 <- info %>% select(Label, Region, Area) %>% distinct(.keep_all=T) %>% rename("W"=Label)
d <- d_x %>% left_join(d_y) %>% left_join(info2)

d$Region <- "Others"
d[d$W%in%Collingwood_Bay,]$Region <- "Collingwood_Bay"
d[d$W%in%W_Mas,]$Region <- "Western_Massim"
d[d$W%in%N_Mas,]$Region <- "Northern_Massim"
d[d$W%in%S_Mas,]$Region <- "Southern_Massim"

Region_col = c("Collingwood_Bay"="#8DD3C7","Western_Massim"="#BEBADA","Northern_Massim"="#2e2967","Southern_Massim"="#b87a41","Others"="#999999")

d$Y <- factor(d$Y, levels=c(PNG_L,Bismark,Solomon), ordered=T)

d$Region <- factor(d$Region, levels=c("Collingwood_Bay", "Western_Massim", "Northern_Massim", "Southern_Massim", "Others"), ordered=T)

d %>% 
  #mutate(Significant=as.factor(abs(Zscore_y)>3)) %>%
  ggplot() + 
  geom_smooth(aes(y=f4_y, x=f4_x), method='lm', formula=y~x, se=F, color="black", size=0.2) +
  #geom_point(aes(y=f4_y, x=f4_x, color=Region, pch=Significant)) + 
  geom_point(aes(y=f4_y, x=f4_x, color=Region), size=2.5, pch=1) +
  geom_errorbar(aes(y=f4_y, x=f4_x, ymin=f4_y-3*stderr_y, ymax=f4_y+3*stderr_y, color=Region)) +
  geom_errorbar(aes(y=f4_y, x=f4_x, xmin=f4_x-3*stderr_x, xmax=f4_x+3*stderr_x, color=Region)) +
  geom_text_repel(aes(y=f4_y, x=f4_x,label=W,color=Region), size=3, segment.alpha=0.5, show.legend=F) +
  #facet_wrap(~Y, nrow=3) +
  scale_color_manual(values = Region_col) +
  #scale_shape_manual(values = c("TRUE"=19, "FALSE"=4)) +
  theme(axis.line.x = element_line(color="black", size = 0.5, linetype = 1),
        axis.line.y = element_line(color="black", size = 0.5, linetype = 1)) +
  theme(panel.background = element_blank()) +
  #xlim(min(c(d$f4_x, d$f4_y)),max(c(d$f4_x, d$f4_y))) +
  #ylim(min(c(d$f4_x, d$f4_y)),max(c(d$f4_x, d$f4_y))) +
  #theme(legend.position = c(0.9, 0.1)) +
  labs(x="f4(X, Southern_Highland; EastAsia_AN, Australia)", y="f4(X, EastAsia_AN; Chimbu, Southern_Highland)", color="Massim region") +
  #labs(x="f4(X, Southern_Highland; EastAsia_AN, EastAsia)", y="f4(X, EastAsia_AN; Chimbu, Southern_Highland)", color="Massim region") +
  #labs(x="f4(X, Southern_Highland; EastAsia_AN, Australia)", y="f4(X, EastAsia_AN; Bougainville, Southern_Highland)", color="Massim region") +
  #labs(x="f4(X, Southern_Highland; EastAsia_AN, EastAsia)", y="f4(X, EastAsia_AN; Bougainville, Southern_Highland)", color="Massim region") +
  #labs(x="f4(X, Southern_Highland; EastAsia_AN, Australia)", y="f4(X, EastAsia_AN; Rossel, Southern_Highland)", color="Massim region") +
  #labs(x="f4(X, Southern_Highland; EastAsia_AN, EastAsia)", y="f4(X, EastAsia_AN; Rossel, Southern_Highland)", color="Massim region") +
  #labs(x="f4(X, Southern_Highland; EastAsia_AN, Australia)", y="f4(X, Southern_Highland; EastAsia_AN, Bellona_Rennell)", color="Massim region") +
  #labs(x="f4(X, Southern_Highland; EastAsia_AN, EastAsia)", y="f4(X, Southern_Highland; EastAsia_AN, Bellona_Rennell)", color="Massim region") +
  theme(legend.text = element_text(size = 12), legend.title = element_text(size = 14)) +
  theme(axis.title.x = element_text(size = 14, face = "italic"), axis.title.y = element_text(size = 14, face = "italic")) + 
  theme(axis.text.x = element_text(size = 12, angle=45, vjust=0.5, hjust=0.5, margin = margin(t = 5)), axis.text.y = element_text(size = 12)) +
  theme(strip.text.x = element_text(size = 12), strip.text.y = element_text(size = 12))

