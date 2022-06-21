# A script to manage info file
# Last_v20220502

library(tidyverse)

d <- read_csv("/home/dang_liu/Projects/SeaNomad/SeaNomads_meta.info.csv")
pop <- d %>% select(Pop) %>% distinct(.keep_all = T) %>% mutate(id=seq_along(Pop))
d2 <- d %>% left_join(pop) %>% mutate(PC_code=id) %>% select(-id)
d2 %>% write_csv("/home/dang_liu/Projects/SeaNomad/SeaNomads_meta.info.csv", quote="none")



d <- read_csv("/home/dang_liu/Projects/SeaNomad/SeaNomads_meta.info.csv")
imiss_m <- read_delim("/mnt/scratch/dang/SeaNomad/QC/SeaNomads_m.QCsnp.imiss.0.05", delim='\t', col_names=F) %>%
  rename(FID="X1",IID="X2")
kin <- read_delim("/mnt/scratch/dang/SeaNomad/QC/SeaNomads_m.QCsnp.king.cutoff.out.id", delim='\t', col_names=T) %>%
  rename(FID="#FID")
imiss_a <- read_table("/mnt/scratch/dang/SeaNomad/QC/SeaNomads_m_a.imiss.15000", col_names=T)
pc_outliers <- read_delim("/mnt/scratch/dang/SeaNomad/QC/PC.outliers", delim='\t', col_names=F) %>%
  rename(FID="X1",IID="X2")
d3 <- d %>% mutate(QC="PASS") %>% 
  mutate(QC=case_when(
    IID%in%c(imiss_m$IID, imiss_a$IID) ~ "imiss",
    IID%in%kin$IID ~ "kin",
    IID%in%pc_outliers$IID ~ "pc",
    TRUE ~ "PASS"))
d3 %>% filter(QC!="PASS") %>% count()
d3 %>% write_csv("/home/dang_liu/Projects/SeaNomad/SeaNomads_meta.info.csv", quote="none")