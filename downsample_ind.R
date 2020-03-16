set.seed(10)
G <- read_delim("/mnt/scratch/dang/New_Guinea/1000G/phase3_no1st.info", delim="\t", col_names=F)
d <- G %>% group_by(X3) %>% sample_n(size=10)
write_delim(d, "/mnt/scratch/dang/New_Guinea/1000G/1KG_size10.txt", delim="\t", col_names=F)
