# title: "TreeMix bootstrap on Vietnam data"
# author: "Dang Liu 10.Apr.2019"

# Last updated: 23.Apr.2019

# Libraries
library(data.table)
library(tidyverse)

# Plotting source
source("/home/dang_liu/bin/treemix-1.13/src/plotting_funcs.R")
source("/home/dang_liu/bin/BITE/R/treemix_boostrap.md.R")
source("/home/dang_liu/bin/BITE/R/treemix_fit.R")
source("/home/dang_liu/bin/BITE/R/treemix_drift.R")
source("/home/dang_liu/bin/BITE/R/newick_split.R")

# Set up input files
treemixout <- "/mnt/scratch/dang/Vietnam/TreeMix/outgroup.v2/boot2/HO.A.OG2.LLR.m4.re100"
pop_list <- "/mnt/scratch/dang/Vietnam/TreeMix/outgroup.v2/pop.color.LLR.list"

  
# plot
treemix.bootstrap(treemixout,
                  out.file = paste(treemixout, "tmp", sep="."),
                  paste(treemixout, "_outtree.newick", sep="."),
                  pop.color.file=pop_list,
                  100, disp=0.0005)
plot_resid(treemixout,
           pop_list)
treemix.drift(treemixout,
              pop.order.color.file=pop_list)

# Extract single tree here
plot_tree("/mnt/scratch/dang/Vietnam/TreeMix/outgroup.v2/test.Try0",
          "/mnt/scratch/dang/Vietnam/TreeMix/outgroup.v2/pop.color.test.list")
