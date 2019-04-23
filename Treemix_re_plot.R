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


# plot
treemix.bootstrap("/mnt/scratch/dang/Vietnam/TreeMix/outgroup.v2/boot2/HO.A.OG2.LLR.m4.re100",
                  out.file = "/mnt/scratch/dang/Vietnam/TreeMix/outgroup.v2/boot2/tmp",
                  "/mnt/scratch/dang/Vietnam/TreeMix/outgroup.v2/boot2/HO.A.OG2.LLR.m4.re100_outtree.newick",
                  pop.color.file="/mnt/scratch/dang/Vietnam/TreeMix/outgroup.v2/pop.color.LLR.list",
                  100, disp=0.0005)
plot_resid("/mnt/scratch/dang/Vietnam/TreeMix/outgroup.v2/boot2/HO.A.OG2.LLR.m4.re100",
           "/mnt/scratch/dang/Vietnam/TreeMix/outgroup.v2/pop.color.LLR.list")
treemix.drift("/mnt/scratch/dang/Vietnam/TreeMix/outgroup.v2/boot2/HO.A.OG2.LLR.m4.re100",
              pop.order.color.file="/mnt/scratch/dang/Vietnam/TreeMix/outgroup.v2/pop.color.LLR.list")


plot_tree("/mnt/scratch/dang/Vietnam/TreeMix/outgroup.v2/test.Try0",
          "/mnt/scratch/dang/Vietnam/TreeMix/outgroup.v2/pop.color.test.list")
