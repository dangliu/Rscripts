## Fitting admixture graphs for the Kula smaples using admixtools2

# author: "Dang Liu 14.Apr.2021"

# Last updated: 01.Sep.2021




library(magrittr)
library(tidyverse)
library(admixtools)


# Make a for loop find_graphs function here
loop_find_graphs <- function(f2_blocks, numadmix, outpop, stop_gen = 100, runs = 10, admix_constraints = NULL){
  total_runs = vector(mode = "list", length = 6)
  for (n in 1:runs){
    set.seed(n)
    opt_results = find_graphs(f2_blocks, numadmix = numadmix, outpop = outpop, stop_gen = stop_gen, admix_constraints = admix_constraints)
    total_runs = rbind(total_runs, opt_results)
  }
  return(total_runs)
}




prefix = '/mnt/scratch/dang/Kula/f4/Kula.meta.filtered'
my_f2_dir = '/mnt/scratch/dang/Kula/qpgraph/admixtools2/f2'
# This just needs to be done once
# It is done!
#extract_f2(prefix, my_f2_dir)

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

# get f2 blocks for certain pops
mypops = c(Massim, "Africa", "EastAsia_AN", "Southern_Highlands", "Central", "East_New_Britain", "Vella_Lavella")
f2_blocks = f2_from_precomp(my_f2_dir, pops = mypops)


#dim(f2_blocks)
#count_snps(f2_blocks)


# Try some f4 here
#f4(f2_blocks, "Rossel", "Laughlan", "Southern_Highlands", "Africa")
#f4(f2_blocks, "Rossel", "Laughlan", "EastAsia_AN", "Africa")


# qpgraph
# Simple test
#example_graph = '/mnt/scratch/dang/Kula/qpgraph/N_Massim/qp1.graph'
#qpg_results = qpgraph(f2_blocks, example_graph)
#qpg_results$score
#qpg_results$f3
#plot_graph(qpg_results$edges)
#qpg_results$edges %>% plotly_graph(highlight_unidentifiable = TRUE)


# find graphs
# C Bay
# AdmixtureBayes graph
ab_graph = '/mnt/scratch/dang/Kula/qpgraph/C_Bay/no_ENB/qp3.graph'
qpg_results = qpgraph(f2_blocks, ab_graph, return_f4=T)
qpg_results$score
plot_graph(qpg_results$edges)
qpg_results$edges %>% plotly_graph(highlight_unidentifiable = TRUE)

# m=5
#set.seed(1)
mypops = c("Africa", "EastAsia_AN", "Southern_Highlands", "Central", "Vella_Lavella", "Wanigela", "Airara")
example_f2_blocks = f2_from_precomp(my_f2_dir, pops = mypops)
#opt_results = find_graphs(example_f2_blocks, numadmix = 5, outpop = 'Africa', stop_gen = 100)
opt_results_10runs = loop_find_graphs(example_f2_blocks, numadmix = 5, outpop = 'Africa', stop_gen = 100, runs = 10)
#winner = opt_results %>% slice_min(score, with_ties = FALSE)
winner = opt_results_10runs %>% slice_min(score, with_ties = FALSE)
winner$score[[1]]
plot_graph(winner$edges[[1]])
winner$edges[[1]] %>% plotly_graph(highlight_unidentifiable = TRUE)

# Compare the two graphs
#opt_winner_results = qpgraph(f2_blocks, winner$edges[[1]])
#plot_comparison(qpg_results, opt_winner_results)
#fits = qpgraph_resample_multi(f2_blocks, list(ab_graph, winner$edges[[1]]), nboot = 100)
#test = compare_fits(fits[[1]]$score_test, fits[[2]]$score_test)

# m=4
set.seed(1)
opt_results = find_graphs(example_f2_blocks, numadmix = 4, outpop = 'Africa', stop_gen = 100)
winner = opt_results %>% slice_min(score, with_ties = FALSE)
winner$score[[1]]
plot_graph(winner$edges[[1]])

# Set constrains for highlanders and EastAsian to be unadmixed
set.seed(1)
constraints = tribble(
  ~pop, ~min, ~max,
  'Southern_Highlands', NA, 0,
  'EastAsia_AN', NA, 0)
opt_results = find_graphs(example_f2_blocks, numadmix = 5, outpop = 'Africa', stop_gen = 100, admix_constraints = constraints)
winner = opt_results %>% slice_min(score, with_ties = FALSE)
winner$score[[1]]
plot_graph(winner$edges[[1]])

save(opt_results_10runs, file="/mnt/scratch/dang/Kula/qpgraph/admixtools2/qpgraph/C_Bay.10runs.Rdata")
#load("/mnt/scratch/dang/Kula/qpgraph/admixtools2/qpgraph/C_Bay.10runs.Rdata")

# W Massim
# AdmixtureBayes graph
ab_graph = '/mnt/scratch/dang/Kula/qpgraph/W_Massim/no_ENB/qp7.graph'
qpg_results = qpgraph(f2_blocks, ab_graph, return_f4=T)
qpg_results$score
plot_graph(qpg_results$edges)
qpg_results$edges %>% plotly_graph(highlight_unidentifiable = TRUE)

# m=4
#set.seed(1)
mypops = c("Africa", "EastAsia_AN", "Southern_Highlands", "Central", "Vella_Lavella", "Fergusson", "Mainland_Eastern_Tip")
example_f2_blocks = f2_from_precomp(my_f2_dir, pops = mypops)
#opt_results = find_graphs(example_f2_blocks, numadmix = 4, outpop = 'Africa', stop_gen = 100)
opt_results_10runs = loop_find_graphs(example_f2_blocks, numadmix = 4, outpop = 'Africa', stop_gen = 100, runs = 10)
#winner = opt_results %>% slice_min(score, with_ties = FALSE)
winner = opt_results_10runs %>% slice_min(score, with_ties = FALSE)
winner$score[[1]]
plot_graph(winner$edges[[1]])
winner$edges[[1]] %>% plotly_graph(highlight_unidentifiable = TRUE)

# Compare the two graphs
#opt_winner_results = qpgraph(f2_blocks, winner$edges[[1]])
#plot_comparison(qpg_results, opt_winner_results)
#fits = qpgraph_resample_multi(f2_blocks, list(ab_graph, winner$edges[[1]]), nboot = 100)
#test = compare_fits(fits[[1]]$score_test, fits[[2]]$score_test)

# m=3
set.seed(1)
opt_results = find_graphs(example_f2_blocks, numadmix = 3, outpop = 'Africa', stop_gen = 100)
winner = opt_results %>% slice_min(score, with_ties = FALSE)
winner$score[[1]]
plot_graph(winner$edges[[1]])

# Set constrains for highlanders and EastAsian to be unadmixed
set.seed(1)
constraints = tribble(
  ~pop, ~min, ~max,
  'Southern_Highlands', NA, 0,
  'EastAsia_AN', NA, 0)
opt_results = find_graphs(example_f2_blocks, numadmix = 5, outpop = 'Africa', stop_gen = 100, admix_constraints = constraints)
winner = opt_results %>% slice_min(score, with_ties = FALSE)
winner$score[[1]]
plot_graph(winner$edges[[1]])

save(opt_results_10runs, file="/mnt/scratch/dang/Kula/qpgraph/admixtools2/qpgraph/W_Massim.10runs.Rdata")
#load("/mnt/scratch/dang/Kula/qpgraph/admixtools2/qpgraph/W_Massim.10runs.Rdata")


# N Massim
# AdmixtureBayes graph
ab_graph = '/mnt/scratch/dang/Kula/qpgraph/N_Massim/no_ENB/qp7.graph'
qpg_results = qpgraph(f2_blocks, ab_graph, return_f4=T)
qpg_results$score
plot_graph(qpg_results$edges)
qpg_results$edges %>% plotly_graph(highlight_unidentifiable = TRUE)

# m=6
set.seed(1)
mypops = c("Africa", "EastAsia_AN", "Southern_Highlands", "Central", "Vella_Lavella", "Trobriand", "Laughlan")
example_f2_blocks = f2_from_precomp(my_f2_dir, pops = mypops)
#opt_results = find_graphs(example_f2_blocks, numadmix = 6, outpop = 'Africa', stop_gen = 100)
opt_results_10runs = loop_find_graphs(example_f2_blocks, numadmix = 6, outpop = 'Africa', stop_gen = 100, runs = 10)
#winner = opt_results %>% slice_min(score, with_ties = FALSE)
winner = opt_results_10runs %>% slice_min(score, with_ties = FALSE)
winner$score[[1]]
plot_graph(winner$edges[[1]])
winner$edges[[1]] %>% plotly_graph(highlight_unidentifiable = TRUE)

# Compare the two graphs
#opt_winner_results = qpgraph(f2_blocks, winner$edges[[1]])
#plot_comparison(qpg_results, opt_winner_results)
#fits = qpgraph_resample_multi(f2_blocks, list(ab_graph, winner$edges[[1]]), nboot = 100)
#test = compare_fits(fits[[1]]$score_test, fits[[2]]$score_test)

# m=5
set.seed(1)
opt_results = find_graphs(example_f2_blocks, numadmix = 5, outpop = 'Africa', stop_gen = 100)
winner = opt_results %>% slice_min(score, with_ties = FALSE)
winner$score[[1]]
plot_graph(winner$edges[[1]])

# Set constrains for highlanders and EastAsian to be unadmixed
set.seed(1)
constraints = tribble(
  ~pop, ~min, ~max,
  'Southern_Highlands', NA, 0,
  'EastAsia_AN', NA, 0)
opt_results = find_graphs(example_f2_blocks, numadmix = 6, outpop = 'Africa', stop_gen = 100, admix_constraints = constraints)
winner = opt_results %>% slice_min(score, with_ties = FALSE)
winner$score[[1]]
plot_graph(winner$edges[[1]])

save(opt_results_10runs, file="/mnt/scratch/dang/Kula/qpgraph/admixtools2/qpgraph/N_Massim.10runs.Rdata")
#load("/mnt/scratch/dang/Kula/qpgraph/admixtools2/qpgraph/N_Massim.10runs.Rdata")

# S Massim
# AdmixtureBayes graph
ab_graph = '/mnt/scratch/dang/Kula/qpgraph/S_Massim/no_ENB/qp3.graph'
qpg_results = qpgraph(f2_blocks, ab_graph, return_f4=T)
qpg_results$score
plot_graph(qpg_results$edges)
qpg_results$edges %>% plotly_graph(highlight_unidentifiable = TRUE)

# m=5
set.seed(1)
mypops = c("Africa", "EastAsia_AN", "Southern_Highlands", "Central", "Vella_Lavella", "Misima", "Rossel")
example_f2_blocks = f2_from_precomp(my_f2_dir, pops = mypops)
#opt_results = find_graphs(example_f2_blocks, numadmix = 5, outpop = 'Africa', stop_gen = 100)
opt_results_10runs = loop_find_graphs(example_f2_blocks, numadmix = 5, outpop = 'Africa', stop_gen = 100, runs = 10)
#winner = opt_results %>% slice_min(score, with_ties = FALSE)
winner = opt_results_10runs %>% slice_min(score, with_ties = FALSE)
winner$score[[1]]
plot_graph(winner$edges[[1]])
winner$edges[[1]] %>% plotly_graph(highlight_unidentifiable = TRUE)

# Compare the two graphs
#opt_winner_results = qpgraph(f2_blocks, winner$edges[[1]])
#plot_comparison(qpg_results, opt_winner_results)
#fits = qpgraph_resample_multi(f2_blocks, list(ab_graph, winner$edges[[1]]), nboot = 100)
#test = compare_fits(fits[[1]]$score_test, fits[[2]]$score_test)

# m=4
set.seed(1)
opt_results = find_graphs(example_f2_blocks, numadmix = 4, outpop = 'Africa', stop_gen = 100)
winner = opt_results %>% slice_min(score, with_ties = FALSE)
winner$score[[1]]
plot_graph(winner$edges[[1]])

# Set constrains for highlanders and EastAsian to be unadmixed
set.seed(1)
constraints = tribble(
  ~pop, ~min, ~max,
  'Southern_Highlands', NA, 0,
  'EastAsia_AN', NA, 0)
opt_results_10runs = find_graphs(example_f2_blocks, numadmix = 5, outpop = 'Africa', stop_gen = 100, admix_constraints = constraints)
winner = opt_results_10runs %>% slice_min(score, with_ties = FALSE)
winner$score[[1]]
plot_graph(winner$edges[[1]])


save(opt_results_10runs, file="/mnt/scratch/dang/Kula/qpgraph/admixtools2/qpgraph/S_Massim.10runs.Rdata")
#load("/mnt/scratch/dang/Kula/qpgraph/admixtools2/qpgraph/S_Massim.10runs.Rdata")



# Some trials on setting up with an initial graph from AB and some constrains
initgraph = edges_to_igraph(qpg_results$edges)
constraints = tribble(
  ~pop, ~min, ~max,
  'Southern_Highlands', NA, 0,
  'EastAsia_AN', NA, 0)
opt_results = find_graphs(example_f2_blocks, initgraph = initgraph, stop_gen = 100, admix_constraints = constraints)



# Run 10 times with one migration less and constraints for no admixtures on EastAsia_AN and Southern_Highlands and compare with the AB graph
constraints = tribble(
  ~pop, ~min, ~max,
  'Southern_Highlands', NA, 0,
  'EastAsia_AN', NA, 0)
opt_results_10runs = loop_find_graphs(example_f2_blocks, numadmix = 4, outpop = 'Africa', stop_gen = 100, runs = 10, admix_constraints = constraints)
#winner = opt_results %>% slice_min(score, with_ties = FALSE)
winner = opt_results_10runs %>% slice_min(score, with_ties = FALSE)
winner$score[[1]]
plot_graph(winner$edges[[1]])
opt_winner_results = qpgraph(f2_blocks, winner$edges[[1]], return_f4 = T)
plot_comparison(qpg_results, opt_winner_results)
fits = qpgraph_resample_multi(f2_blocks, list(ab_graph, winner$edges[[1]]), nboot = 100)
compare_fits(fits[[1]]$score_test, fits[[2]]$score_test)

save(opt_results_10runs, file="/mnt/scratch/dang/Kula/qpgraph/admixtools2/qpgraph/S_Massim.10runs.m4.constraints.Rdata")
load("/mnt/scratch/dang/Kula/qpgraph/admixtools2/qpgraph/S_Massim.10runs.m4.constraints.Rdata")




###Test for PNG ancestry difference for Massim groups
mypops = c("Africa", "EastAsia_AN", "Southern_Highlands", "Misima", "Rossel", "Fergusson", "Trobriand", "Wanigela")
example_f2_blocks = f2_from_precomp(my_f2_dir, pops = mypops)
constraints = tribble(
  ~pop, ~min, ~max,
  'Southern_Highlands', NA, 0,
  'EastAsia_AN', NA, 0)

#m1
#opt_results_10runs = loop_find_graphs(example_f2_blocks, numadmix = 1, outpop = 'Africa', stop_gen = 100, runs = 10, admix_constraints = constraints)
#save(opt_results_10runs, file="/mnt/scratch/dang/Kula/qpgraph/admixtools2/qpgraph/All.10runs.m1.constraints.Rdata")
load("/mnt/scratch/dang/Kula/qpgraph/admixtools2/qpgraph/All.10runs.m1.constraints.Rdata")
winner = opt_results_10runs %>% slice_min(score, with_ties = FALSE)
winner$score[[1]]
plot_graph(winner$edges[[1]])
winner$edges[[1]] %>% plotly_graph(highlight_unidentifiable = TRUE)

#m2
#opt_results_10runs = loop_find_graphs(example_f2_blocks, numadmix = 2, outpop = 'Africa', stop_gen = 100, runs = 10, admix_constraints = constraints)
#save(opt_results_10runs, file="/mnt/scratch/dang/Kula/qpgraph/admixtools2/qpgraph/All.10runs.m2.constraints.Rdata")
load("/mnt/scratch/dang/Kula/qpgraph/admixtools2/qpgraph/All.10runs.m2.constraints.Rdata")
winner = opt_results_10runs %>% slice_min(score, with_ties = FALSE)
winner$score[[1]]
plot_graph(winner$edges[[1]])
winner$edges[[1]] %>% plotly_graph(highlight_unidentifiable = TRUE)

#m3
#opt_results_10runs = loop_find_graphs(example_f2_blocks, numadmix = 3, outpop = 'Africa', stop_gen = 100, runs = 10, admix_constraints = constraints)
#save(opt_results_10runs, file="/mnt/scratch/dang/Kula/qpgraph/admixtools2/qpgraph/All.10runs.m3.constraints.Rdata")
load("/mnt/scratch/dang/Kula/qpgraph/admixtools2/qpgraph/All.10runs.m3.constraints.Rdata")
winner = opt_results_10runs %>% slice_min(score, with_ties = FALSE)
winner$score[[1]]
plot_graph(winner$edges[[1]])
winner$edges[[1]] %>% plotly_graph(highlight_unidentifiable = TRUE)

#m4
#opt_results_10runs = loop_find_graphs(example_f2_blocks, numadmix = 4, outpop = 'Africa', stop_gen = 100, runs = 10, admix_constraints = constraints)
#save(opt_results_10runs, file="/mnt/scratch/dang/Kula/qpgraph/admixtools2/qpgraph/All.10runs.m4.constraints.Rdata")
load("/mnt/scratch/dang/Kula/qpgraph/admixtools2/qpgraph/All.10runs.m4.constraints.Rdata")
winner = opt_results_10runs %>% slice_min(score, with_ties = FALSE)
winner$score[[1]]
plot_graph(winner$edges[[1]])
winner$edges[[1]] %>% plotly_graph(highlight_unidentifiable = TRUE)

#m5
#opt_results_10runs = loop_find_graphs(example_f2_blocks, numadmix = 5, outpop = 'Africa', stop_gen = 100, runs = 10, admix_constraints = constraints)
#save(opt_results_10runs, file="/mnt/scratch/dang/Kula/qpgraph/admixtools2/qpgraph/All.10runs.m5.constraints.Rdata")
load("/mnt/scratch/dang/Kula/qpgraph/admixtools2/qpgraph/All.10runs.m5.constraints.Rdata")
winner = opt_results_10runs %>% slice_min(score, with_ties = FALSE)
winner$score[[1]]
plot_graph(winner$edges[[1]])
winner$edges[[1]] %>% plotly_graph(highlight_unidentifiable = TRUE)

#m6
#opt_results_10runs = loop_find_graphs(example_f2_blocks, numadmix = 6, outpop = 'Africa', stop_gen = 100, runs = 10, admix_constraints = constraints)
#save(opt_results_10runs, file="/mnt/scratch/dang/Kula/qpgraph/admixtools2/qpgraph/All.10runs.m6.constraints.Rdata")
load("/mnt/scratch/dang/Kula/qpgraph/admixtools2/qpgraph/All.10runs.m6.constraints.Rdata")
winner = opt_results_10runs %>% slice_min(score, with_ties = FALSE)
winner$score[[1]]
plot_graph(winner$edges[[1]])
winner$edges[[1]] %>% plotly_graph(highlight_unidentifiable = TRUE)

#m7
#opt_results_10runs = loop_find_graphs(example_f2_blocks, numadmix = 7, outpop = 'Africa', stop_gen = 100, runs = 10, admix_constraints = constraints)
#save(opt_results_10runs, file="/mnt/scratch/dang/Kula/qpgraph/admixtools2/qpgraph/All.10runs.m7.constraints.Rdata")
load("/mnt/scratch/dang/Kula/qpgraph/admixtools2/qpgraph/All.10runs.m7.constraints.Rdata")
winner = opt_results_10runs %>% slice_min(score, with_ties = FALSE)
winner$score[[1]]
plot_graph(winner$edges[[1]])
winner$edges[[1]] %>% plotly_graph(highlight_unidentifiable = TRUE)