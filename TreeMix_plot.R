# title: "TreeMix on Vietnam data"
# author: "Dang Liu 04.Apr.2019"

# Last updated: 06.Apr.2019

# Plotting source
source("/home/dang_liu/bin/treemix-1.13/src/plotting_funcs.R")

## Only modern samples
# No migration events
plot_tree("/mnt/scratch/dang/Vietnam/TreeMix/Only_modern/HO.O.Try8")
plot_resid("/mnt/scratch/dang/Vietnam/TreeMix/Only_modern/HO.O.Try8","/mnt/scratch/dang/Vietnam/TreeMix/Only_modern/pop.list")
# With 1 migration event
plot_tree("/mnt/scratch/dang/Vietnam/TreeMix/Only_modern/HO.O.Mig1.Try9")
plot_resid("/mnt/scratch/dang/Vietnam/TreeMix/Only_modern/HO.O.Mig1.Try9","/mnt/scratch/dang/Vietnam/TreeMix/Only_modern/pop.list")
# With 2 migration event
plot_tree("/mnt/scratch/dang/Vietnam/TreeMix/Only_modern/HO.O.Mig2.Try4")
plot_resid("/mnt/scratch/dang/Vietnam/TreeMix/Only_modern/HO.O.Mig2.Try4","/mnt/scratch/dang/Vietnam/TreeMix/Only_modern/pop.list")
# With 3 migration event
plot_tree("/mnt/scratch/dang/Vietnam/TreeMix/Only_modern/HO.O.Mig3.Try6")
plot_resid("/mnt/scratch/dang/Vietnam/TreeMix/Only_modern/HO.O.Mig3.Try6","/mnt/scratch/dang/Vietnam/TreeMix/Only_modern/pop.list")
# With 4 migration event
plot_tree("/mnt/scratch/dang/Vietnam/TreeMix/Only_modern/HO.O.Mig4.Try2")
plot_resid("/mnt/scratch/dang/Vietnam/TreeMix/Only_modern/HO.O.Mig4.Try2","/mnt/scratch/dang/Vietnam/TreeMix/Only_modern/pop.list")
# With 5 migration event
plot_tree("/mnt/scratch/dang/Vietnam/TreeMix/Only_modern/HO.O.Mig5.Try5")
plot_resid("/mnt/scratch/dang/Vietnam/TreeMix/Only_modern/HO.O.Mig5.Try5","/mnt/scratch/dang/Vietnam/TreeMix/Only_modern/pop.list")


## With ancient samples
# No migration events
plot_tree("/mnt/scratch/dang/Vietnam/TreeMix/HO.A.O.Try19", "/mnt/scratch/dang/Vietnam/TreeMix/pop.color.list")
plot_resid("/mnt/scratch/dang/Vietnam/TreeMix/HO.A.O.Try19","/mnt/scratch/dang/Vietnam/TreeMix/pop.color.list")
# With 1 migration event
plot_tree("/mnt/scratch/dang/Vietnam/TreeMix/HO.A.O.Mig1.Try6", "/mnt/scratch/dang/Vietnam/TreeMix/pop.color.list")
plot_resid("/mnt/scratch/dang/Vietnam/TreeMix/HO.A.O.Mig1.Try6", "/mnt/scratch/dang/Vietnam/TreeMix/pop.color.list")
# With 2 migration event
plot_tree("/mnt/scratch/dang/Vietnam/TreeMix/HO.A.O.Mig2.Try9", "/mnt/scratch/dang/Vietnam/TreeMix/pop.color.list")
plot_resid("/mnt/scratch/dang/Vietnam/TreeMix/HO.A.O.Mig2.Try9", "/mnt/scratch/dang/Vietnam/TreeMix/pop.color.list")
# With 3 migration event
plot_tree("/mnt/scratch/dang/Vietnam/TreeMix/HO.A.O.Mig3.Try9", "/mnt/scratch/dang/Vietnam/TreeMix/pop.color.list")
plot_resid("/mnt/scratch/dang/Vietnam/TreeMix/HO.A.O.Mig3.Try9", "/mnt/scratch/dang/Vietnam/TreeMix/pop.color.list")
# With 4 migration event
plot_tree("/mnt/scratch/dang/Vietnam/TreeMix/HO.A.O.Mig4.Try3", "/mnt/scratch/dang/Vietnam/TreeMix/pop.color.list")
plot_resid("/mnt/scratch/dang/Vietnam/TreeMix/HO.A.O.Mig4.Try3", "/mnt/scratch/dang/Vietnam/TreeMix/pop.color.list")

## With ancient samples 2nd run
# No migration events
plot_tree("/mnt/scratch/dang/Vietnam/TreeMix/2nd_run_ancient/HO.A.O.Try6", "/mnt/scratch/dang/Vietnam/TreeMix/2nd_run_ancient/pop.color.list")
plot_resid("/mnt/scratch/dang/Vietnam/TreeMix/2nd_run_ancient/HO.A.O.Try6","/mnt/scratch/dang/Vietnam/TreeMix/2nd_run_ancient/pop.color.list")
# With 1 migration event
plot_tree("/mnt/scratch/dang/Vietnam/TreeMix/2nd_run_ancient/HO.A.O.Mig1.Try6", "/mnt/scratch/dang/Vietnam/TreeMix/2nd_run_ancient/pop.color.list")
plot_resid("/mnt/scratch/dang/Vietnam/TreeMix/2nd_run_ancient/HO.A.O.Mig1.Try6","/mnt/scratch/dang/Vietnam/TreeMix/2nd_run_ancient/pop.color.list")
# With 2 migration event
plot_tree("/mnt/scratch/dang/Vietnam/TreeMix/2nd_run_ancient/HO.A.O.Mig2.Try8", "/mnt/scratch/dang/Vietnam/TreeMix/2nd_run_ancient/pop.color.list")
plot_resid("/mnt/scratch/dang/Vietnam/TreeMix/2nd_run_ancient/HO.A.O.Mig2.Try8","/mnt/scratch/dang/Vietnam/TreeMix/2nd_run_ancient/pop.color.list")
# With 3 migration event
plot_tree("/mnt/scratch/dang/Vietnam/TreeMix/2nd_run_ancient/HO.A.O.Mig3.Try7", "/mnt/scratch/dang/Vietnam/TreeMix/2nd_run_ancient/pop.color.list")
plot_resid("/mnt/scratch/dang/Vietnam/TreeMix/2nd_run_ancient/HO.A.O.Mig3.Try7","/mnt/scratch/dang/Vietnam/TreeMix/2nd_run_ancient/pop.color.list")
# With 4 migration event
plot_tree("/mnt/scratch/dang/Vietnam/TreeMix/2nd_run_ancient/HO.A.O.Mig4.Try2", "/mnt/scratch/dang/Vietnam/TreeMix/2nd_run_ancient/pop.color.list")
plot_resid("/mnt/scratch/dang/Vietnam/TreeMix/2nd_run_ancient/HO.A.O.Mig4.Try2","/mnt/scratch/dang/Vietnam/TreeMix/2nd_run_ancient/pop.color.list")
# With 5 migration event
plot_tree("/mnt/scratch/dang/Vietnam/TreeMix/2nd_run_ancient/HO.A.O.Mig5.Try5", "/mnt/scratch/dang/Vietnam/TreeMix/2nd_run_ancient/pop.color.list")
plot_resid("/mnt/scratch/dang/Vietnam/TreeMix/2nd_run_ancient/HO.A.O.Mig5.Try5","/mnt/scratch/dang/Vietnam/TreeMix/2nd_run_ancient/pop.color.list")
# With 6 migration event
plot_tree("/mnt/scratch/dang/Vietnam/TreeMix/2nd_run_ancient/HO.A.O.Mig6.Try5", "/mnt/scratch/dang/Vietnam/TreeMix/2nd_run_ancient/pop.color.list")
plot_resid("/mnt/scratch/dang/Vietnam/TreeMix/2nd_run_ancient/HO.A.O.Mig6.Try5","/mnt/scratch/dang/Vietnam/TreeMix/2nd_run_ancient/pop.color.list")
# With 7 migration event
plot_tree("/mnt/scratch/dang/Vietnam/TreeMix/2nd_run_ancient/HO.A.O.Mig7.Try8", "/mnt/scratch/dang/Vietnam/TreeMix/2nd_run_ancient/pop.color.list")
plot_resid("/mnt/scratch/dang/Vietnam/TreeMix/2nd_run_ancient/HO.A.O.Mig7.Try8","/mnt/scratch/dang/Vietnam/TreeMix/2nd_run_ancient/pop.color.list")
# With 8 migration event
plot_tree("/mnt/scratch/dang/Vietnam/TreeMix/2nd_run_ancient/HO.A.O.Mig8.Try2", "/mnt/scratch/dang/Vietnam/TreeMix/2nd_run_ancient/pop.color.list")
plot_resid("/mnt/scratch/dang/Vietnam/TreeMix/2nd_run_ancient/HO.A.O.Mig8.Try2","/mnt/scratch/dang/Vietnam/TreeMix/2nd_run_ancient/pop.color.list")
# With 9 migration event
plot_tree("/mnt/scratch/dang/Vietnam/TreeMix/2nd_run_ancient/HO.A.O.Mig9.Try10", "/mnt/scratch/dang/Vietnam/TreeMix/2nd_run_ancient/pop.color.list")
plot_resid("/mnt/scratch/dang/Vietnam/TreeMix/2nd_run_ancient/HO.A.O.Mig9.Try10","/mnt/scratch/dang/Vietnam/TreeMix/2nd_run_ancient/pop.color.list")
# With 10 migration event
plot_tree("/mnt/scratch/dang/Vietnam/TreeMix/2nd_run_ancient/HO.A.O.Mig10.Try10", "/mnt/scratch/dang/Vietnam/TreeMix/2nd_run_ancient/pop.color.list")
plot_resid("/mnt/scratch/dang/Vietnam/TreeMix/2nd_run_ancient/HO.A.O.Mig10.Try10","/mnt/scratch/dang/Vietnam/TreeMix/2nd_run_ancient/pop.color.list")
