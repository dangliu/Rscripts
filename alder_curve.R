## Alder fitting curve
# Ref: https://comppopgenworkshop2019.readthedocs.io/en/latest/contents/06_alder/alder.html

# Last updated: 21.Feb.2020

# This is the input decay table
fn1 = "/mnt/scratch/dang/Thailand/malder/alder.Mon.decay.txt"

# This should be retrieved from the alder log output
n = 17.85  ## decay parameter
M = 0.00006516  ## amp_exp
K = 0.00000228  ## amp_aff

# Using genetic map from 1000G
# This is the input decay table 
#fn1 = "/mnt/scratch/dang/Thailand/malder/map_from_1000G/alder.Mon.cm.decay.txt"

# This should be retrieved from the alder log 
#n = 19.79  ## decay parameter
#M = 0.00007512  ## amp_exp
#K = 0.00000228  ## amp_aff



# Curve from data
d1 = read.table(fn1, header=T)
xv = as.vector(d1$Dist)        ## distance between SNP bins (cM)
yv = as.vector(d1$weightedLD)  ## Weighted LD
fv = as.vector(d1$use) == "Y"  ## a boolean vector marking if each bin is used in fitting
prdv = M * exp(-1 * n * xv / 100) + K

# Fitting curve
plot(xv[fv], yv[fv], xlab="Genetic distance (cM)", ylab = "weighted LD", pch=4, col="blue")
points(xv[fv], prdv[fv], type="l", lwd=1.5, col="red")