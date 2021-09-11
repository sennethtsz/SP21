rm(list=ls())

library(FrF2)

set.seed(42069)

design = FrF2(nruns = 16, nfactors = 10)
design
