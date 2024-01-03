# Run Doubilet
setwd("D:\\Box Sync\\RTree\\RDTree\\rdtree\\Doubilet1985")
source("doubilet1985code.R")

pDieBiopsy <- 0.004
pSevBiopsy <- 0.01
pModBiopsy <- 0.03
sensBiopsy <- 0.95
specBiopsy <- 0.99
pHSE <- 0.4 #overall

pDieHSE <- .7
pSevHSE <- .333
pModHSE <- .5

fDie <- .37
fSev <- .2
fMod <- .2

pDieNoHSE <- .18
pSevNoHSE <- .122
pModNoHSE <- .139

addProbDie <- .004
addProbSev <- .01
addProbMod <- .02

uDie <- 0
uSev <- 0.02
uMod <- .8
uMld <- 1

hse()