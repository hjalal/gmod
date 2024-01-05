#  Doubilet 1985 example ========
## only Treat and NoTrt strategies  ========
rm(list = ls())
source("functions.R")
library(tidyverse)

# parameters =======
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


# functions =======
pDie <- function(HSE, decision){
  (decision=="NoBiopsy_NoTreat")*(HSE*pDieHSE + (!HSE)*pDieNoHSE) + 
  (decision=="NoBiopsy_Treat")*(HSE*(1-fDie)*pDieHSE + (!HSE)*(pDieNoHSE+addProbDie-pDieNoHSE*addProbDie)) 
}
pDie(TRUE, "NoBiopsy_Treat")
pSevSeq <- function(HSE, decision){
  (decision=="NoBiopsy_NoTreat")*(HSE*pSevHSE + (!HSE)*pSevNoHSE) + 
  (decision=="NoBiopsy_Treat")*(HSE*(1-fSev)*pSevHSE + (!HSE)*(pSevNoHSE+addProbSev-pSevNoHSE*addProbSev)) 
}
pModSeq <- function(HSE, decision){
  (decision=="NoBiopsy_NoTreat")*(HSE*pModHSE + (!HSE)*pModNoHSE) + 
  (decision=="NoBiopsy_Treat")*(HSE*(1-fMod)*pModHSE + (!HSE)*(pModNoHSE+addProbMod-pModNoHSE*addProbMod))
}
util <- function(outcome){
  switch(outcome,
         "DEAD" = uDie,
         "SEVSEQHSE" = uSev, 
         "MODSEQHSE" = uMod, 
         "MLDSEQHSE" = uMld)
}

# GMOD =========
mygmod <- gmod(model_type = "Decision") + 
  decisions("BrainBiopsy", "NoBiopsy_Treat", "NoBiopsy_NoTreat") + 
  outcomes("DEAD","SEVSEQHSE","MODSEQHSE","MLDSEQHSE") + 
  event_mapping(event = "event_HSE",  
                values = c(TRUE, FALSE), 
                results = c("event_die", "event_die"), 
                probs = c(pHSE, Inf))  + 
  event_mapping(event = "event_die",  
                values = c(T, F), 
                results = c("DEAD", "event_sevSeqHSE"), 
                probs = c(pDie(prev_event("event_HSE"), decision), Inf)) +
  event_mapping(event = "event_sevSeqHSE",  
                values = c(T, F), 
                results = c("SEVSEQHSE", "event_modSeqHSE"), 
                probs = c(pSevSeq(prev_event("event_HSE"), decision), Inf)) +
  event_mapping(event = "event_modSeqHSE",  
                values = c(T, F), 
                results = c("MODSEQHSE", "MLDSEQHSE"), 
                probs = c(pModSeq(prev_event("event_HSE"), decision), Inf)) + 
  payoffs(util = util(outcome))

model_struc <- gmod_build(mygmod)
model_struc
model_num_struc <- gmod_parse(model_struc, params = NULL)
model_num_struc
model_res <- gmod_evaluate(model_num_struc)

print(model_res)




# Doubilet's all strategies ===========
## only Treat and NoTrt strategies  ========
rm(list = ls())
source("functions.R")
library(tidyverse)

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

## functions =======
pDie <- function(HSE, decision, biopRes){
  pDieRx <- HSE*(1-fDie)*pDieHSE + (!HSE)*(pDieNoHSE+addProbDie-pDieNoHSE*addProbDie)
  pDieNoRx <- HSE*pDieHSE + (!HSE)*pDieNoHSE
  (decision=="NoBiopsy_NoTreat")*pDieNoRx + 
    (decision=="NoBiopsy_Treat")*pDieRx +
    (decision=="BrainBiopsy")*(biopRes*pDieRx + (!biopRes)*pDieNoRx)
}
pSevSeq <- function(HSE, decision, biopRes){
  pSevSeqRx <- HSE*(1-fSev)*pSevHSE + (!HSE)*(pSevNoHSE+addProbSev-pSevNoHSE*addProbSev)
  pSevSeqNoRx <- HSE*pSevHSE + (!HSE)*pSevNoHSE
  (decision=="NoBiopsy_NoTreat")*pSevSeqNoRx + 
    (decision=="NoBiopsy_Treat")*pSevSeqRx + 
    (decision=="BrainBiopsy")*(biopRes*pSevSeqRx + (!biopRes)*pSevSeqNoRx)
}
pModSeq <- function(HSE, decision, biopRes){
  pModSeqRx <- HSE*(1-fMod)*pModHSE + (!HSE)*(pModNoHSE+addProbMod-pModNoHSE*addProbMod)
  pModSeqNoRx <- HSE*pModHSE + (!HSE)*pModNoHSE
  (decision=="NoBiopsy_NoTreat")*pModSeqNoRx + 
    (decision=="NoBiopsy_Treat")*pModSeqRx + 
    (decision=="BrainBiopsy")*(biopRes*pModSeqRx + (!biopRes)*pModSeqNoRx)
}
pBiopsy <- function(decision){
  (decision=="BrainBiopsy")
}
pBiopRes <- function(HSE){
  HSE*sensBiopsy + (!HSE)*(1-specBiopsy)
}

util <- function(decision, outcome){ #}, sevBiopSeq, modBiopSeq){
  uMult <- 1 #(decision=="BrainBiopsy")*(sevBiopSeq*uSev + modBiopSeq*uMod + (!modBiopSeq)*uMld) + 
    #(decision!="BrainBiopsy")
  switch(outcome,
         "DEAD" = uDie,
         "SEVSEQHSE" = uSev*uMult, 
         "MODSEQHSE" = uMod*uMult, 
         "MLDSEQHSE" = uMld*uMult)
}

## GMOD =========
mygmod <- gmod(model_type = "Decision") + 
  decisions("BrainBiopsy", "NoBiopsy_Treat", "NoBiopsy_NoTreat") + 
  outcomes("DEAD","SEVSEQHSE","MODSEQHSE","MLDSEQHSE") + 
  event_mapping(event = "event_Biopsy",  
                values = c(TRUE, FALSE), 
                results = c("event_dieBiop", "event_HSE"), 
                probs = c(pBiopsy(decision), Inf))  + 
  event_mapping(event = "event_dieBiop",  
                values = c(TRUE, FALSE), 
                results = c("DEAD", "event_sevBiopSeq"), 
                probs = c(pDieBiopsy, Inf))  + 
  event_mapping(event = "event_sevBiopSeq",  
                values = c(TRUE, FALSE), 
                results = c("event_HSE", "event_modBiopSeq"), 
                probs = c(pSevBiopsy, Inf))  + 
  event_mapping(event = "event_modBiopSeq",  
                values = c(TRUE, FALSE), 
                results = c("event_HSE", "event_HSE"), 
                probs = c(pModBiopsy, Inf))  + 
  event_mapping(event = "event_HSE",  
                values = c(TRUE, FALSE), 
                results = c("event_BiopAvail", "event_BiopAvail"), 
                probs = c(pHSE, Inf))  +
  event_mapping(event = "event_BiopAvail",  
                values = c(TRUE, FALSE), 
                results = c("event_BiopRes", "event_die"), 
                probs = c(pBiopsy(decision), Inf))  + 
  event_mapping(event = "event_BiopRes",  
                #values = c("Positive", "Negative"), 
                values = c(TRUE, FALSE), 
                results = c("event_die", "event_die"), 
                probs = c(pBiopRes(prev_event("event_HSE")), Inf))  + 
  event_mapping(event = "event_die",  
                values = c(T, F), 
                results = c("DEAD", "event_sevSeqHSE"), 
                probs = c(pDie(prev_event("event_HSE"), decision, prev_event("event_BiopRes")), Inf)) +
  event_mapping(event = "event_sevSeqHSE",  
                values = c(T, F), 
                results = c("SEVSEQHSE", "event_modSeqHSE"), 
                probs = c(pSevSeq(prev_event("event_HSE"), decision, prev_event("event_BiopRes")), Inf)) +
  event_mapping(event = "event_modSeqHSE",  
                values = c(T, F), 
                results = c("MODSEQHSE", "MLDSEQHSE"), 
                probs = c(pModSeq(prev_event("event_HSE"), decision, prev_event("event_BiopRes")), Inf)) + 
  payoffs(util = util(decision, outcome)) #, prev_event("event_sevBiopSeq"), prev_event("event_modBiopSeq")))

model_struc <- gmod_build(mygmod)
model_struc
model_num_struc <- gmod_parse(model_struc, params = NULL)
model_num_struc
model_res <- gmod_evaluate(model_num_struc)

print(model_res)


# Doubilet example from OpenTree
a1 <- c(prod(c(pDieBiopsy)), prod(c(1 - pDieBiopsy, pSevBiopsy, pHSE, 1 - sensBiopsy, pDieHSE)), prod(c(1 - pDieBiopsy, pSevBiopsy, pHSE, 1 - sensBiopsy, 1 - pDieHSE, pSevHSE)), prod(c(1 - pDieBiopsy, pSevBiopsy, pHSE, 1 - sensBiopsy, 1 - pDieHSE, 1 - pSevHSE, pModHSE)), prod(c(1 - pDieBiopsy, pSevBiopsy, pHSE, 1 - sensBiopsy, 1 - pDieHSE, 1 - pSevHSE, 1 - pModHSE)), prod(c(1 - pDieBiopsy, pSevBiopsy, pHSE, sensBiopsy, (1 - fDie) * pDieHSE)), prod(c(1 - pDieBiopsy, pSevBiopsy, pHSE, sensBiopsy, 1 - (1 - fDie) * pDieHSE, (1 - fSev) * pSevHSE)), prod(c(1 - pDieBiopsy, pSevBiopsy, pHSE, sensBiopsy, 1 - (1 - fDie) * pDieHSE, 1 - (1 - fSev) * pSevHSE, (1 - fMod) * pModHSE)), prod(c(1 - pDieBiopsy, pSevBiopsy, pHSE, sensBiopsy, 1 - (1 - fDie) * pDieHSE, 1 - (1 - fSev) * pSevHSE, 1 - (1 - fMod) * pModHSE)), prod(c(1 - pDieBiopsy, pSevBiopsy, 1 - pHSE, specBiopsy, pDieNoHSE)), prod(c(1 - pDieBiopsy, pSevBiopsy, 1 - pHSE, specBiopsy, 1 - pDieNoHSE, pSevNoHSE)), prod(c(1 - pDieBiopsy, pSevBiopsy, 1 - pHSE, specBiopsy, 1 - pDieNoHSE, 1 - pSevNoHSE, pModNoHSE)), prod(c(1 - pDieBiopsy, pSevBiopsy, 1 - pHSE, specBiopsy, 1 - pDieNoHSE, 1 - pSevNoHSE, 1 - pModNoHSE)), prod(c(1 - pDieBiopsy, pSevBiopsy, 1 - pHSE, 1 - specBiopsy, pDieNoHSE + addProbDie - pDieNoHSE * addProbDie)), prod(c(1 - pDieBiopsy, pSevBiopsy, 1 - pHSE, 1 - specBiopsy, 1 - (pDieNoHSE + addProbDie - pDieNoHSE * addProbDie), pSevNoHSE + addProbSev - pSevNoHSE * addProbSev)), prod(c(1 - pDieBiopsy, pSevBiopsy, 1 - pHSE, 1 - specBiopsy, 1 - (pDieNoHSE + addProbDie - pDieNoHSE * addProbDie), 1 - (pSevNoHSE + addProbSev - pSevNoHSE * addProbSev), pModNoHSE + addProbMod - pModNoHSE * addProbMod)), prod(c(1 - pDieBiopsy, pSevBiopsy, 1 - pHSE, 1 - specBiopsy, 1 - (pDieNoHSE + addProbDie - pDieNoHSE * addProbDie), 1 - (pSevNoHSE + addProbSev - pSevNoHSE * addProbSev), 1 - (pModNoHSE + addProbMod - pModNoHSE * addProbMod))), prod(c(1 - pDieBiopsy, 1 - pSevBiopsy, pModBiopsy, pHSE, 1 - sensBiopsy, pDieHSE)), prod(c(1 - pDieBiopsy, 1 - pSevBiopsy, pModBiopsy, pHSE, 1 - sensBiopsy, 1 - pDieHSE, pSevHSE)), prod(c(1 - pDieBiopsy, 1 - pSevBiopsy, pModBiopsy, pHSE, 1 - sensBiopsy, 1 - pDieHSE, 1 - pSevHSE, pModHSE)), prod(c(1 - pDieBiopsy, 1 - pSevBiopsy, pModBiopsy, pHSE, 1 - sensBiopsy, 1 - pDieHSE, 1 - pSevHSE, 1 - pModHSE)), prod(c(1 - pDieBiopsy, 1 - pSevBiopsy, pModBiopsy, pHSE, sensBiopsy, (1 - fDie) * pDieHSE)), prod(c(1 - pDieBiopsy, 1 - pSevBiopsy, pModBiopsy, pHSE, sensBiopsy, 1 - (1 - fDie) * pDieHSE, (1 - fSev) * pSevHSE)), prod(c(1 - pDieBiopsy, 1 - pSevBiopsy, pModBiopsy, pHSE, sensBiopsy, 1 - (1 - fDie) * pDieHSE, 1 - (1 - fSev) * pSevHSE, (1 - fMod) * pModHSE)), prod(c(1 - pDieBiopsy, 1 - pSevBiopsy, pModBiopsy, pHSE, sensBiopsy, 1 - (1 - fDie) * pDieHSE, 1 - (1 - fSev) * pSevHSE, 1 - (1 - fMod) * pModHSE)), prod(c(1 - pDieBiopsy, 1 - pSevBiopsy, pModBiopsy, 1 - pHSE, specBiopsy, pDieNoHSE)), prod(c(1 - pDieBiopsy, 1 - pSevBiopsy, pModBiopsy, 1 - pHSE, specBiopsy, 1 - pDieNoHSE, pSevNoHSE)), prod(c(1 - pDieBiopsy, 1 - pSevBiopsy, pModBiopsy, 1 - pHSE, specBiopsy, 1 - pDieNoHSE, 1 - pSevNoHSE, pModNoHSE)))

a2 <- c(prod(c(1 - pDieBiopsy, 1 - pSevBiopsy, pModBiopsy, 1 - pHSE, specBiopsy, 1 - pDieNoHSE, 1 - pSevNoHSE, 1 - pModNoHSE)), prod(c(1 - pDieBiopsy, 1 - pSevBiopsy, pModBiopsy, 1 - pHSE, 1 - specBiopsy, pDieNoHSE + addProbDie - pDieNoHSE * addProbDie)), prod(c(1 - pDieBiopsy, 1 - pSevBiopsy, pModBiopsy, 1 - pHSE, 1 - specBiopsy, 1 - (pDieNoHSE + addProbDie - pDieNoHSE * addProbDie), pSevNoHSE + addProbSev - pSevNoHSE * addProbSev)), prod(c(1 - pDieBiopsy, 1 - pSevBiopsy, pModBiopsy, 1 - pHSE, 1 - specBiopsy, 1 - (pDieNoHSE + addProbDie - pDieNoHSE * addProbDie), 1 - (pSevNoHSE + addProbSev - pSevNoHSE * addProbSev), pModNoHSE + addProbMod - pModNoHSE * addProbMod)), prod(c(1 - pDieBiopsy, 1 - pSevBiopsy, pModBiopsy, 1 - pHSE, 1 - specBiopsy, 1 - (pDieNoHSE + addProbDie - pDieNoHSE * addProbDie), 1 - (pSevNoHSE + addProbSev - pSevNoHSE * addProbSev), 1 - (pModNoHSE + addProbMod - pModNoHSE * addProbMod))), prod(c(1 - pDieBiopsy, 1 - pSevBiopsy, 1 - pModBiopsy, pHSE, 1 - sensBiopsy, pDieHSE)), prod(c(1 - pDieBiopsy, 1 - pSevBiopsy, 1 - pModBiopsy, pHSE, 1 - sensBiopsy, 1 - pDieHSE, pSevHSE)), prod(c(1 - pDieBiopsy, 1 - pSevBiopsy, 1 - pModBiopsy, pHSE, 1 - sensBiopsy, 1 - pDieHSE, 1 - pSevHSE, pModHSE)), prod(c(1 - pDieBiopsy, 1 - pSevBiopsy, 1 - pModBiopsy, pHSE, 1 - sensBiopsy, 1 - pDieHSE, 1 - pSevHSE, 1 - pModHSE)), prod(c(1 - pDieBiopsy, 1 - pSevBiopsy, 1 - pModBiopsy, pHSE, sensBiopsy, (1 - fDie) * pDieHSE)), prod(c(1 - pDieBiopsy, 1 - pSevBiopsy, 1 - pModBiopsy, pHSE, sensBiopsy, 1 - (1 - fDie) * pDieHSE, (1 - fSev) * pSevHSE)), prod(c(1 - pDieBiopsy, 1 - pSevBiopsy, 1 - pModBiopsy, pHSE, sensBiopsy, 1 - (1 - fDie) * pDieHSE, 1 - (1 - fSev) * pSevHSE, (1 - fMod) * pModHSE)), prod(c(1 - pDieBiopsy, 1 - pSevBiopsy, 1 - pModBiopsy, pHSE, sensBiopsy, 1 - (1 - fDie) * pDieHSE, 1 - (1 - fSev) * pSevHSE, 1 - (1 - fMod) * pModHSE)), prod(c(1 - pDieBiopsy, 1 - pSevBiopsy, 1 - pModBiopsy, 1 - pHSE, specBiopsy, pDieNoHSE)), prod(c(1 - pDieBiopsy, 1 - pSevBiopsy, 1 - pModBiopsy, 1 - pHSE, specBiopsy, 1 - pDieNoHSE, pSevNoHSE)), prod(c(1 - pDieBiopsy, 1 - pSevBiopsy, 1 - pModBiopsy, 1 - pHSE, specBiopsy, 1 - pDieNoHSE, 1 - pSevNoHSE, pModNoHSE)), prod(c(1 - pDieBiopsy, 1 - pSevBiopsy, 1 - pModBiopsy, 1 - pHSE, specBiopsy, 1 - pDieNoHSE, 1 - pSevNoHSE, 1 - pModNoHSE)), prod(c(1 - pDieBiopsy, 1 - pSevBiopsy, 1 - pModBiopsy, 1 - pHSE, 1 - specBiopsy, pDieNoHSE + addProbDie - pDieNoHSE * addProbDie)), prod(c(1 - pDieBiopsy, 1 - pSevBiopsy, 1 - pModBiopsy, 1 - pHSE, 1 - specBiopsy, 1 - (pDieNoHSE + addProbDie - pDieNoHSE * addProbDie), pSevNoHSE + addProbSev - pSevNoHSE * addProbSev)), prod(c(1 - pDieBiopsy, 1 - pSevBiopsy, 1 - pModBiopsy, 1 - pHSE, 1 - specBiopsy, 1 - (pDieNoHSE + addProbDie - pDieNoHSE * addProbDie), 1 - (pSevNoHSE + addProbSev - pSevNoHSE * addProbSev), pModNoHSE + addProbMod - pModNoHSE * addProbMod)), prod(c(1 - pDieBiopsy, 1 - pSevBiopsy, 1 - pModBiopsy, 1 - pHSE, 1 - specBiopsy, 1 - (pDieNoHSE + addProbDie - pDieNoHSE * addProbDie), 1 - (pSevNoHSE + addProbSev - pSevNoHSE * addProbSev), 1 - (pModNoHSE + addProbMod - pModNoHSE * addProbMod)))) 
probs1 <- c(a1,a2)
utils1 <- c(uDie, uDie * uSev, uSev * uSev, uMod * uSev, uMld * uSev, uDie * uSev, uSev * uSev, uMod * uSev, uMld * uSev, uDie * uSev, uSev * uSev, uMod * uSev, uMld * uSev, uDie * uSev, uSev * uSev, uMod * uSev, uMld * uSev, uDie * uMod, uSev * uMod, uMod * uMod, uMld * uMod, uDie * uMod, uSev * uMod, uMod * uMod, uMld * uMod, uDie * uMod, uSev * uMod, uMod * uMod, uMld * uMod, uDie * uMod, uSev * uMod, uMod * uMod, uMld * uMod, uDie * uMld, uSev * uMld, uMod * uMld, uMld * uMld, uDie * uMld, uSev * uMld, uMod * uMld, uMld * uMld, uDie * uMld, uSev * uMld, uMod * uMld, uMld * uMld, uDie * uMld, uSev * uMld, uMod * uMld, uMld * uMld)
utils1_red <- c(uDie, uDie, uSev, uMod, uMld, uDie, uSev, uMod, uMld, uDie, uSev, uMod, uMld, uDie, uSev, uMod, uMld, uDie, uSev, uMod, uMld, uDie, uSev, uMod, uMld, uDie, uSev, uMod, uMld, uDie, uSev, uMod, uMld, uDie, uSev, uMod, uMld, uDie, uSev, uMod, uMld, uDie, uSev, uMod, uMld, uDie, uSev, uMod, uMld)
probs1 %*% utils1
probs1 %*% utils1_red # doesn't multiply the outcome utilities by biopsy sequalae
  
probs2 <- c(prod(c(pHSE, (1 - fDie) * pDieHSE)), prod(c(pHSE, 1 - (1 - fDie) * pDieHSE, (1 - fSev) * pSevHSE)), prod(c(pHSE, 1 - (1 - fDie) * pDieHSE, 1 - (1 - fSev) * pSevHSE, (1 - fMod) * pModHSE)), prod(c(pHSE, 1 - (1 - fDie) * pDieHSE, 1 - (1 - fSev) * pSevHSE, 1 - (1 - fMod) * pModHSE)), prod(c(1 - pHSE, pDieNoHSE + addProbDie - pDieNoHSE * addProbDie)), prod(c(1 - pHSE, 1 - (pDieNoHSE + addProbDie - pDieNoHSE * addProbDie), pSevNoHSE + addProbSev - pSevNoHSE * addProbSev)), prod(c(1 - pHSE, 1 - (pDieNoHSE + addProbDie - pDieNoHSE * addProbDie), 1 - (pSevNoHSE + addProbSev - pSevNoHSE * addProbSev), pModNoHSE + addProbMod - pModNoHSE * addProbMod)), prod(c(1 - pHSE, 1 - (pDieNoHSE + addProbDie - pDieNoHSE * addProbDie), 1 - (pSevNoHSE + addProbSev - pSevNoHSE * addProbSev), 1 - (pModNoHSE + addProbMod - pModNoHSE * addProbMod))))
util2 <-  c(uDie, uSev, uMod, uMld, uDie, uSev, uMod, uMld)
probs2
# 0.17640000 0.05956704 0.06561318 0.09841978 0.10996800 0.06408638 0.06654122 0.35940439
util2
# 0.00 0.02 0.80 1.00 0.00 0.02 0.80 1.00
probs2 %*% util2


probs3 <- c(prod(c(pHSE, pDieHSE)), prod(c(pHSE, 1 - pDieHSE, pSevHSE)), prod(c(pHSE, 1 - pDieHSE, 1 - pSevHSE, pModHSE)), prod(c(pHSE, 1 - pDieHSE, 1 - pSevHSE, 1 - pModHSE)), prod(c(1 - pHSE, pDieNoHSE)), prod(c(1 - pHSE, 1 - pDieNoHSE, pSevNoHSE)), prod(c(1 - pHSE, 1 - pDieNoHSE, 1 - pSevNoHSE, pModNoHSE)), prod(c(1 - pHSE, 1 - pDieNoHSE, 1 - pSevNoHSE, 1 - pModNoHSE))) 
probs3
# 0.28000000 0.03996000 0.04002000 0.04002000 0.10800000 0.06002400 0.06004466 0.37193134
probs3 %*% util2
