#  Doubilet 1985 example ========
## only Treat and NoTrt strategies with NA functions ========
rm(list = ls())
library(gmod)
#library(tidyverse)

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
pEvent <- function(HSE, decision, BiopRes, pEventHSE, pEventNoHSE, fEvent, addProbEvent){
  pEventRx <- if (HSE){
              (1-fEvent)*pEventHSE 
              } else {
                pEventNoHSE+addProbEvent-pEventNoHSE*addProbEvent
              }
  pEventNoRx <- if (HSE) pEventHSE else pEventNoHSE
  if (decision=="NoBiopsy_NoTreat") pEventNoRx else 
    if (decision=="NoBiopsy_Treat") pEventRx else 
      if (decision=="BrainBiopsy") 
        if (BiopRes) pEventRx else pEventNoRx
}

pBiopsy <- function(decision){
  (decision=="BrainBiopsy")
}
pBiopRes <- function(HSE){
  HSE*sensBiopsy + (!HSE)*(1-specBiopsy)
}

util <- function(decision, final_outcome, sevBiopSeq, modBiopSeq){
  if (decision=="BrainBiopsy"){
    if (!is.na(sevBiopSeq) & sevBiopSeq){
      uMult <- uSev
    } else {
      if (!is.na(modBiopSeq) & modBiopSeq){
        uMult <- uMod
      } else {
        uMult <- uMld
      }
    }
  } else {
    uMult <- 1
  }
  # uMult <- (decision=="BrainBiopsy")*(sevBiopSeq*uSev + modBiopSeq*uMod + (!modBiopSeq)*uMld) + 
  #   (decision!="BrainBiopsy")
  #print(uMult)
  switch(final_outcome,
         "DEAD" = uDie,
         "SEVSEQHSE" = uSev*uMult, 
         "MODSEQHSE" = uMod*uMult, 
         "MLDSEQHSE" = uMld*uMult)
}

## GMOD =========
mygmod <- gmod(model_type = "Decision") + 
  decisions("BrainBiopsy", "NoBiopsy_Treat", "NoBiopsy_NoTreat") + 
  #final_outcomes("DEAD","SEVSEQHSE","MODSEQHSE","MLDSEQHSE") + 
  event_mapping(event = "Biopsy",  
                values = c(TRUE, FALSE), 
                outcomes = c("dieBiop", "HSE"), 
                probs = c(pBiopsy(decision), Inf))  + 
  event_mapping(event = "dieBiop",  
                values = c(TRUE, FALSE), 
                outcomes = c("DEAD", "sevBiopSeq"), 
                probs = c(pDieBiopsy, Inf))  + 
  event_mapping(event = "sevBiopSeq",  
                values = c(TRUE, FALSE), 
                outcomes = c("HSE", "modBiopSeq"), 
                probs = c(pSevBiopsy, Inf))  + 
  event_mapping(event = "modBiopSeq",  
                values = c(TRUE, FALSE), 
                outcomes = c("HSE", "HSE"), 
                probs = c(pModBiopsy, Inf))  + 
  event_mapping(event = "HSE",  
                values = c(TRUE, FALSE), 
                outcomes = c("BiopAvail", "BiopAvail"), 
                probs = c(pHSE, Inf))  +
  event_mapping(event = "BiopAvail",  
                values = c(TRUE, FALSE), 
                outcomes = c("BiopRes", "die"), 
                probs = c(pBiopsy(decision), Inf))  + 
  event_mapping(event = "BiopRes",  
                #values = c("Positive", "Negative"), 
                values = c(TRUE, FALSE), 
                outcomes = c("die", "die"), 
                probs = c(pBiopRes(HSE), Inf))  + 
  event_mapping(event = "die",  
                values = c(T, F), 
                outcomes = c("DEAD", "sevSeqHSE"), 
                probs = c(pEvent(HSE, decision, BiopRes, pDieHSE, pDieNoHSE, fDie, addProbDie), Inf)) +
  event_mapping(event = "sevSeqHSE",  
                values = c(T, F), 
                outcomes = c("SEVSEQHSE", "modSeqHSE"), 
                probs = c(pEvent(HSE, decision, BiopRes, pSevHSE, pSevNoHSE, fSev, addProbSev), Inf)) +
  event_mapping(event = "modSeqHSE",  
                values = c(T, F), 
                outcomes = c("MODSEQHSE", "MLDSEQHSE"), 
                probs = c(pEvent(HSE, decision, BiopRes, pModHSE, pModNoHSE, fMod, addProbMod), Inf)) + 
  payoffs(util = util(decision, final_outcome, sevBiopSeq, modBiopSeq))

model_struc <- gmod_gen_model_function(mygmod)
model_res <- my_decision_model(params=NULL)
print(model_res)




