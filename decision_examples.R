# Decision Tree ========
rm(list = ls())
source("functions.R")
library(tidyverse)

## rock climber example ======= 
pDie <- function(decision){
  if (decision == "Amputate") 0.99 else 0.8
}
pDie("Amputate")
pDie("Antibiotics")

mygmod <- gmod(model_type = "Decision") + 
  decisions("Amputate", "Antibiotics") + 
  #outcomes("Dead", "Alive") +
  #events("EVENT1", "DIE") + 
  event_mapping(event = "COMPL",  
                values = c(T, F), 
                results = c("DIE", "DIE"), 
                probs = c(0.6, Inf))  + 
  event_mapping(event = "DIE",  
            values = c(T, F), 
            results = c("Dead", "Alive"), 
            probs = c(pDie(decision), Inf)) 

model_struc <- gmod_build(mygmod)
model_struc
model_num_struc <- gmod_parse(model_struc, params = NULL)
model_res <- gmod_evaluate(model_num_struc)

print(model_res)

## HVE/OVE example ========
rm(list = ls())
source("functions.R")
library(tidyverse)


v_names_str    <- c('Do not treat', "Treat", "Biopsy")  # names of strategies
n_str          <- length(v_names_str)                   # number of strategies
wtp            <- 100000                          # willingness to pay threshold

# Probabilities
p_HVE          <- 0.52   # prevalence of HVE
p_HVE_comp     <- 0.71   # complications with untreated HVE
p_OVE_comp     <- 0.01   # complications with untreated OVE
p_HVE_comp_tx  <- 0.36   # complications with treated HVE
p_OVE_comp_tx  <- 0.20   # complications with treated OVE
p_biopsy_death <- 0.005  # probability of death due to biopsy

# Costs
c_VE           <- 1200   # cost of viral encephalitis care without complications
c_VE_comp      <- 9000   # cost of viral encephalitis care with complications
c_tx           <- 9500   # cost of treatment
c_biopsy       <- 25000  # cost of brain biopsy

# QALYs
q_VE           <- 20     # remaining QALYs for those without VE-related complications
q_VE_comp      <- 19     # remaining QALYs for those with VE-related complications
q_loss_biopsy  <- 0.01   # one-time QALY loss due to brain biopsy
q_death_biopsy <- 0      # remaining QALYs for those who died during biopsy



pDie <- function(decision){
  if (decision == "Biopsy") p_biopsy_death else 0
}
p_comp <- function(decision, HVE){
  if (decision == "DoNotTreat" & HVE ) return(p_HVE_comp)
  if (decision == "DoNotTreat" & !HVE )return(p_OVE_comp)
  if (decision == "Treat" & HVE ) return(p_HVE_comp_tx)
  if (decision == "Treat" & !HVE ) return(p_OVE_comp_tx)
  if (decision == "Biopsy" & HVE) return(p_HVE_comp_tx)
  if (decision == "Biopsy" & !HVE) return(p_OVE_comp)
}
p_comp(decision = "Biopsy", prev_event = "OVE")
c_HVE <- function(decision){
  if (decision == "biopsy") c_tx else 0
}

cost <- function(decision, outcome){ #}, propHVE){
  c_biopsy*(decision=="Biopsy") + 
    c_tx*(decision=="Treat" | (decision=="Biopsy" & outcome %in% c("HVE_comp", "no_HVE_comp"))) + 
    c_VE_comp*(outcome %in% c("HVE_comp", "OVE_comp")) + 
    c_VE*(outcome %in% c("no_HVE_comp", "no_OVE_comp")) #+
    #c_HVE*propHVE
}
effectiveness <- function(decision, outcome){
  -q_loss_biopsy*(decision=="Biopsy") + 
    q_VE_comp*(outcome %in% c("HVE_comp", "OVE_comp")) + 
    q_VE*(outcome %in% c("no_HVE_comp", "no_OVE_comp"))
}
cost("Biopsy", "HVE_comp")
effectiveness("Biopsy", "Death")

mygmod <- gmod(model_type = "Decision") + 
  decisions("DoNotTreat", "Treat", "Biopsy") + 
  #outcomes("Death", "HVE_comp", "no_HVE_comp", "OVE_comp", "no_OVE_comp") +
  #events("DIE", "HVE","get_comp") + 
  event_mapping(event = "DIE",  
            values = c(T, F), 
            results = c("Death", "HVE"), 
            probs = c(pDie(decision), Inf)) + 
  event_mapping(event = "HVE",  
            values = c(T, F), 
            results = c("get_HVE_comp", "get_OVE_comp"), 
            probs = c(p_HVE, Inf)) +
  event_mapping(event = "get_HVE_comp", 
            values = c(T, F),
            results = c("HVE_comp", "no_HVE_comp"),
            probs = c(p_comp(decision, HVE = TRUE), Inf))  +
  event_mapping(event = "get_OVE_comp", 
            values = c(T, F),
            results = c("OVE_comp", "no_OVE_comp"),
            probs = c(p_comp(decision, HVE = FALSE), Inf)) + 
  #payoffs(cost = cost(decision, outcome, prop_with_event("HVE"=TRUE, decision)), 
  payoffs(cost = cost(decision, outcome),  
          effectiveness = effectiveness(decision, outcome))

model_struc <- gmod_build(mygmod)
model_num_struc <- gmod_parse(model_struc, params = NULL)
model_res <- gmod_evaluate(model_num_struc)

print(model_res)


## HVE/OVE example with prev_event_dependence ========
rm(list = ls())
source("functions.R")
library(tidyverse)


v_names_str    <- c('Do not treat', "Treat", "Biopsy")  # names of strategies
n_str          <- length(v_names_str)                   # number of strategies
wtp            <- 100000                          # willingness to pay threshold

# Probabilities
p_HVE          <- 0.52   # prevalence of HVE
p_HVE_comp     <- 0.71   # complications with untreated HVE
p_OVE_comp     <- 0.01   # complications with untreated OVE
p_HVE_comp_tx  <- 0.36   # complications with treated HVE
p_OVE_comp_tx  <- 0.20   # complications with treated OVE
p_biopsy_death <- 0.005  # probability of death due to biopsy

# Costs
c_VE           <- 1200   # cost of viral encephalitis care without complications
c_VE_comp      <- 9000   # cost of viral encephalitis care with complications
c_tx           <- 9500   # cost of treatment
c_biopsy       <- 25000  # cost of brain biopsy

# QALYs
q_VE           <- 20     # remaining QALYs for those without VE-related complications
q_VE_comp      <- 19     # remaining QALYs for those with VE-related complications
q_loss_biopsy  <- 0.01   # one-time QALY loss due to brain biopsy
q_death_biopsy <- 0      # remaining QALYs for those who died during biopsy



pDie <- function(decision){
  if (decision == "Biopsy") p_biopsy_death else 0
}
p_comp <- function(decision, HVE, DIE){
  if (decision == "DoNotTreat" & HVE ) return(p_HVE_comp)
  if (decision == "DoNotTreat" & !HVE ) return(p_OVE_comp)
  if (decision == "Treat" & HVE ) return(p_HVE_comp_tx)
  if (decision == "Treat" & !HVE ) return(p_OVE_comp_tx)
  if (decision == "Biopsy" & HVE) return(p_HVE_comp_tx)
  if (decision == "Biopsy" & !HVE) return(p_OVE_comp)
}
p_comp(decision = "Biopsy", HVE = TRUE)
f_HVE <- function(DIE){
  (!DIE) * p_HVE
}
mygmod <- gmod(model_type = "Decision") + 
  decisions("DoNotTreat", "Treat", "Biopsy") + 
  #outcomes("Death", "HVE_comp", "no_HVE_comp", "OVE_comp", "no_OVE_comp") +
  #events("DIE", "HVE","get_comp") + 
  event_mapping(event = "DIE",  
                values = c(T, F), 
                results = c("Death", "HVE"), 
                probs = c(pDie(decision), Inf)) + 
  event_mapping(event = "HVE",  
                values = c(T, F), 
                results = c("get_comp", "get_comp"), 
                #probs = c(f_HVE(prev_event("DIE")), Inf)) +
                probs = c(p_HVE, Inf)) +
  event_mapping(event = "get_comp", 
                values = c(T, F),
                results = c("comp", "no_comp"),
                probs = c(p_comp(decision, prev_event("HVE")), Inf))

model_struc <- gmod_build(mygmod)
model_num_struc <- gmod_parse(model_struc, params = NULL)
model_res <- gmod_evaluate(model_num_struc)

print(model_res)


#
## HVE/OVE example with event rewards ========
rm(list = ls())
source("functions.R")
library(tidyverse)


v_names_str    <- c('Do not treat', "Treat", "Biopsy")  # names of strategies
n_str          <- length(v_names_str)                   # number of strategies
wtp            <- 100000                          # willingness to pay threshold

# Probabilities
p_HVE          <- 0.52   # prevalence of HVE
p_HVE_comp     <- 0.71   # complications with untreated HVE
p_OVE_comp     <- 0.01   # complications with untreated OVE
p_HVE_comp_tx  <- 0.36   # complications with treated HVE
p_OVE_comp_tx  <- 0.20   # complications with treated OVE
p_biopsy_death <- 0.005  # probability of death due to biopsy

# Costs
c_VE           <- 1200   # cost of viral encephalitis care without complications
c_VE_comp      <- 9000   # cost of viral encephalitis care with complications
c_tx           <- 9500   # cost of treatment
c_biopsy       <- 25000  # cost of brain biopsy

# QALYs
q_VE           <- 20     # remaining QALYs for those without VE-related complications
q_VE_comp      <- 19     # remaining QALYs for those with VE-related complications
q_loss_biopsy  <- 0.01   # one-time QALY loss due to brain biopsy
q_death_biopsy <- 0      # remaining QALYs for those who died during biopsy

cDie <- function(decision){
  if (decision == "Biopsy") 10000 else 500
}
cNotDie <- 1000
eDie <- 0.1
eNotDie <- 0.9


pDie <- function(decision){
  if (decision == "Biopsy") p_biopsy_death else 0
}
p_comp <- function(decision, HVE){
  if (decision == "DoNotTreat" & HVE ) return(p_HVE_comp)
  if (decision == "DoNotTreat" & !HVE )return(p_OVE_comp)
  if (decision == "Treat" & HVE ) return(p_HVE_comp_tx)
  if (decision == "Treat" & !HVE ) return(p_OVE_comp_tx)
  if (decision == "Biopsy" & HVE) return(p_HVE_comp_tx)
  if (decision == "Biopsy" & !HVE) return(p_OVE_comp)
}
cHVE <- function(decision){
  if (decision == "Biopsy") c_tx else 0
}

cost <- function(decision, outcome){ 
  c_biopsy*(decision=="Biopsy") + 
    c_tx*(decision=="Treat" | (decision=="Biopsy" & outcome %in% c("HVE_comp", "no_HVE_comp"))) + 
    c_VE_comp*(outcome %in% c("HVE_comp", "OVE_comp")) + 
    c_VE*(outcome %in% c("no_HVE_comp", "no_OVE_comp"))
  }
effectiveness <- function(decision, outcome){
  -q_loss_biopsy*(decision=="Biopsy") + 
    q_VE_comp*(outcome %in% c("HVE_comp", "OVE_comp")) + 
    q_VE*(outcome %in% c("no_HVE_comp", "no_OVE_comp"))
}
cost("Biopsy", "HVE_comp")
effectiveness("Biopsy", "Death")

mygmod <- gmod(model_type = "Decision") + 
  decisions("DoNotTreat", "Treat", "Biopsy") + 
  #outcomes("Death", "HVE_comp", "no_HVE_comp", "OVE_comp", "no_OVE_comp") +
  #events("DIE", "HVE","get_comp") + 
  event_mapping(event = "DIE",  
                values = c(T, F), 
                results = c("Death", "HVE"), 
                probs = c(pDie(decision), Inf), 
                payoffs = list(cost = c(cDie(decision), cNotDie), 
                               effectiveness = c(eDie, eNotDie))) + 
  event_mapping(event = "HVE",  
                values = c(T, F), 
                results = c("get_HVE_comp", "get_OVE_comp"), 
                probs = c(p_HVE, Inf),
                payoffs = list(cost = c(cHVE(decision), NA))) +
  event_mapping(event = "get_HVE_comp", 
                values = c(T, F),
                results = c("HVE_comp", "no_HVE_comp"),
                probs = c(p_comp(decision, HVE = TRUE), Inf))  +
  event_mapping(event = "get_OVE_comp", 
                values = c(T, F),
                results = c("OVE_comp", "no_OVE_comp"),
                probs = c(p_comp(decision, HVE = FALSE), Inf)) + 
  #payoffs(cost = cost(decision, outcome, prop_with_event("HVE"=TRUE, decision)), 
  payoffs(cost = cost(decision, outcome),  
          effectiveness = effectiveness(decision, outcome))

model_struc <- gmod_build(mygmod)
model_num_struc <- gmod_parse(model_struc, params = NULL)
model_res <- gmod_evaluate(model_num_struc)

print(model_res)

# Doubilet example? ==========

