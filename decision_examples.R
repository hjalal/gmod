# Decision Tree ========
rm(list = ls())
#library(tidyverse)
library(gmod)
pSick <- function(decision){
  if (decision == "A") 0.5 else 0.7
}
mygmod <- gmod(model_type = "Decision") + 
  decisions("A", "B") + 
  event_mapping(event = "get_infection",  
                values = c(T, F), 
                outcomes = c("Sick", "Healthy"), 
                probs = c(pSick(decision), Inf)) + 
  payoffs(utility = )

model_struc <- gmod_build(mygmod)
model_struc
#model_num_struc <- gmod_parse(model_struc, params = NULL)
model_res <- gmod_evaluate(model_struc)
model_res


## rock climber example ======= 
pDie <- function(decision){
  if (decision == "Amputate") 0.99 else 0.8
}
pDie("Amputate")
pDie("Antibiotics")

mygmod <- gmod(model_type = "Decision") + 
  decisions("Amputate", "Antibiotics") + 
  #final_outcomes("Dead", "Alive") +
  #events("EVENT1", "DIE") + 
  event_mapping(event = "COMPL",  
                values = c(T, F), 
                outcomes = c("DIE", "DIE"), 
                probs = c(0.6, Inf))  + 
  event_mapping(event = "DIE",  
            values = c(T, F), 
            outcomes = c("Dead", "Alive"), 
            probs = c(pDie(decision), Inf)) 

model_struc <- gmod_build(mygmod)
model_struc
model_num_struc <- gmod_parse(model_struc, params = NULL)
model_res <- gmod_evaluate(model_num_struc)

print(model_res)

## HVE/OVE example ========
rm(list = ls())
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

cost <- function(decision, final_outcome){ #}, propHVE){
  c_biopsy*(decision=="Biopsy") + 
    c_tx*(decision=="Treat" | (decision=="Biopsy" & final_outcome %in% c("HVE_comp", "no_HVE_comp"))) + 
    c_VE_comp*(final_outcome %in% c("HVE_comp", "OVE_comp")) + 
    c_VE*(final_outcome %in% c("no_HVE_comp", "no_OVE_comp")) #+
    #c_HVE*propHVE
}
effectiveness <- function(decision, final_outcome){
  -q_loss_biopsy*(decision=="Biopsy") + 
    q_VE_comp*(final_outcome %in% c("HVE_comp", "OVE_comp")) + 
    q_VE*(final_outcome %in% c("no_HVE_comp", "no_OVE_comp"))
}
cost("Biopsy", "HVE_comp")
effectiveness("Biopsy", "Death")

mygmod <- gmod(model_type = "Decision") + 
  decisions("DoNotTreat", "Treat", "Biopsy") + 
  #final_outcomes("Death", "HVE_comp", "no_HVE_comp", "OVE_comp", "no_OVE_comp") +
  #events("DIE", "HVE","get_comp") + 
  event_mapping(event = "DIE",  
            values = c(T, F), 
            outcomes = c("Death", "HVE"), 
            probs = c(pDie(decision), Inf)) + 
  event_mapping(event = "HVE",  
            values = c(T, F), 
            outcomes = c("get_HVE_comp", "get_OVE_comp"), 
            probs = c(p_HVE, Inf)) +
  event_mapping(event = "get_HVE_comp", 
            values = c(T, F),
            outcomes = c("HVE_comp", "no_HVE_comp"),
            probs = c(p_comp(decision, HVE = TRUE), Inf))  +
  event_mapping(event = "get_OVE_comp", 
            values = c(T, F),
            outcomes = c("OVE_comp", "no_OVE_comp"),
            probs = c(p_comp(decision, HVE = FALSE), Inf)) + 
  #payoffs(cost = cost(decision, final_outcome, prop_with_event("HVE"=TRUE, decision)), 
  payoffs(cost = cost(decision, final_outcome),  
          effectiveness = effectiveness(decision, final_outcome))

model_struc <- gmod_build(mygmod)
model_num_struc <- gmod_parse(model_struc, params = NULL)
model_res <- gmod_evaluate(model_num_struc)

print(model_res)


## HVE/OVE example with prev_event_dependence ========
rm(list = ls())
#source("functions.R")
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
cost <- function(decision, final_outcome){ #}, propHVE){
  c_biopsy*(decision=="Biopsy") + 
    c_tx*(decision=="Treat" | (decision=="Biopsy" & final_outcome %in% c("HVE_comp", "no_HVE_comp"))) + 
    c_VE_comp*(final_outcome %in% c("HVE_comp", "OVE_comp")) + 
    c_VE*(final_outcome %in% c("no_HVE_comp", "no_OVE_comp")) #+
  #c_HVE*propHVE
}
effectiveness <- function(decision, final_outcome){
  -q_loss_biopsy*(decision=="Biopsy") + 
    q_VE_comp*(final_outcome %in% c("HVE_comp", "OVE_comp")) + 
    q_VE*(final_outcome %in% c("no_HVE_comp", "no_OVE_comp"))
}
mygmod <- gmod(model_type = "Decision") + 
  decisions("DoNotTreat", "Treat", "Biopsy") + 
  #final_outcomes("Death", "HVE_comp", "no_HVE_comp", "OVE_comp", "no_OVE_comp") +
  #events("DIE", "HVE","get_comp") + 
  event_mapping(event = "DIE",  
                values = c(T, F), 
                outcomes = c("Death", "HVE"), 
                probs = c(pDie(decision), Inf)) + 
  event_mapping(event = "HVE",  
                values = c(T, F), 
                outcomes = c("get_comp", "get_comp"), 
                #probs = c(f_HVE(prev_event("DIE")), Inf)) +
                probs = c(p_HVE, Inf)) +
  event_mapping(event = "get_comp", 
                values = c(T, F),
                outcomes = c("comp", "no_comp"),
                probs = c(p_comp(decision, HVE), Inf)) + 
  payoffs(cost = cost(decision, final_outcome),  
          effectiveness = effectiveness(decision, final_outcome))

model_struc <- gmod_build(mygmod)
#model_num_struc <- gmod_parse(model_struc, params = NULL)
model_res <- gmod_evaluate(model_struc, params = NULL)

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

cost <- function(decision, final_outcome){ 
  c_biopsy*(decision=="Biopsy") + 
    c_tx*(decision=="Treat" | (decision=="Biopsy" & final_outcome %in% c("HVE_comp", "no_HVE_comp"))) + 
    c_VE_comp*(final_outcome %in% c("HVE_comp", "OVE_comp")) + 
    c_VE*(final_outcome %in% c("no_HVE_comp", "no_OVE_comp"))
  }
effectiveness <- function(decision, final_outcome){
  -q_loss_biopsy*(decision=="Biopsy") + 
    q_VE_comp*(final_outcome %in% c("HVE_comp", "OVE_comp")) + 
    q_VE*(final_outcome %in% c("no_HVE_comp", "no_OVE_comp"))
}
cost("Biopsy", "HVE_comp")
effectiveness("Biopsy", "Death")

mygmod <- gmod(model_type = "Decision") + 
  decisions("DoNotTreat", "Treat", "Biopsy") + 
  #final_outcomes("Death", "HVE_comp", "no_HVE_comp", "OVE_comp", "no_OVE_comp") +
  #events("DIE", "HVE","get_comp") + 
  event_mapping(event = "DIE",  
                values = c(T, F), 
                outcomes = c("Death", "HVE"), 
                probs = c(pDie(decision), Inf), 
                payoffs = list(cost = c(cDie(decision), cNotDie), 
                               effectiveness = c(eDie, eNotDie))) + 
  event_mapping(event = "HVE",  
                values = c(T, F), 
                outcomes = c("get_HVE_comp", "get_OVE_comp"), 
                probs = c(p_HVE, Inf),
                payoffs = list(cost = c(cHVE(decision), NA))) +
  event_mapping(event = "get_HVE_comp", 
                values = c(T, F),
                outcomes = c("HVE_comp", "no_HVE_comp"),
                probs = c(p_comp(decision, HVE = TRUE), Inf))  +
  event_mapping(event = "get_OVE_comp", 
                values = c(T, F),
                outcomes = c("OVE_comp", "no_OVE_comp"),
                probs = c(p_comp(decision, HVE = FALSE), Inf)) + 
  #payoffs(cost = cost(decision, final_outcome, prop_with_event("HVE"=TRUE, decision)), 
  payoffs(cost = cost(decision, final_outcome, HVE),  
          effectiveness = effectiveness(decision, final_outcome, DIE))

model_struc <- gmod_build(mygmod)
model_num_struc <- gmod_parse(model_struc, params = NULL)
model_res <- gmod_evaluate(model_num_struc)

print(model_res)


# HVE example traditional R code ============
v_names_str    <- c("No Tx", "Tx All", "Biopsy")  # names of strategies
n_str          <- length(v_names_str)             # number of strategies
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
q_loss_biopsy  <- 0.01   # one-time  QALY loss due to brain biopsy
q_death_biopsy <- 0      # remaining QALYs for those who died during biopsy

# Create vector of weights for each strategy 

v_w_no_tx  <- c(    p_HVE  *      p_HVE_comp     ,  # HVE, complications
                    p_HVE  * (1 - p_HVE_comp)    ,  # HVE, no complications
                    (1 - p_HVE) *      p_OVE_comp     ,  # OVE, complications
                    (1 - p_HVE) * (1 - p_OVE_comp))      # OVE, no complications

v_w_tx     <- c(    p_HVE  *      p_HVE_comp_tx  ,  # HVE w/tx, complications
                    p_HVE  * (1 - p_HVE_comp_tx) ,  # HVE w/tx, no complications
                    (1 - p_HVE) *      p_OVE_comp_tx  ,  # OVE w/tx, complications
                    (1 - p_HVE) * (1 - p_OVE_comp_tx))   # OVE w/tx, no complications

v_w_biopsy <- c(p_biopsy_death                   ,  # biopsy death
                # no biopsy death.,   HVE w/tx,        complications
                (1-p_biopsy_death)   *    p_HVE  *    p_HVE_comp_tx  ,  
                # no biopsy death.,   HVE w/tx,     no complications
                (1-p_biopsy_death)   *    p_HVE  * (1-p_HVE_comp_tx) ,  
                # no biopsy death.,        OVE,        complications
                (1-p_biopsy_death)   * (1-p_HVE) *      p_OVE_comp   ,  
                # no biopsy death.,        OVE,     no complications
                (1-p_biopsy_death)   * (1-p_HVE) * (1 - p_OVE_comp))      

# Create vector of final_outcomes (QALYs) for each strategy 

v_qaly_no_tx  <- c(q_VE_comp ,          # HVE, complications
                   q_VE      ,          # HVE, no complications
                   q_VE_comp ,          # OVE, complications
                   q_VE)                # OVE, no complications

v_qaly_tx     <- c(q_VE_comp ,          # HVE, complications
                   q_VE      ,          # HVE, no complications
                   q_VE_comp ,          # OVE, complications
                   q_VE)                # OVE, no complications


v_qaly_biopsy <- -q_loss_biopsy     +   # loss due to biopsy
  c(q_death_biopsy  ,   # biopsy complications
    q_VE_comp       ,   # no biopsy comp., HVE w/tx, complications 
    q_VE            ,   # no biopsy comp., HVE w/tx, no complications
    q_VE_comp       ,   # no biopsy comp., OVE, complications
    q_VE)               # no biopsy comp., OVE, no complications

# Create vector of costs for each strategy 

v_cost_no_tx  <- c(c_VE_comp ,          # HVE, complications
                   c_VE      ,          # HVE, no complications
                   c_VE_comp ,          # OVE, complications
                   c_VE)                # OVE, no complications

v_cost_tx     <- c_tx +                 # cost of treatment
  c(c_VE_comp ,          # HVE, complications
    c_VE      ,          # HVE, no complications
    c_VE_comp ,          # OVE, complications
    c_VE)                # OVE, no complications

v_cost_biopsy <- c_biopsy           +   # cost of biopsy procedure
  c(0                ,   # cost of death (zero)
    c_VE_comp + c_tx ,   # no biopsy comp., HVE w/tx, complications 
    c_VE + c_tx      ,   # no biopsy comp., HVE w/tx, no complications
    c_VE_comp        ,   # no biopsy comp., OVE, complications
    c_VE)                # no biopsy comp., OVE, no complications

# Calculate total utilities for each strategy 
total_qaly_no_tx  <- v_w_no_tx  %*%  v_qaly_no_tx      
total_qaly_tx     <- v_w_tx     %*%  v_qaly_tx
total_qaly_biopsy <- v_w_biopsy %*%  v_qaly_biopsy

# Calculate total costs for each strategy 
total_cost_no_tx  <- v_w_no_tx  %*%  v_cost_no_tx    
total_cost_tx     <- v_w_tx     %*%  v_cost_tx
total_cost_biopsy <- v_w_biopsy %*%  v_cost_biopsy

# vector of total QALYs
v_total_qaly <- c(total_qaly_no_tx, total_qaly_tx, total_qaly_biopsy) 
# vector of total costs
v_total_cost <- c(total_cost_no_tx, total_cost_tx, total_cost_biopsy) 
# calculate vector of nmb
v_nmb        <- v_total_qaly * wtp - v_total_cost                      

# Name final_outcomes
names(v_total_qaly) <- v_names_str  # names for the elements of the total QALYs vector
names(v_total_cost) <- v_names_str  # names for the elements of the total cost vector
names(v_nmb)        <- v_names_str  # names for the elements of the nmb vector

df_output <- data.frame(Strategy =  v_names_str,
                        Cost     =  v_total_cost,
                        Effect   =  v_total_qaly,
                        NMB      =  v_nmb)

# model output
df_output
