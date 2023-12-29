# Markov with n_cycles# 
rm(list = ls())
source("functions.R")
library(tidyverse)
# Parameters:
pProgNoTrt <-	0.100
rrProgTrtA<-	0.8
rrProgTrtB<-	0.7

pMortMod	<- 0.050
pMortSev	<- 0.200



# without arguments because it gets determined by the local environment
rrProg <- function(decision){
  #decision <- get("decision", envir = parent.frame())  # Get x from the calling environment
  switch(decision, "NoTrt" = 1,
         "TrtA" = rrProgTrtA,
         "TrtB" = rrProgTrtB)
  #df_rrProg$value[df_rrProg$decision == decision]
}

pDie <- function(state, cycle){
  #state <- get("state", envir = parent.frame())  # Get x from the calling environment
  switch(state, "Moderate" = pMortMod,
         "Severe" = pMortSev, 
         "Dead" = 1)
  #df_pDie$value[df_pDie$state == state]
}

pProg <- function(state, decision){
  #decision <- get("decision", envir = parent.frame())  # Get x from the calling environment
  #state <- get("state", envir = parent.frame())  # Get x from the calling environment
  (state=="Moderate")*rrProg(decision)*pProgNoTrt
}

cost <- function(state){
  switch(state, "Moderate" = 3000,
         "Severe" = 6000,
         "Dead" = 0)
}

effectiveness <- function(state){
  switch(state, "Moderate" = 0.8,
         "Severe" = 0.6,
         "Dead" = 0)
}


# model design ==========
# must be unique events = verbs
#state_names <- c("Moderate", "Severe", "Dead")
#decision_names <- c("NoTrt", "TrtA", "TrtB")
#event_names <- c("Progress", "Die") # verbs

# df_rrProg <- data.frame(decision = c(decision_names), 
#                         value = c(1, rrProgTrtA, rrProgTrtB))
# df_rrProg
# 
# df_pDie <- data.frame(state = state_names, 
#                       value = c(pMortMod, pMortSev, 1))
# df_pDie
# simplest case 
# remainder prob, either Inf or prob_left()
# curr_state, either do curr_state() or "curr_state"


# You can continue adding more layers (+) to build the Markov model using this style.


# Markov Model ==========
mygmod <- gmod(model_type = "Markov", n_cycles = 3) + 
  decisions("NoTrt", "TrtA", "TrtB") + 
  states("Moderate", "Severe", "Dead") + 
  events("DIE", "PROGRESS") +
  discounts(payoffs = c("cost", "effectiveness"), discounts = c(0.015, 0.015)) + 
  initial_probs(states = c("Moderate", "Severe"), probs = c(0.5,0.5)) +  
  event_mapping(name = "DIE",  
            if_event = c(T, F), 
            goto = c("Dead", "PROGRESS"), 
            with_probs = c(pDie(state, cycle), Inf)) +
  event_mapping(name = "PROGRESS", 
            if_event = c(T, F), 
            goto = c("Severe", curr_state()), 
            #with_probs = c((state=="Moderate")*rrProg(decision)*pProgNoTrt, Inf))
            with_probs = c(pProg(state, decision), Inf)) + 
  payoffs(cost = cost(state), 
          effectiveness = effectiveness(state))
tunnel_states(mygmod)
is_cycle_dep(mygmod)
model_struc <- gmod_build(mygmod)
x <- model_struc$P$NoTrt
model_num_struc <- gmod_parse(model_struc, params = NULL)
model_res <- gmod_evaluate(model_num_struc)

print(model_res)







# Markov + age = f(n_cycles) ==========
pDie <- function(state, cycle){
  #state <- get("state", envir = parent.frame())  # Get x from the calling environment
  df_pDie$value[df_pDie$state == state]*sqrt(cycle)
}
mygmod <- gmod(model_type = "Markov", n_cycles = 5) + 
  decisions("NoTrt", "TrtA", "TrtB") + 
  states("Moderate", "Severe", "Dead") + 
  events("DIE", "PROGRESS") +
  initial_probs(states = c("Moderate", "Severe"), probs = c(0.3, Inf)) +  
  event_mapping(name = "DIE",  
            if_event = c(T, F), 
            goto = c("Dead", "PROGRESS"), 
            with_probs = c(pDie(state, cycle), Inf)) +
  event_mapping(name = "PROGRESS", 
            if_event = c(T, F), 
            goto = c("Severe", curr_state()), 
            #with_probs = c((state=="Moderate")*rrProg(decision)*pProgNoTrt, Inf))
            with_probs = c(pProg(state, decision), Inf))

model_obj <- print(mygmod)


# Markov as function of tunnel ============
# without arguments because it gets determined by the local environment
rrProg <- function(decision){
  #decision <- get("decision", envir = parent.frame())  # Get x from the calling environment
  df_rrProg$value[df_rrProg$decision == decision]
}

pDie <- function(state, cycle_in_state){
  #state <- get("state", envir = parent.frame())  # Get x from the calling environment
  df_pDie$value[df_pDie$state == state]
}

pProg <- function(state, decision, cycle_in_state){
  #decision <- get("decision", envir = parent.frame())  # Get x from the calling environment
  #state <- get("state", envir = parent.frame())  # Get x from the calling environment
  (state=="Moderate")*rrProg(decision)*pProgNoTrt
}

mygmod <- gmod(model_type = "Markov", n_cycles = 3) + 
  decisions("NoTrt", "TrtA", "TrtB") + 
  states("Moderate", "Severe", "Dead") + 
  events("DIE", "PROGRESS") +
  initial_probs(states = "Moderate", probs = 1) +  
  event_mapping(name = "DIE",  
            if_event = c(T, F), 
            goto = c("Dead", "PROGRESS"), 
            with_probs = c(pDie(state, cycle_in_state('Severe')), Inf)) +
  event_mapping(name = "PROGRESS", 
            if_event = c(T, F), 
            goto = c("Severe", curr_state()), 
            #with_probs = c((state=="Moderate")*rrProg(decision)*pProgNoTrt, Inf))
            with_probs = c(pProg(state, decision, cycle_in_state('Moderate')), Inf))

model_obj <- print(mygmod)


# Time and tunnel dependencies ========
# without arguments because it gets determined by the local environment
rrProg <- function(decision){
  #decision <- get("decision", envir = parent.frame())  # Get x from the calling environment
  df_rrProg$value[df_rrProg$decision == decision]
}

pDie <- function(state, cycle){
  #state <- get("state", envir = parent.frame())  # Get x from the calling environment
  df_pDie$value[df_pDie$state == state]
}

pProg <- function(state, decision, cycle_in_state){
  #decision <- get("decision", envir = parent.frame())  # Get x from the calling environment
  #state <- get("state", envir = parent.frame())  # Get x from the calling environment
  (state=="Moderate")*rrProg(decision)*pProgNoTrt
}

mygmod <- gmod(model_type = "Markov", n_cycles = 3) + 
  decisions("NoTrt", "TrtA", "TrtB") + 
  states("Moderate", "Severe", "Dead") + 
  events("DIE", "PROGRESS") +
  initial_probs(states = "Moderate", probs = 1) +  
  event_mapping(name = "DIE",  
            if_event = c(T, F), 
            goto = c("Dead", "PROGRESS"), 
            with_probs = c(pDie(state, cycle), Inf)) +
  event_mapping(name = "PROGRESS", 
            if_event = c(T, F), 
            goto = c("Severe", curr_state()), 
            #with_probs = c((state=="Moderate")*rrProg(decision)*pProgNoTrt, Inf))
            with_probs = c(pProg(state, decision, cycle_in_state('Moderate')), Inf))

model_obj <- print(mygmod)
