# simple GMOD model ========
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
  df_rrProg$value[df_rrProg$decision == decision]
}

pDie <- function(state){
  #state <- get("state", envir = parent.frame())  # Get x from the calling environment
  df_pDie$value[df_pDie$state == state]
}

pProg <- function(state, decision){
  #decision <- get("decision", envir = parent.frame())  # Get x from the calling environment
  #state <- get("state", envir = parent.frame())  # Get x from the calling environment
  (state=="Moderate")*rrProg(decision)*pProgNoTrt
}


# model design ==========
# must be unique events = verbs
state_names <- c("Moderate", "Severe", "Dead")
decision_names <- c("NoTrt", "TrtA", "TrtB")
event_names <- c("Progress", "Die") # verbs

df_rrProg <- data.frame(decision = c(decision_names), 
                     value = c(1, rrProgTrtA, rrProgTrtB))
df_rrProg

df_pDie <- data.frame(state = state_names, 
                   value = c(pMortMod, pMortSev, 1))
df_pDie
# simplest case 
# remainder prob, either Inf or prob_left()
# curr_state, either do curr_state() or "curr_state"


# You can continue adding more layers (+) to build the Markov model using this style.

# Markov Model ==========
mygmod <- gmod(model_type = "Markov") + 
  decisions("NoTrt", "TrtA", "TrtB") + 
  states("Moderate", "Severe", "Dead") + 
  events("DIE", "PROGRESS") +
  initial_probs(states = state_names, probs = c(1,0,0)) +  
  add_event(name = "DIE",  
            if_event = c(T, F), 
            goto = c("Dead", "PROGRESS"), 
            with_probs = c(pDie(state), Inf)) +
  add_event(name = "PROGRESS", 
            if_event = c(T, F), 
            goto = c("Severe", curr_state()), 
            #with_probs = c((state=="Moderate")*rrProg(decision)*pProgNoTrt, Inf))
            with_probs = c(pProg(state, decision), Inf))


# Decision Tree ========
## rock climber example ======= 
pDie <- function(decision){
  if (decision == "Amputate") 0.99 else 0.8
}
pDie("Amputate")
pDie("Antibiotics")

mygmod <- gmod(model_type = "Decision") + 
  decisions("Amputate", "Antibiotics") + 
  outcomes("Dead", "Alive") +
  events("DIE") + 
  add_event(name = "DIE",  
            if_event = c(T, F), 
            goto = c("Dead", "Alive"), 
            with_probs = c(pDie(decision), Inf)) 


gmod_obj <- print(mygmod)


## HVE/OVE example ========
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
p_comp <- function(decision, prev_event){
  if (decision == "DoNotTreat" & any(prev_event == "HVE") ) return(p_HVE_comp)
  if (decision == "DoNotTreat" & any(prev_event== "OVE") )return(p_OVE_comp)
  if (decision == "Treat" & any(prev_event== "HVE") ) return(p_HVE_comp_tx)
  if (decision == "Treat" & any(prev_event== "OVE") ) return(p_OVE_comp_tx)
  if (decision == "Biopsy" & any(prev_event== "HVE")) return(p_HVE_comp_tx)
  if (decision == "Biopsy" & any(prev_event== "OVE")) return(p_OVE_comp  )
}
p_comp(decision = "Biopsy", prev_event = "OVE")
c_HVE <- function(decision){
  if (decision == "biopsy") c_tx else 0
}

mygmod <- gmod(model_type = "Decision", payoffs = c("cost", "effectiveness")) + 
  decisions(names = c("DoNotTreat", "Treat", "Biopsy"), 
            payoffs = list(cost = c(0, c_tx, c_biopsy), 
                    effectiveness = c(0, 0, -q_loss_biopsy))) + 
  outcomes(names = c("Death", "HVE_comp", "no_HVE_comp", "OVE_comp", "no_OVE_comp"), 
           payoffs= list(cost = c(0, c_VE_comp, c_VE),
                    effectiveness = c(q_death_biopsy, q_VE_comp, q_VE))) +
  events("DIE", "HVE","get_comp") + 
  add_event(name = "DIE",  
            if_event = c(T, F), 
            goto = c("Death", "HVE"), 
            with_probs = c(pDie(decision), Inf)) + 
  add_event(name = "HVE",  
            if_event = c(T, F), 
            goto = c("get_HVE_comp", "get_OVE_comp"), 
            with_probs = c(p_HVE, Inf)) +
  add_event(name = "get_HVE_comp", 
            if_event = c(T, F),
            goto = c("HVE_comp", "no_HVE_comp"),
            with_prob = c(p_comp(decision, "HVE"), Inf))  +
  add_event(name = "get_OVE_comp", 
            if_event = c(T, F),
            goto = c("OVE_comp", "no_OVE_comp"),
            with_prob = c(p_comp(decision, "OVE"), Inf)) 

print(mygmod)
#gmod_obj <- mygmod

retrieve_obj_type(gmod_obj, "Dead")
retrieve_obj_type(gmod_obj, "DIE")

# ====
# logic to create chained probabilities

# start from an event that is not listed in the "then" of 
# other events
events_df <- get_event_df(mygmod)
first_event <- get_first_event(events_df)
gmod_obj$states

end_state <- "Severe"
get_prob_chain(gmod_obj, events_df, end_state = "Severe")
get_prob_chain(gmod_obj, events_df, end_state = "Dead")
get_prob_chain(gmod_obj, events_df, end_state = "Moderate")
get_prob_chain(gmod_obj, events_df, end_state = "curr_state")



# iterate through each "then" 

# if the "then" is a state then 
# do a transition prob to that state

# if the "then" is another event, go 
# find that "event" and go through it's 
# "then" statements until all events are 
# exhausted

# if these events lead to multiple 
# chains from an origin state and destinatino state
# then add them all up.