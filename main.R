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


mygmod <- gmod() + 
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
            with_probs = c(pProg(state, decision), Inf))

gmod_obj <- print(mygmod)
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