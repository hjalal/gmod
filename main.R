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


# model design ==========
# must be unique events = verbs
state_names <- c("Moderate", "Severe", "Dead")
decision_names <- c("TrtA", "TrtB")
event_names <- c("Progress", "Die") # verbs

# simplest case 
# remainder prob, either Inf or prob_left()
# curr_state, either do curr_state() or "curr_state"

# Usage example in a ggplot-like style with '+'
my_model <- gmod() +
  decisions("NoTrt", "TrtA", "TrtB") +
  markov_states("Moderate", "Severe", "Dead") +
  events("DIE", "PROGRESS") +
  initial_probs(states = c("Moderate", "Severe", "Dead"), probs = c(1, 0, 0)) +
  add_event(name = "DIE", if_event = c(TRUE, FALSE), then = c("Dead", "PROGRESS"), with_probs = c(0.1, Inf)) +
  add_event(name = "PROGRESS", if_event = c(TRUE, FALSE), then = c("Severe", "Moderate"), with_probs = c(0.2, Inf))

# You can continue adding more layers (+) to build the Markov model using this style.


mygmod <- gmod() + 
  decisions("NoTrt", "TrtA", "TrtB") + 
  markov_states("Moderate", "Severe", "Dead") + 
  events("DIE", "PROGRESS") +
  initial_probs(states = state_names, probs = c(1,0,0)) +  
  add_event(name = "DIE",  
            if_event = c(T, F), 
            then = c("Dead", "PROGRESS"), 
            with_probs = c(0.1, Inf)) +
  add_event(name = "PROGRESS", 
            if_event = c(T, F), 
            then = c("Severe", curr_state()), 
            with_probs = c(0.2 * is_curr_state("Moderate"), Inf))

mygmod

construct_prob_vec(x = c("A", "B", "C"), with_probs = c(0.1, 0.1, Inf))



mygmod$layers[[1]]

lengths(mygmod$layers[[2]])
convert_events_to_df(mygmod)
check_element_lengths(mygmod$layers[[2]])
max()
# Example list
original_list <- list(
  name = "Die",
  from = c("Moderate", "Severe"),
  to = "Dead",
  values = c(0.2, 0.5)
)

# Expand the original list
expanded <- expand_list(original_list)

# Display the expanded list
print(expanded)

