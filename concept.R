# Concept: 
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
states <- c("Moderate", "Severe", "Dead")
decisions <- c("TrtA", "TrtB")
events <- c("Progress", "Die") # verbs

# Usage of gmod_markov function
# the sequence is not important ... 
# can either refer to states by "string" or by state(variable) which returns the state name
# events = verbs, states are adjectives/nouns
mygmod <- gmod() + 
  # these sequences indicate that decisions come first, then markov states then events... 
  decisions("NoTrt", "TrtA", "TrtB") + 
  states("Moderate", "Severe", "Dead") +  # write routine to detect mis-named or miscalculated names
  events("PROGRESS", "DIE") +
  initial_probs(states = "Moderate", values = 1) +  # the rest will be assumed 0, or if one left it is assumed to be 1-sum(others)
  # event_mappings, the most powerful becuase it will allow using previous values, parameters...etc
  # probs can infer a scalar, a function or a table. - function is changed to a table discretized ... 
  # current_state(), remainder() and decision() are variables that can be used internally 
  # other variables are curr_cycle() and curr_state_cycle() 
  # previous event() 
  event_mapping(name = "DIE",  
            values = c(T, F), 
            outcomes = c("Dead", "PROGRESS"), 
            # probs - can also ignore the last entry - assumed = remainder()
            probs = c(f(curr_state), prob_left())) +
  event_mapping(name = "PROGRESS", 
            values = c(T, F), 
            outcomes = c("Severe", stay()), 
            probs = c(f(current_state(), decision()), prob_left()))

mygmod
