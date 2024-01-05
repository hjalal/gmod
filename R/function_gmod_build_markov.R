

add_markov_info <- function(gmod_obj, model_obj){
  # retrieve states layer
  states_layer <- retrieve_layer_by_type(gmod_obj, type = "states")
  states <- states_layer$states# no tunnel states
  n_cycles <- gmod_obj$n_cycles
  
  tunnel_states <- model_obj$tunnel_states
  if (length(tunnel_states) > 0){ #there are tunnel states
    #for each tunnel state expand states vector
    is_tunnel <- states %in% tunnel_states # F,T,F since severe is the tunnel state 
    # now we need to expand the state space to incorporate the tunnels and all other vectors
    rep_states <- is_tunnel * (n_cycles - 1) + 1 # {1, 40, 1} a trick to get number of replication for each state
    states_expanded <- rep(states, rep_states)
    for (tunnel_state in tunnel_states){
      states_expanded[states_expanded == tunnel_state] <- paste0(tunnel_state, "_tnl", 1:n_cycles) # we need to replace severe with Severe Yr1, Severe Yr2, ... etc
    }
  } else {
    states_expanded <- states
  }
  model_obj$states <- states
  model_obj$n_states <- length(states)
  model_obj$states_expanded <- states_expanded
  model_obj$n_states_expanded <- length(states_expanded)    
  return(model_obj)
}

add_markov_initial_probs <- function(gmod_obj, model_obj){
  # all prob are 0, just replace the ones provided with their values
  markov_p0 <- retrieve_layer_by_type(gmod_obj, type = "initial_prob")
  init_p0 <- markov_p0$probs
  init_states <- markov_p0$states
  tunnel_states <- model_obj$tunnel_states
  p0 <- rep(0, model_obj$n_states_expanded)  # empty vector
  names(p0) <- model_obj$states_expanded
  
  for (d in model_obj$decisions){
    model_obj$p0[[d]] <- p0
    for (i in 1:length(init_p0)){
      state <- init_states[i]
      if (state %in% tunnel_states){
        state <- paste0(state, "_tnl1")
      }
      model_obj$p0[[d]][state] <- init_p0[i]
    }
    model_obj$p0[[d]] <- check_prob_vector(model_obj$p0[[d]])
  }
  return(model_obj)
}


add_markov_transition_eqns <- function(gmod_obj, model_obj, events_df){
  states <- model_obj$states
  #n_states <- model_obj$n_states
  states_expanded <- model_obj$states_expanded
  n_states_expanded <- length(states_expanded)
  tunnel_states <- model_obj$tunnel_states
  n_cycles <- gmod_obj$n_cycles
  # get expanded states 
  
  # add "curr_state" ones as well here to form P_raw as a matrix
  for (decision in model_obj$decisions){
    if (model_obj$is_cycle_dep){ # array
      n_cycles <- gmod_obj$n_cycles
      cycle <- 1:n_cycles
      #cycle <- cycle_range # try vector format!
      cycles <- paste0("cycle", cycle)
      # change n_states to n_states_expanded
      model_obj$P[[decision]] <- array("0", dim = c(n_states_expanded, n_states_expanded, n_cycles), 
                                       dimnames = list(states_expanded, states_expanded, cycles))
    } else { # matrix
      model_obj$P[[decision]] <- matrix("0", nrow = n_states_expanded, ncol = n_states_expanded, 
                                        dimnames = list(states_expanded, states_expanded))
    }
    
    for (dest_expanded in states_expanded){
      dest_comp <- tunnel2state(dest_expanded)
      dest <- dest_comp[1]
      dest_idx <- as.integer(dest_comp[2])
      for (state_expanded in states_expanded){
        state_comp <- tunnel2state(state_expanded)
        state <- state_comp[1]
        state_idx <- as.integer(state_comp[2])
        
        p_trans_formula <- get_prob_chain(gmod_obj, events_df, end_state = dest)
        #if (p_trans_formula == "()") {p_trans_formula <- "0"}
        if (state == dest){
          p_stay_formula <- get_prob_chain(gmod_obj, events_df, end_state = "curr_state")
          p_trans_formula <- paste0(p_trans_formula, "+", p_stay_formula)
        }
        p_trans_formula <- gsub("\\bstate\\b", paste0("'", state, "'"), p_trans_formula)
        p_trans_formula <- gsub("\\bdecision\\b", paste0("'", decision, "'"), p_trans_formula)
        
        # write error trap if state within cycle_in_state doesn't match state
        p_trans_formula <- gsub("cycle_in_state\\([^)]+\\)", paste0("cycle_in_state=",state_idx), p_trans_formula)
        
        if (!(state %in% tunnel_states & state == dest & dest_idx != state_idx+1)){ #otherwise keep at "0"
          if (model_obj$is_cycle_dep){ # array
            p_trans_formula <- sapply(cycle, function(x) gsub("\\bcycle\\b", paste0("cycle=",x), p_trans_formula))
            model_obj$P[[decision]][state_expanded, dest_expanded, ] <- p_trans_formula
          } else {
            model_obj$P[[decision]][state_expanded, dest_expanded] <- p_trans_formula
          }
        }
      } # end origin state
    } # end dest state 
  } # end decision
  return(model_obj)
}




add_markov_payoff_eqns <- function(gmod_obj, model_obj, events_df){
  states <- model_obj$states
  #n_states <- model_obj$n_states
  states_expanded <- model_obj$states_expanded
  n_states_expanded <- length(states_expanded)
  tunnel_states <- model_obj$tunnel_states
  n_cycles <- gmod_obj$n_cycles
  payoffs <- model_obj$payoffs
  payoff_names <- names(payoffs)
  model_obj$State_payoffs <- list()
  # add "curr_state" ones as well here to form P_raw as a matrix
  for (payoff in payoff_names){
    for (decision in model_obj$decisions){
      if (model_obj$is_cycle_dep){ # matrix of rewards states x cycle
        n_cycles <- gmod_obj$n_cycles
        cycle <- 1:n_cycles
        #cycle <- cycle_range # try vector format!
        cycles <- paste0("cycle", cycle)
        # change n_states to n_states_expanded
        model_obj$State_payoffs[[payoff]][[decision]] <- matrix("0", nrow=n_cycles, ncol = n_states_expanded, 
                                                                dimnames = list(cycles, states_expanded))
      } else { # a single vector 
        model_obj$State_payoffs[[payoff]][[decision]] <- rep("0", n_states_expanded)
        names(model_obj$State_payoffs[[payoff]][[decision]]) <- states_expanded
      }
      
      for (state_expanded in states_expanded){
        state_comp <- tunnel2state(state_expanded)
        state <- state_comp[1]
        state_idx <- as.integer(state_comp[2])
        
        payoff_formula <- paste0(deparse(payoffs[[payoff]]), collapse = "")
        payoff_formula <- gsub("\\bstate\\b", paste0("'", state, "'"), payoff_formula)
        payoff_formula <- gsub("\\bdecision\\b", paste0("'", decision, "'"), payoff_formula)
        # write error trap if state within cycle_in_state doesn't match state
        payoff_formula <- gsub("cycle_in_state\\([^)]+\\)", paste0("cycle_in_state=",state_idx), payoff_formula)
        
        if (model_obj$is_cycle_dep){ # matrix
          payoff_formula <- sapply(cycle, function(x) gsub("\\bcycle\\b", paste0("cycle=",x), payoff_formula))
          model_obj$State_payoffs[[payoff]][[decision]][, state_expanded] <- payoff_formula
        } else {
          model_obj$State_payoffs[[payoff]][[decision]][state_expanded] <- payoff_formula
        }
        
      } # end origin state
    } # end decision
  } # end payoff
  return(model_obj)
}

#proportion of events by decision state and cycle if available
add_event_prop_eqns <- function(gmod_obj, model_obj, events_df, events_with_payoffs_df){
  states <- model_obj$states
  #n_states <- model_obj$n_states
  states_expanded <- model_obj$states_expanded
  n_states_expanded <- length(states_expanded)
  tunnel_states <- model_obj$tunnel_states
  n_cycles <- gmod_obj$n_cycles
  #events <- model_obj$events
  #n_events <- model_obj$n_events
  # get expanded states 
  payoffs <- model_obj$payoffs
  payoff_names <- names(payoffs)
  
  # add events here
  for (i in 1:nrow(events_with_payoffs_df)){
    # get the events and values that have payoffs
    event <- events_with_payoffs_df$event[i]
    event_value <- events_with_payoffs_df$values[i]
    
    for (payoff in payoff_names){
      if (payoff %in% colnames(events_with_payoffs_df)){
        event_payoff_input <- events_with_payoffs_df[i,payoff]
        if (!is.na(event_payoff_input) & event_payoff_input!=0 & event_payoff_input!="NA"){
          for (decision in model_obj$decisions){
            
            # initialization based on cycle dependence   
            
            if (model_obj$is_cycle_dep){ # array
              n_cycles <- gmod_obj$n_cycles
              cycle <- 1:n_cycles
              #cycle <- cycle_range # try vector format!
              cycles <- paste0("cycle", cycle)
              # change n_states to n_states_expanded
              model_obj$Event_payoff[[payoff]][[event]][[event_value]][[decision]] <- matrix("0", nrow = n_cycles, ncol = n_states_expanded, 
                                                                                             dimnames = list(cycles, states_expanded))
            } else { # matrix
              model_obj$Event_payoff[[payoff]][[event]][[event_value]][[decision]] <- rep("0", n_states_expanded)
              names(model_obj$Event_payoff[[payoff]][[event]][[event_value]][[decision]]) <- states_expanded
            }
            
            # iterate for each state
            for (state_expanded in states_expanded){
              state_comp <- tunnel2state(state_expanded)
              state <- state_comp[1]
              state_idx <- as.integer(state_comp[2])
              
              # end events and values
              # exclude rows where selected event has other values 
              sel_events_df <- events_df[!(events_df$event == event & events_df$value != event_value),]
              end_state <- sel_events_df$results[sel_events_df$event == event]
              if (length(end_state) != 1){
                stop(paste("results of event",event,"is none or not unique:", end_state))
              }
              p_event_formula <- get_prob_chain(gmod_obj, sel_events_df, end_state = end_state)
              # will also allow for event payoffs to also be functions of state, decision, cycle 
              p_event_formula <- paste0("(", event_payoff_input, ")*(", p_event_formula, ")")
              p_event_formula <- gsub("\\bdecision\\b", paste0("'",decision,"'"), p_event_formula)
              p_event_formula <- gsub("\\bstate\\b", paste0("'", state, "'"), p_event_formula)
              
              # write error trap if state within cycle_in_state doesn't match state
              p_event_formula <- gsub("cycle_in_state\\([^)]+\\)", paste0("cycle_in_state=",state_idx), p_event_formula)
              
              #if (!(state %in% tunnel_states & state == dest & dest_idx != state_idx+1)){ #otherwise keep at "0"
              if (model_obj$is_cycle_dep){ # array
                p_event_formula <- sapply(cycle, function(x) gsub("\\bcycle\\b", paste0("cycle=",x), p_event_formula))
                model_obj$Event_payoff[[payoff]][[event]][[event_value]][[decision]][, state_expanded] <- p_event_formula
              } else {
                model_obj$Event_payoff[[payoff]][[event]][[event_value]][[decision]][state_expanded] <- p_event_formula
              }
              #}
              
            } # end origin state
          } # end decision
        } # end indivdual payoff condition that is not 0
      } # end payoff column condition
    } # end payoffs
  } # end rows for events with payoffs
  
  return(model_obj)
}

# gmod 

tunnel_states <- function(gmod_obj){
  #gmod_obj$layers
  events_df <- get_event_df(gmod_obj)
  payoffs_layer <- retrieve_layer_by_type(gmod_obj, type = "payoffs")
  payoffs_str <- sapply(payoffs_layer$payoffs, my_deparse)
  # capture all tunnel states 
  matches_transitions <- str_match_all(events_df$probs, 'cycle_in_state\\("(.*?)"\\)')
  tunnel_transitions <- lapply(matches_transitions, function(x) x[2])
  # Convert the list to a vector if needed
  matches_payoffs <- str_match_all(payoffs_str, 'cycle_in_state\\("(.*?)"\\)')
  tunnel_payoffs <- lapply(matches_payoffs, function(x) x[2])
  
  # combine tunnel states from both payoffs and transitions
  tunnel_states <- unique(c(unlist(tunnel_transitions), unlist(tunnel_payoffs)))
  tunnel_states <- tunnel_states[!is.na(tunnel_states)]
  return(tunnel_states)
}


is_cycle_dep <- function(gmod_obj){
  #gmod_obj$layers
  events_df <- get_event_df(gmod_obj)
  payoffs <- retrieve_layer_by_type(gmod_obj, type = "payoffs")
  transitions_cycle_dep <- any(grepl("\\bcycle\\b", events_df$probs)) 
  payoffs_cycle_dep <- any(grepl("\\bcycle\\b", payoffs$payoffs)) 
  any(transitions_cycle_dep, payoffs_cycle_dep)
}
