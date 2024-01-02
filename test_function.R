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
        model_obj$event_payoff[[payoff]][[event]][[event_value]][[decision]] <- matrix("0", nrow = n_cycles, ncol = n_states_expanded, 
                                                                           dimnames = list(cycles, states_expanded))
      } else { # matrix
        model_obj$event_payoff[[payoff]][[event]][[event_value]][[decision]] <- rep("0", n_states_expanded)
        names(model_obj$event_payoff[[payoff]][[event]][[event_value]][[decision]]) <- states_expanded
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
        
        # p_trans_formula <- get_prob_chain(gmod_obj, events_df, end_state = dest)
        # #if (p_trans_formula == "()") {p_trans_formula <- "0"}
        # if (state == dest){
        #   p_stay_formula <- get_prob_chain(gmod_obj, events_df, end_state = "curr_state")
        #   p_trans_formula <- paste0(p_trans_formula, "+", p_stay_formula)
        # }
        
        p_event_formula <- gsub("\\bstate\\b", paste0("'", state, "'"), p_event_formula)

        # write error trap if state within cycle_in_state doesn't match state
        p_event_formula <- gsub("cycle_in_state\\([^)]+\\)", paste0("cycle_in_state=",state_idx), p_event_formula)
        

        #if (!(state %in% tunnel_states & state == dest & dest_idx != state_idx+1)){ #otherwise keep at "0"
          if (model_obj$is_cycle_dep){ # array
            p_event_formula <- sapply(cycle, function(x) gsub("\\bcycle\\b", paste0("cycle=",x), p_event_formula))
            model_obj$event_payoff[[payoff]][[event]][[event_value]][[decision]][state_expanded, ] <- p_event_formula
          } else {
            model_obj$event_payoff[[payoff]][[event]][[event_value]][[decision]][state_expanded] <- p_event_formula
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