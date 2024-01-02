

gmod_parse <- function(x, ...) UseMethod("gmod_parse")
gmod_parse.gmod_markov <- function(model_struc, params = NULL){
  if(is.null(params)){
    warning("No parameters were provided. Will use the parameters from the global environment. 
            If instead you want to evaluate the model with specific parameter values, please provide 
            them here as list.")
  } else {
    list2env(params)
  }
  # for Markov structure, parse P, p0, Payoffs, event_payoff and replace
  model_num_str <- model_struc
  decisions <- model_struc$decisions
  events <- model_struc$events
  event_with_payoffs_df <- model_struc$events_with_payoffs_df
  for (decision in decisions){
    model_num_str$p0[[decision]] <- parse_object(model_struc$p0[[decision]])
    model_num_str$P[[decision]] <- parse_object(model_struc$P[[decision]])
    for (payoff in model_struc$payoff_names){
      model_num_str$State_payoffs[[payoff]][[decision]] <- 
        parse_object(model_struc$State_payoffs[[payoff]][[decision]])
      # initialize overall payoffs as the state payoffs
      model_num_str$Overall_payoff[[payoff]][[decision]] <- model_num_str$State_payoffs[[payoff]][[decision]] 
      for (i in 1:nrow(event_with_payoffs_df)){
        event <- event_with_payoffs_df$event[i]
        event_value <- event_with_payoffs_df$values[i]
        event_payoff <- model_struc$Event_payoff[[payoff]][[event]][[event_value]][[decision]]
        if (!is.null(event_payoff)){ # only a single type of payoff may be available
          model_num_str$Event_payoff[[payoff]][[event]][[event_value]][[decision]] <- 
            parse_object(event_payoff)
          model_num_str$Overall_payoff[[payoff]][[decision]] <-
            model_num_str$Overall_payoff[[payoff]][[decision]] + #overall payoffs
            model_num_str$Event_payoff[[payoff]][[event]][[event_value]][[decision]] #event payoffs
        } # end is null event_payoff
      } # end rows of events with payoffs
      
    } # end payoffs
  } # end decisions
  #for (event in events){
  #  model_num_str$event_prop[[event]] <- parse_object(model_struc$event_prop[[event]])
  #} # end event
  class(model_num_str) <- "gmod_markov"
  return(model_num_str)
}


gmod_evaluate <- function(x, ...) UseMethod("gmod_evaluate")
gmod_evaluate.gmod_markov <- function(model_num_struc){
  # evaluate based on the objects and create a list
  model_results <- model_num_struc #list()
  decisions <- model_num_struc$decisions
  n_decisions <- model_num_struc$n_decisions
  n_cycles <- model_num_struc$n_cycles
  cycles <- 1:n_cycles
  states <- model_num_struc$states
  n_states <- model_num_struc$n_states
  states_expanded <- model_num_struc$states_expanded
  n_states_expanded <- model_num_struc$n_states_expanded
  payoffs <- model_num_struc$payoff_names
  n_payoffs <- model_num_struc$n_payoffs
  discounts <- model_num_struc$discounts # a named vector with discount rates
  if (is.null(discounts)){
    warning("A discount was not provided. Will assume no discount.  To apply a discount, please add discount() to your gmod model")
    discounts <- rep(1, n_payoffs)
    names(discounts) <- payoffs
  }
  is_cycle_dep <- model_num_struc$is_cycle_dep
  
  for (decision in decisions){
    Trace <- matrix(NA, nrow = n_cycles, ncol = n_states_expanded, dimnames = list(cycles, states_expanded))
    Trace[1, ] <- model_num_struc$p0[[decision]]
    for (i in 2:n_cycles){
      if (is_cycle_dep){ # use array syntax
        P <- model_num_struc$P[[decision]][,,i]
      } else { #use matrix syntax
        P <- model_num_struc$P[[decision]]
      }
      Trace[i, ] <- Trace[i-1, ] %*% P
    }
    model_results$Trace[[decision]] <- Trace
  } # end decision
  
  # Add payoffs multiplied by traces '
  cycle_ones <- matrix(1,nrow = n_cycles, ncol = 1)
  state_ones <- matrix(1,nrow = 1, ncol = n_states_expanded)
  mat_summary <- matrix(0, nrow = n_decisions, ncol = n_payoffs, dimnames = list(decisions, payoffs))
  for (decision in decisions){
    for (payoff in payoffs){
      mat_discounts <- matrix(1/(1+discounts[payoff])^(cycles-1), ncol = 1) %*% state_ones
      Payoff <- model_num_struc$Overall_payoff[[payoff]][[decision]] 
      if (!is_cycle_dep){ # if model is not cycle dependent, we will get a single vector of values
        Payoff <- cycle_ones %*% matrix(Payoff, nrow = 1)
      } 
      R <- model_results$Trace[[decision]] * Payoff * mat_discounts
      model_results$Results[[payoff]][[decision]] <- R
      mat_summary[decision, payoff] <- sum(R)
    } # end payoff
    
  } # end decision
  model_results$Summary <- mat_summary
  class(model_results) <- "gmod_markov"
  return(model_results)
}


