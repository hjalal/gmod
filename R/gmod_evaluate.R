#' Runs a decision tree or Markov model
#' @description runs the decision tree or markov model and returns the results either traces and summary results
#' @param model_num_struc a matrix containing the numerical gmod object from the gmod_parse() function
#'
#' @return
#' @export
#'
#' @examples gmod_evaluate(numerical_model_structure)
gmod_evaluate <- function(x, ...) UseMethod("gmod_evaluate")


#' Runs the markov model
#' @description runs the markov model and returns the traces and summary results
#' @param model_num_struc a matrix containing the numerical gmod object from the gmod_parse() function
#' @export
#' 
#' @return
#'
#' @examples gmod_evaluate(numerical_model_structure)
gmod_evaluate.gmod_markov <- function(model_struc, params = NULL){
  model_num_struc <- gmod_parse(model_struc, params = params)
  # evaluate based on the objects and create a list
  model_results <- model_num_struc #list()
  
  
  
  decisions <- model_num_struc$decisions
  n_decisions <- model_num_struc$n_decisions
  n_cycles <- model_num_struc$n_cycles
  cycles <- 1:n_cycles
  cycle_names <- paste0("cycle_", cycles)
  states <- model_num_struc$states
  n_states <- model_num_struc$n_states
  states_expanded <- model_num_struc$states_expanded
  n_states_expanded <- model_num_struc$n_states_expanded
  payoff_names <- model_num_struc$payoff_names
  n_payoffs <- model_num_struc$n_payoffs
  discounts <- model_num_struc$discounts # a named vector with discount rates
  
  if (is.null(discounts)){
    warning("A discount was not provided. Will assume no discount.  To apply a discount, please add discount() to the gmod")
    discounts <- rep(1, n_payoffs)
    names(discounts) <- payoffs
  }
  is_cycle_dep <- model_num_struc$is_cycle_dep
  
  # construct P 
  model_results$P <- construct_P(model_num_struc)
  

  for (decision in decisions){
    Trace <- matrix(NA, nrow = n_cycles, ncol = n_states_expanded, dimnames = list(cycles, states_expanded))
    Trace[1, ] <- model_num_struc$p0[[decision]]
    for (i in 2:n_cycles){
      if (is_cycle_dep){ # use array syntax
        P <- model_results$P[[decision]][,,i]
      } else { #use matrix syntax
        P <- model_results$P[[decision]]
      }
      Trace[i, ] <- Trace[i-1, ] %*% P
    }
    model_results$Trace[[decision]] <- Trace
  } # end decision
  
  # Add payoffs multiplied by traces '
  model_results$Payoff <- construct_Payoff(model_num_struc)  
  cycle_ones <- matrix(1,nrow = n_cycles, ncol = 1)
  state_ones <- matrix(1,nrow = 1, ncol = n_states_expanded)
  mat_summary <- matrix(0, nrow = n_decisions, ncol = n_payoffs, dimnames = list(decisions, payoff_names))
  for (decision in decisions){
    for (payoff_name in payoff_names){
      mat_discounts <- matrix(1/(1+discounts[payoff_name])^(cycles-1), ncol = 1) %*% state_ones
      Payoff <- model_results$Payoff[[payoff_name]][[decision]] 
      if (!is_cycle_dep){ # if model is not cycle dependent, we will get a single vector of values
        Payoff <- cycle_ones %*% Payoff
      } 
      R <- model_results$Trace[[decision]] * Payoff * mat_discounts
      model_results$Results[[payoff_name]][[decision]] <- R
      mat_summary[decision, payoff_name] <- sum(R)
    } # end payoff
    
  } # end decision
  model_results$Summary <- mat_summary
  class(model_results) <- "gmod_markov"
  return(model_results)
}


construct_P <- function(model_num_struc){
  markov_eqns <- model_num_struc$markov_eqns
  is_cycle_dep <- model_num_struc$is_cycle_dep
  decisions <- model_num_struc$decisions
  n_cycles <- model_num_struc$n_cycles
  cycles <- 1:n_cycles
  cycle_names <- paste0("cycle_", cycles)
  # states <- model_num_struc$states
  # n_states <- model_num_struc$n_states
  states_expanded <- model_num_struc$states_expanded
  n_states_expanded <- model_num_struc$n_states_expanded
  P <- list()
  for (decision in decisions){
    if (is_cycle_dep){
      P[[decision]] <- array(0, dim = c(n_states_expanded, n_states_expanded, n_cycles), 
                             dimnames = list(states_expanded, states_expanded, cycle_names))
    } else {
      P[[decision]] <- matrix(0, nrow = n_states_expanded, ncol = n_states_expanded, 
                              dimnames = list(states_expanded, states_expanded))
    }
  }
  # iterate through the equations and populate P
  for (i in 1:nrow(markov_eqns)){
    state <- markov_eqns$state_expanded[i]
    dest <- markov_eqns$dest_expanded[i]
    decision <- markov_eqns$decision[i]
    prob <- markov_eqns$probs[i]
    if (is_cycle_dep){
      cycle <- markov_eqns$cycle[i]
      P[[decision]][state, dest, cycle] <- prob
    } else {
      P[[decision]][state, dest] <- prob
    }
  }
  return(P)
}


construct_Payoff <- function(model_num_struc){
  markov_eqns <- model_num_struc$markov_eqns
  is_cycle_dep <- model_num_struc$is_cycle_dep
  decisions <- model_num_struc$decisions
  n_cycles <- model_num_struc$n_cycles
  cycles <- 1:n_cycles
  cycle_names <- paste0("cycle_", cycles)
  # states <- model_num_struc$states
  # n_states <- model_num_struc$n_states
  states_expanded <- model_num_struc$states_expanded
  n_states_expanded <- model_num_struc$n_states_expanded
  payoff_names <- model_num_struc$payoff_names
  Payoff <- list()
  for (payoff_name in payoff_names){
    for (decision in decisions){
      if (is_cycle_dep){ # use array syntax
        Payoff[[payoff_name]][[decision]] <- matrix(0, nrow = n_cycles, ncol = n_states_expanded, 
                                                                   dimnames = list(cycle_names, states_expanded))
      } else { #use matrix syntax
        Payoff[[payoff_name]][[decision]] <- matrix(0, nrow = 1, ncol = n_states_expanded, 
                                                                   dimnames = list("", states_expanded))
      }
    }
  }
  payoff_eqns <- model_num_struc$payoff_eqns
  for (i in 1:nrow(payoff_eqns)){
    state <- payoff_eqns$state_expanded[i]
    decision <- payoff_eqns$decision[i]
    for (payoff_name in payoff_names){
      if (is_cycle_dep){
        cycle <- payoff_eqns$cycle[i]
        Payoff[[payoff_name]][[decision]][cycle, state] <- payoff_eqns[[payoff_name]][i]
      } else {
        Payoff[[payoff_name]][[decision]][1, state] <- payoff_eqns[[payoff_name]][i]
      }
    } # end payoff_name
  }
  return(Payoff)
}



#' Runs the decision tree
#' @description runs the decision model and returns the expected payoff values and summary results
#' @param model_num_struc a matrix containing the numerical gmod decision object from the gmod_parse() function
#' @export
#' 
#' @return
#'
#' @examples gmod_evaluate(numerical_model_structure)
gmod_evaluate.gmod_decision <- function(model_struc, params = NULL){
    if(is.null(params)){
      warning("No parameters were provided. Will use the parameters from the global environment. 
            If instead you want to evaluate the model with specific parameter values, please provide 
            them here as list.")
    } else {
      list2env(params)
    }
    model_results <- list()
    # for Decison structure, parse P and Payoffs 
    summary_formulae <- model_struc$summary_formulae
    payoff_names <- model_struc$payoff_names
    n <- nrow(summary_formulae)
    summary_results <- summary_formulae
    for (payoff_name in payoff_names){
      for (i in 1:n){
        summary_results[[payoff_name]][i] <- eval(parse(text = summary_formulae[[payoff_name]][i]))
      }
    }
    model_results$summary_results <- summary_results
    class(model_results) <- "gmod_decision"

  return(model_results)
}