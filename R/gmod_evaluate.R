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


#' Runs the decision tree
#' @description runs the decision model and returns the expected payoff values and summary results
#' @param model_num_struc a matrix containing the numerical gmod decision object from the gmod_parse() function
#' @export
#' 
#' @return
#'
#' @examples gmod_evaluate(numerical_model_structure)
gmod_evaluate.gmod_decision <- function(model_num_struc){
  payoffs <- model_num_struc$payoff_names
  decisions <- model_num_struc$decisions
  n_payoffs <- model_num_struc$n_payoffs
  n_decisions <- model_num_struc$n_decisions
  events <- model_num_struc$events
  model_results <- model_num_struc #list()
  mat_outcome_payoffs <- matrix(0, nrow = n_decisions, ncol = n_payoffs, dimnames = list(decisions, payoffs))
  
  for (decision in decisions){
    for (payoff in payoffs){
      mat_outcome_payoffs[decision, payoff] <- 
        matrix(model_num_struc$Outcome_payoffs[[payoff]][[decision]], nrow = 1) %*% 
        matrix(model_num_struc$P[[decision]], ncol = 1)
    } # end payoffs
  } # end decision
  model_results$Outcome_payoff_summary <- mat_outcome_payoffs
  
  # Event payoffs 
  mat_event_payoffs <- matrix(0, nrow = n_decisions, ncol = n_payoffs, 
                              dimnames = list(decisions, payoffs))
  
  for (payoff in payoffs){
    event_payoffs <- model_num_struc$Event_payoffs[[payoff]] #[[event]]
    if (!is.null(event_payoffs)){
      temp_mat <- 0
      for (event in events){
        # compute event rewards = p_event*event_payoff
        temp_mat <- temp_mat + model_num_struc$event_prop[[event]] * 
          model_num_struc$Event_payoffs[[payoff]][[event]]
      } # end event
      mat_event_payoffs[,payoff] <- colSums(temp_mat)
    }
  } # end payoffs
  if (sum(mat_event_payoffs)!=0){
    model_results$Event_payoff_summary <- mat_event_payoffs
  }
  model_results$Overall_summary <- mat_outcome_payoffs + mat_event_payoffs
  return(model_results)
}