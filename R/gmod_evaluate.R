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




#' Title
#'
#' @param model_struc 
#' @param model_function_name 
#' @param print_model_function 
#'
#' @return
#' @export
#'
#' @examples
gmod_evaluate.gmod_decision <- function(model_struc, model_function_name = "my_dec_model", print_model_function = FALSE){
  # build model as a vector of strings 
  model_lines <- paste0(model_function_name, "<- function(params){")
  model_lines <- c(model_lines, "list2env(params)")
  model_lines <- c(model_lines, paste0("summary_results <- data.frame(decision=c('", paste0(model_struc$decision, collapse = "','"), "'))"))
  # for Decison structure, parse P and Payoffs 
  summary_formulae <- model_struc$summary_formulae
  payoff_names <- model_struc$payoff_names
  n <- nrow(summary_formulae)
  for (payoff_name in payoff_names){
    for (i in 1:n){
      model_lines <- c(model_lines, paste0("summary_results[['", payoff_name, "']][",i, "] <- ", summary_formulae[[payoff_name]][i]))
    }
  }
  model_lines <- c(model_lines, "return(summary_results)")
  model_lines <- c(model_lines, "}")
  
  model_string <- paste(model_lines, collapse = "\n")
  
  if (print_model_function){
    cat(model_string)
  }
  new_func <- eval(parse(text = model_string)) # generates the function
  # Assign the new function to the global environment
  assign(model_function_name, new_func, envir = .GlobalEnv)
  cat(paste0("\n\n\033[94mNote:Model function ", model_function_name, " is generated. It can be run by calling it directly:\n", model_function_name, "(params)\033[0m\n"))
  return(TRUE)
}

