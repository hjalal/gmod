#' Builds a Markov model
#'
#' @param gmod_obj 
#'
#' @return a gmod object containing the equations for the Markov model
#' @export
#' 
#' @examples 
#' gmod_build(mygmod)
gmod_build <- function(x, ...) UseMethod("gmod_build")

#' Build a Markov model structure
#'
#' @param gmod_obj 
#' @description takes in a gmod formula structure and returns a gmod numerical formula structure 
#' @return gmod structure containing formulae strings
#' @export
#'
#' @examples gmod_build(mygmod)
gmod_build.gmod_markov <- function(gmod_obj, params = NULL, simplify = FALSE){
  if (simplify){
    if (is.null(params)){
      stop("simplify = TRUE. please provide a list of paramters to simplify the generated model structure by removing paths that generate 0 probabilities. Avoid passing probabilities that are either 0 or 1.")
    } else { # not null params
    list2env(params, envir = .GlobalEnv)
    }
  }
  
  # here we will have an environment to parse the gmod_object
  n_cycles <- gmod_obj$n_cycles
  model_obj <- list()
  model_obj$n_cycles <- n_cycles
  model_obj$is_cycle_dep <- is_cycle_dep(gmod_obj)
  model_obj$tunnel_states <- tunnel_states(gmod_obj)
  
  model_obj <- add_decision_info(gmod_obj, model_obj)
  model_obj <- add_markov_info(gmod_obj, model_obj)
  model_obj <- event_mapping_info(gmod_obj, model_obj)
  model_obj <- add_markov_initial_probs(gmod_obj, model_obj)
  model_obj <- add_discounts_info(gmod_obj, model_obj)
  events_df <- get_event_df(gmod_obj)
  model_obj$events <- unique(events_df$event)
  model_obj$n_events <- length(model_obj$events)
  events_with_payoffs_df <- get_events_with_payoffs_df(events_df)
  model_obj$events_df <- events_df
  model_obj$events_with_payoffs_df <- events_with_payoffs_df
  model_obj <- add_payoffs(gmod_obj, model_obj)
  model_obj <- add_markov_transition_eqns(gmod_obj, model_obj, events_df, simplify = simplify)
  #model_obj <- add_markov_payoff_eqns(gmod_obj, model_obj, events_df)
  #model_obj <- add_event_prop_eqns(gmod_obj, model_obj, events_df, events_with_payoffs_df)
  #model_obj <- add_markov_event_payoff_eqns(gmod_obj, model_obj, events_df)
  
  class(model_obj) <- "gmod_markov"
  return(model_obj)
  #print(model_obj)
}



#' Build a decision tree structure
#'
#' @param gmod_obj 
#' @description takes in a gmod model syntax and returns a gmod formula structure 
#' @return gmod structure containing formulae strings
#' @export
#'
#' @examples gmod_build(mygmod)
gmod_build.gmod_decision <- function(gmod_obj, params = NULL, simplify = FALSE){
  if (simplify){
    if (is.null(params)){
      stop("simplify = TRUE. please provide a list of paramters to simplify the generated model structure by removing paths that generate 0 probabilities. Avoid passing probabilities that are either 0 or 1.")
    } else { # not null params
      list2env(params, envir = .GlobalEnv)
    }
  }
  # here we will have an environment to parse the gmod_object
  model_obj <- list()
  model_obj <- add_decision_info(gmod_obj, model_obj)
  model_obj <- event_mapping_info(gmod_obj, model_obj)
  model_obj <- add_final_outcome_info(gmod_obj, model_obj)
  model_obj <- add_event_info(gmod_obj, model_obj)
  model_obj <- add_payoffs(gmod_obj, model_obj)
  # adding stirng equations 
  model_obj <- add_decision_eqns(gmod_obj, model_obj, simplify = simplify)
  class(model_obj) <- "gmod_decision"
  return(model_obj)
}

