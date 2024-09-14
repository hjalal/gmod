
#' Create a new gmod
#'
#' @param model_type 
#' @return a new gmod object 
#' @export
#' @importFrom magrittr %>%
#' @examples gmod(model_type = "Markov", n_cycles = 10)
#' 
gmod <- function(name, model_function_name = NULL) {
  gmod_obj <- list(name = name)
  if (!is.null(model_function_name)){
    gmod_obj$model_function_name = model_function_name
  }
  class(gmod_obj) <- c("gmod_decision", "gmod_class")
  # if (!is.null(n_cycles)){
  #   gmod_obj$n_cycles = n_cycles
  # }
  gmod_obj
}


#' Define a method for the `+` operator for `gmod` objects
#'
#' @param gmod_obj 
#' @param layer 
#' @description Adds layers to the gmod object
#' @return gmod_obj
#' @export
#'
#' @examples 
#' mygmod <- gmod(model_type = "Markov", n_cycles = 75) + 
#' decisions("StandardOfCare", "StrategyA", "StrategyB", "StrategyAB")

`+.gmod_class` <- function(gmod_obj, layer) {
  if (is.null(layer$type)){ 
    if (layer[[1]]$type=="states"){ # split the layer into 3
      for (l in layer){
        gmod_obj$layers <- c(gmod_obj$layers, list(l))
      }
      class(gmod_obj) <- NULL
      class(gmod_obj) <- c("gmod_markov", "gmod_class")
    } else if (layer[[1]]$type=="payoffs"){
      for (l in layer){
        gmod_obj$layers <- c(gmod_obj$layers, list(l))
      }
  }
  } else {
    # Add the layer to the gmod object
    gmod_obj$layers <- c(gmod_obj$layers, list(layer))
  }
  # Return the modified gmod object
  gmod_obj
}



#' Add event mapping
#'
#' @param name 
#' @param scenarios 
#' @param probs 
#' @param outcomes 
#'
#' @return gmod layer 
#' @export
#'
#' @examples event_mapping(name = "event_progress", 
#' scenarios = c(TRUE,FALSE), 
#' probs = c(p_progress_function(state), Inf), 
#' outcomes = c("Severe","curr_state")
#' 
event <- function(name, scenarios, probs, outcomes){
  # events are the links that can either go to states or other events
  input_string <- paste0(deparse(substitute(probs)), collapse = "")
  
  # payoffs_string <- paste0(deparse(substitute(payoffs)), collapse = "")
  # if (payoffs_string == "NULL"){
  #   payoffs_string <- ""
  # }
  #input_string <- as.list(match.call())$probs
  list(type = "event", 
       event = name, 
       values = scenarios, 
       probs = probs2string(input_string),
       outcomes = outcomes #,
       #payoffs = payoffs_string
  )
}





#' Add discounts to a gmod Markov object
#'
#' @param ... decision names
#'
#' @return a gmod layer with decision names
#' @export
#'
#' @examples discounts(payoffs = c("cost", "effectiveness"), discounts = c(0.5, 0.5))
#' @examples discounts(payoffs = c("cost", "effectiveness"), discounts = c(0.15, 0.15))


#' Title
#'
#' @param states names of the states to expand
#' @param lengths the length of each tunnel state
#'
#' @return a gmod layer with tunnels
#' @export
#'
#' @examples tunnels(states = c("S1", "S2"), lengths = c(3, 5))



#' Add decisions to a gmod
#'
#' @param ... decision names
#'
#' @return a gmod layer with decision names
#' @export
#'
#' @examples decisions("A", "B", "C")
decisions <- function(...){
  list(type = "decisions", decisions = c(...))
  # Define decisions based on each input
}


#' Add Markov states to a gmod
#'
#' @param ... Markov state names
#'
#' @return a gmod layer with Markov state names
#' @export
#'
#' @examples states("Healthy", "Sick", "Dead")
states <- function(names, init_probs, max_cycle_in_states = NULL){
  l1<- list(type = "states", states = names)
  l2<- list(type = "initial_prob", states = names, probs = init_probs)
  
  tunnel_names <- names[max_cycle_in_states > 1]
  tunnel_lengths <- max_cycle_in_states[max_cycle_in_states>1]
  
  if (length(tunnel_names)>0){
    l3<- list(type = "tunnels", states = tunnel_names, lengths = tunnel_lengths)
    l <- list(l1,l2,l3)
    
  } else {
    l <- list(l1,l2)
  }
  return(l)
}

#' Add final_outcomes from a decision tree to a gmod
#'
#' @param ... Decision final_outcome names
#'
#' @return a gmod layer with Decision final_outcome names
#' @export
#'
#' @examples final_outcomes("Alive", "Dead")
# final_outcomes <- function(...){
#   list(type = "final_outcomes", final_outcomes = c(...))
# }

#' Add payoffs to a gmod
#'
#' @param ... a named list containing the payoffs and the associated payoff functions
#'
#' @return a gmod layer with payoffs
#' @export
#'
#' @examples payoffs(cost = cost_function(state), effectiveness = effective_function(state))
payoffs <- function(...){
  input_string <- as.list(match.call())
  payoffs <- input_string[-1]
  payoffs$discount_rates <- NULL
  l1 <- list(type = "payoffs", payoffs = payoffs)
  discounts <- input_string$discount_rates
  if (length(discounts)>0){
    names(discounts) <- c(NA, names(payoffs))
    l2 <- list(type = "discounts", payoffs = names(payoffs), discounts = discounts)
    l <- list(l1,l2)
  } else {
    l <- l1
  }
return(l)
}


#' Define a print method for `gmod_class` objects
#' @param gmod_obj The gmod object
#' @export
print.gmod_class <- function(gmod_obj) {
  # Print the gmod object as usual
  if (is.null(gmod_obj$model_function_name)){
    model_struc <- gmod_gen_model_function(gmod_obj) 
  } else {
    model_struc <- gmod_gen_model_function(gmod_obj, model_function_name = gmod_obj$model_function_name) 
  }
  
  gmod_obj <- c(gmod_obj, model_struc)
  assign(gmod_obj$name, gmod_obj, envir = .GlobalEnv) # return it to the global environment
  #print.default(gmod_obj)
}




