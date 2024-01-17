
#' Create a new gmod
#'
#' @param model_type 
#' @param n_cycles = 50 
#' @return a new gmod object 
#' @export
#' @importFrom magrittr %>%
#' @examples gmod(model_type = "Markov", n_cycles = 40)
#' 
gmod <- function(model_type, n_cycles = 50) {
  gmod_obj <- list()
  model_type <- tolower(model_type)
  if (model_type == "markov"){
    class(gmod_obj) <- c("gmod_markov", "gmod_class")
    # by default is time independent - becomes time dep if there is n_cycles in one of the events
    gmod_obj$n_cycles <- n_cycles
  } else if (model_type == "decision"){
    class(gmod_obj) <- c("gmod_decision", "gmod_class")
  } else {
    stop(paste("model_type can either be Markov or Decision. Model type = ", model_type, "is not supported."))
  }
  gmod_obj
}



#' Define a method for the `+` operator for `gmod` objects
#'
#' @param gmod_obj 
#' @param layer 
#'
#' @return gmod_obj
#' @export
#'
#' @examples 
`+.gmod_class` <- function(gmod_obj, layer) {
  # Add the layer to the gmod object
  gmod_obj$layers <- c(gmod_obj$layers, list(layer))
  # Return the modified gmod object
  gmod_obj
}



#' Add event mapping
#'
#' @param event 
#' @param values 
#' @param outcomes 
#' @param probs 
#' @param payoffs 
#'
#' @return gmod layer 
#' @export
#'
#' @examples event_mapping(event = "event_progress", 
#' values = c(TRUE,FALSE), 
#' outcomes = c("Severe",curr_state()), 
#' probs = c(p_progress_function(state), Inf), 
#' payoffs = list(cost = c(cProgress, cNotProgress)))

event_mapping <- function(event, values, outcomes, probs, payoffs=NULL){
  # events are the links that can either go to states or other events
  input_string <- paste0(deparse(substitute(probs)), collapse = "")
  
  payoffs_string <- paste0(deparse(substitute(payoffs)), collapse = "")
  if (payoffs_string == "NULL"){
    payoffs_string <- ""
  }
  #input_string <- as.list(match.call())$probs
  list(type = "event", 
       event = event, 
       values = values, 
       outcomes = outcomes, 
       probs = probs2string(input_string),
       payoffs = payoffs_string
  )
}

#' Add initial probabilities to a Markov gmod object
#'
#' @param states
#' @param probs
#'
#' @return a gmod layer with initial probabilities
#' @export
#'
#' @examples initial_probs(states = c("Healthy", "Sick", "Dead"), probs = c(1,0,0))
#' @examples initial_probs(states = c("Healthy", "Sick", "Dead"), probs = c(0.5,0.3,Inf))
#' @examples initial_probs(states = "Healthy", probs = 1)

initial_probs <- function(states, probs){
  list(type = "initial_prob", states = states, probs = probs)
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
discounts <- function(payoffs, discounts){
  list(type = "discounts", payoffs = payoffs, discounts = discounts)
}

#' Title
#'
#' @param states 
#' @param lengths 
#'
#' @return a gmod layer with tunnels
#' @export
#'
#' @examples
tunnels <- function(states, lengths = NULL){
  list(type = "tunnels", states = states, lengths = lengths)
}


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
states <- function(...){
  list(type = "states", states = c(...))
}

#' Add final_outcomes from a decision tree to a gmod
#'
#' @param ... Decision final_outcome names
#'
#' @return a gmod layer with Decision final_outcome names
#' @export
#'
#' @examples final_outcomes("Alive", "Dead")
final_outcomes <- function(...){
  list(type = "final_outcomes", final_outcomes = c(...))
}

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
  list(type = "payoffs", payoffs = input_string[-1])
}


#' Current state
#' @description A function used inside event_mapping(probs = ) placeholder for returning the current state
#' @param 
#'
#' @return "curr_state"
#' @export
#'
#' @examples curr_state()
curr_state <- function(){
  return("curr_state")
}

#' Complementary probability
#' @description A function used inside event_mapping(probs = ) placeholder for returning Inf. which will be used internally to return the complementary probabilities
#' @param 
#'
#' @return "curr_state"
#' @export
#'
#' @examples curr_state() 
prob_left <- function() {
  return(Inf)
}

#' Value of a previous event  
#' @description A function used inside event_mapping(probs = ) placeholder for returning the value of the previous event
#' @param 
#'
#' @return "curr_state"
#' @export
#'
#' @examples curr_state() 
prev_event <- function(...){
  args <- list(...)
  return(unlist(args))
}
