
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

# Define a method for the `+` operator for `gmod` objects
`+.gmod_class` <- function(gmod_obj, layer) {
  # Add the layer to the gmod object
  gmod_obj$layers <- c(gmod_obj$layers, list(layer))
  # Return the modified gmod object
  gmod_obj
}

event_mapping <- function(event, values, results, probs, payoffs=NULL){
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
       results = results, 
       probs = probs2string(input_string),
       payoffs = payoffs_string
  )
}
initial_probs <- function(states, probs){
  list(type = "initial_prob", states = states, probs = probs)
}
discounts <- function(payoffs, discounts){
  list(type = "discounts", payoffs = payoffs, discounts = discounts)
}

decisions <- function(...){
  list(type = "decisions", decisions = c(...))
  # Define decisions based on each input
}
#print.gmod_class <- function(gmod_obj){
states <- function(...){
  list(type = "states", states = c(...))
}
# outcomes <- function(...){
#   list(type = "outcomes", outcomes = c(...))
# }
payoffs <- function(...){
  input_string <- as.list(match.call())
  list(type = "payoffs", payoffs = input_string[-1])
}
curr_state <- function(){
  return("curr_state")
}

# Define a placeholder function for prob_left()
prob_left <- function() {
  return(Inf)
}

# specialized functions
prev_event <- function(...){
  args <- list(...)
  return(unlist(args))
}
