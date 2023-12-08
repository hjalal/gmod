# sandbox 

# to do: 
# - define keywards (self, all, age, tau/state_residence, cycle, )
# - functions like start_with(), all(), all_states(), all_decisions() ... 
# - checks of non-overlap between decisions/states/events 
# - keywards vs. functions (e.g., all(), starts_with(), ... ) 
# - - if funcitons - delay execution?
# features - sparse, 1/2 cycle correction, 
# model types DES, microsim, decision tree
# rewards, transition vs. state rewards
# compleemtn transition # by default for all states unless otherwise specified

# Define a simple S3 class for a plot
gmod <- function(n_cycles = 40) {
  gmod_obj <- list(n_cycles = n_cycles)
  class(gmod_obj) <- "gmod_class"
  
  gmod_obj
}

# Define a method for the `+` operator for `gmod` objects
`+.gmod_class` <- function(gmod_obj, layer) {
  # Add the layer to the gmod object
  type <- layer$type
  layer[["type"]] <- NULL
  if (type == "event"){
    gmod_obj[[type]] <- append(gmod_obj[[type]], list(layer))
  } else {
    gmod_obj[[type]] <- layer
    
  }
  # Return the modified gmod object
  gmod_obj
}

add_event <- function(name, if_event, then, with_probs){
  # events are the links that can either go to states or other events
  list(type = "event", 
       name = name, 
       if_event = if_event, 
       then = then, 
       with_probs = with_probs)
}
initial_probs <- function(states, probs){
  list(type = "initial_prob", states = states, probs = probs)
}
decisions <- function(...){
  list(type = "decisions", decision_names = c(...))
  # Define decisions based on each input
}
#print.gmod_class <- function(gmod_obj){
markov_states <- function(...){
  list(type = "markov_states", markov_names = c(...))
}
events <- function(...){
  list(type = "events", event_names = c(...))
}


curr_state <- function(){
  return("curr_state")
}

is_curr_state <- function(state){
  return(T)
}
# Define a placeholder function for prob_left()
prob_left <- function() {
  return(Inf)
}

construct_prob_vec <- function(x, with_probs) {
  # Check if prob_left() function is used in with_probs
  if (Inf %in% with_probs) {
    # Calculate the complement probability
    complement_index <- which(with_probs == Inf)
    complement_prob <- 1 - sum(with_probs[-complement_index])
    # Replace prob_left() in the vector with its calculated value
    with_probs[complement_index] <- complement_prob
  }
  
  # Check if the length of the vectors matches
  if (length(x) != length(with_probs)) {
    print(paste(x, with_probs))
    stop("Lengths of 'x' and 'with_probs' vectors should match.")
  }
  # Check if the length of the vectors matches
  if (sum(with_probs) != 1) {
    print(paste(x, with_probs))
    stop("Probabilities must add to 1.")
  }
  if (any(with_probs > 1) | any(with_probs < 0) ){
    print(paste(x, with_probs))
    stop("Probabilities must be between 0 and 1.")
  }
  # Create a named vector of probabilities
  prob_vector <- setNames(with_probs, x)
  return(prob_vector)
}




#}

# markov functions

# build transition prob matrix 
convert_events_to_df <- function(gmod_obj){
  events <- list()
  #initial_prob <- list()
  for (l in gmod_obj$layers){
    if (l$type == "event"){ # parse the events ... 
      expanded_layer <- expand_list(l)
      events <- bind_rows(events, expanded_layer)
    }
  }
  return(list(events = events))
}

# build transition prob from the events df
build_trans_prob_matrix <- function(gmod_obj){
  events_df <- convert_events_to_df(gmod_obj)
  states <- gmod_obj$states
  
}


# function to make sure elements legnths are either 1 or n
check_element_lengths <- function(layer){
  length_of_elements <- length(layer)
  unique_lengths <- unique(length_of_elements)
  n_unique_lengths <- length(unique_lengths)
  if (n_unique_lengths > 2 | (n_unique_lengths == 2 & min(unique_lengths) > 1)){ # print error and stop
    print("Error in ", layer, ". lengths of arguments can either be 1 or n")
  }
  return(TRUE)
}


# Function to expand the original list
expand_list <- function(original_list, output = "df") {
  check_element_lengths(original_list)
  max_length <- max(lengths(original_list))
  new_list <- vector("list", length = max_length)
  
  for (i in 1:max_length) {
    new_element <- lapply(original_list, function(x) if(length(x) == 1) x else x[i])
    new_list[[i]] <- new_element
  }
  
  if (output == "df"){
    return(data.frame(do.call(rbind, new_list)))
  } else {
    return(new_list)
  }
}



# single parser where it outputs everything 
parse_gmod <- function(gmod){
  # get state names 
  state_names <- gmod$state_names
  n_states <- length(state_names)
  # initialize matrices
  p0 <- rep(0, n_states)
  names(p0) <- state_names
  P <- matrix(0, nrow = n_states, ncol = n_states)
  rownames(P) <- colnames(P) <- state_names
  
  # fill initial prob
  gmod$layers
  index <- match("initial_probs", gmod$layers$layer)
  
  initial_prob_states <- gmod$initial_probs$states
  initial_prob_values <- gmod$initial_probs$values
  
  # fill transition prob 
  
  
  #res <- list(p0, P, T)
  return()
}