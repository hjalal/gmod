# sandbox 

# to do: 
# - define keywards (stay, all, age, n_cycles_in_state, cycle, )
# - functions like start_with(), all(), all_states(), all_decisions() ... 
# - checks of non-overlap between decisions/states/events 
# - keywards vs. functions (e.g., all(), starts_with(), ... ) 
# - - if funcitons - delay execution?
# features - sparse, 1/2 cycle correction, 
# model types DES, microsim, decision tree
# rewards, transition vs. state rewards
# compleemtn transition # by default for all states unless otherwise specified
# feature - multi-dimensional states, add state dimensions - where these dimensions can be referenced
# for example, disease can have two dimensions - bp level and dm.
# 


# Define a simple S3 class for a plot
gmod <- function(n_cycles = 40) {
  gmod_obj <- list(n_cycles = n_cycles)
  class(gmod_obj) <- "gmod_class"
  gmod_obj
}

event_dependencies <- function(gmod_obj){
  event_layers <- retrieve_layer_by_type(gmod_obj, type = "event")
  dependencies <- list()
  for (l in event_layers){
    for (k in l$goto){
    dependencies <- append(dependencies, 
                           c(from = l$name, from_type = l$type,
                             to = k, to_type = obj_type(k)))
    }
  }
  return(dependencies)
}

retrieve_obj_type <- function(gmod_obj, obj){
  states <- gmod_obj$states
  events <- gmod_obj$events
  if (obj %in% states){
    "state"
  } else if (obj %in% events){
    "event"
  } else if (obj %in% events & obj %in% states){
    stop(paste(obj, "cannot be both a state and an event"))
  } else if (!(obj %in% events) & !(obj %in% states)){
    stop(paste(obj, "is neither a state nor an event"))
  }
}

print.gmod_class <- function(gmod_obj){
  print("Hey!")
  # here we will have an environment to parse the gmod_object
  model_obj <- list()
  add_decision_info <- function(){
    # retrieve states layer
    decision_layer <- retrieve_layer_by_type(gmod_obj, type = "decisions")
    model_obj$decisions <- decision_layer$decisions
    model_obj$n_decisions <- length(model_obj$decisions)
    return(model_obj)
  }
  add_markov_info <- function(){
    # retrieve states layer
    states_layer <- retrieve_layer_by_type(gmod_obj, type = "states")
    model_obj$states <- states_layer$states
    model_obj$n_states <- length(model_obj$states)
    return(model_obj)
  }
  add_event_info <- function(){
    # retrieve states layer
    event_layer <- retrieve_layer_by_type(gmod_obj, type = "events")
    model_obj$events <- event_layer$events
    model_obj$n_events <- length(model_obj$events)
    return(model_obj) 
  }
 add_markov_initial_probs <- function(){
   markov_p0 <- retrieve_layer_by_type(gmod_obj, type = "initial_prob")
   p0 <- markov_p0$probs
   names(p0) <- markov_p0$states
   for (d in model_obj$decisions){
     model_obj[[d]]$p0 <- p0
   }
   return(model_obj)
 }
 add_markov_transition_matrix <- function(){
   P <- matrix(0, nrow = model_obj$n_states, ncol = model_obj$n_states)
   colnames(P) <- rownames(P) <- model_obj$states
   for (d in model_obj$decisions){
     model_obj[[d]]$P <- P
   }
   return(model_obj)
 }
 
  model_obj <- add_decision_info()
  model_obj <- add_markov_info()
  model_obj <- add_event_info()
  model_obj <- add_markov_initial_probs()
  model_obj <- add_markov_transition_matrix()
  #return(model_obj)
  print(model_obj)
}

retrieve_layer_by_type <- function(gmod_obj, type){
  # Use lapply to filter the list based on the condition
  result <- lapply(gmod_obj$layers, function(x) if (x$type == type) x else NULL)
  # Remove NULL elements from the list
  lyr<-Filter(Negate(is.null), result)
  if (length(lyr)==1){
    lyr <- lyr[[1]] #only select the first element if there is no more
  }
  lyr
}

# retrieve_layer_by_type <- function(gmod_obj, type){
#   layer <- list()
#   for (lyr in gmod_obj$layers){
#     if (lyr$type == type){
#       name <- lyr$name
#       #i$name <- NULL
#       layer[[name]] <- lyr
#     }
#   }
#   return(layer)
# }


# Define a method for the `+` operator for `gmod` objects
`+.gmod_class` <- function(gmod_obj, layer) {
  # Add the layer to the gmod object
  gmod_obj$layers <- c(gmod_obj$layers, list(layer))
  # Return the modified gmod object
  gmod_obj
}

add_event <- function(name, if_event, goto, with_probs){
  # events are the links that can either go to states or other events
  list(type = "event", 
       name = name, 
       if_event = if_event, 
       goto = goto, 
       with_probs = with_probs)
}
initial_probs <- function(states, probs){
  list(type = "initial_prob", states = states, probs = probs)
}
decisions <- function(...){
  list(type = "decisions", decisions = c(...))
  # Define decisions based on each input
}
#print.gmod_class <- function(gmod_obj){
states <- function(...){
  list(type = "states", states = c(...))
}
events <- function(...){
  list(type = "events", events = c(...))
}


curr_state <- function(){
  return("curr_state")
}

is_curr_state <- function(state){
  return(T)
}
curr_decision <- function(){
  return("curr_decision")
}

is_curr_decision <- function(decision){
  return(T)
}

# Define a placeholder function for prob_left()
prob_left <- function() {
  return(Inf)
}

construct_prob_vec <- function(x, v_prob) {
  # Check if prob_left() function is used in v_prob
  if (Inf %in% v_prob) {
    if (length(v_prob[is.infinite(v_prob)])>1){
      stop("Only one probability can be Inf as a complementary probability = 1-sum(other probs).")
      
    }
    # Calculate the complement probability
    complement_index <- which(v_prob == Inf)
    complement_prob <- 1 - sum(v_prob[-complement_index])
    # Replace prob_left() in the vector with its calculated value
    v_prob[complement_index] <- complement_prob
  }
  
  # Check if the length of the vectors matches
  if (length(x) != length(v_prob)) {
    print(paste(x, v_prob))
    stop("Lengths of 'x' and 'v_prob' vectors should match.")
  }
  # Check if the length of the vectors matches
  if (sum(v_prob) != 1) {
    print(paste(x, v_prob))
    stop("Probabilities must add to 1. Inf can be used as complement for one of the probabilities.")
  }
  if (any(v_prob > 1) | any(v_prob < 0) ){
    print(paste(x, v_prob))
    stop("Probabilities must be between 0 and 1. Inf can be used as complement for one of the probabilities.")
  }
  # Create a named vector of probabilities
  prob_vector <- setNames(v_prob, x)
  return(prob_vector)
}




#}

# markov functions

# build transition prob matrix 
convert_events_to_df <- function(gmod_obj){
  event_layers <- retrieve_layer_by_type(gmod_obj, type = "event")
  events_df <- bind_rows(event_layers)
  
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


# building transition prob matrix logic =======
get_event_df <- function(gmod_obj){
  event_layers <- retrieve_layer_by_type(gmod_obj, type = "event")
  bind_rows(event_layers) %>% 
    group_by(name) %>% 
    mutate(with_probs = ifelse(is.infinite(with_probs), 
                               1 - sum(with_probs[is.finite(with_probs)]), 
                               with_probs)) %>% 
    ungroup()
}

# identify the event chain
get_first_event <- function(events_df){
  event_names <- events_df$name
  event_dest <- events_df$goto
  first_event <- unique(event_names[!(event_names %in% event_dest)])
  if (length(first_event) > 1){
    stop(paste(first_event, "are originating events. There must be only a single event."))
  }
  return(first_event)
}


get_prob_chain <- function(gmod_obj, events_df, end_state){
  first_event <- get_first_event(events_df)
  # read rows where then == end_state
  sel_events_df <- events_df %>% 
    filter(goto == end_state)
  n_row <- nrow(sel_events_df)
  prob_chain <- 0
  # for each row, create chain
  for (i in 1:n_row){
    # from each then go to "name" and 
    # concatenate until reaching first_event 
    # there should be a single first_event
    # otherwise error
    curr_row <- sel_events_df[i,]
    p <- 1
    while(TRUE){
      # cumulate and get p where goto pf prev row == name of curr_row
      p <- curr_row$with_probs * p
      # stop if curr_row$name == first_event
      if (curr_row$name == first_event) break
      # set prev_row to current row
      curr_row <- events_df[events_df$goto == curr_row$name, ]
    }
    prob_chain <- prob_chain + p
  }
  # remove row
  return(prob_chain)
}
