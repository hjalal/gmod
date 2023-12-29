# sandbox 

# to do: 
# - define keywards (stay, all, age, cycle_in_state, cycle, )
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

# is_cycle_dep <- function(gmod_obj){
#   #gmod_obj$layers
#   events_df <- get_event_df(gmod_obj)
#   any(grepl("\\bcycle\\b", events_df$with_probs)) 
#   # or any payoffs?
# }
# is_payoff_cycle_dep <- function(model_obj){
#   any(grepl("\\bcycle\\b", model_obj$payoffs)) 
# }
is_cycle_dep <- function(gmod_obj){
  #gmod_obj$layers
  events_df <- get_event_df(gmod_obj)
  payoffs <- retrieve_layer_by_type(gmod_obj, type = "payoffs")
  transitions_cycle_dep <- any(grepl("\\bcycle\\b", events_df$with_probs)) 
  payoffs_cycle_dep <- any(grepl("\\bcycle\\b", payoffs$payoffs)) 
  any(transitions_cycle_dep, payoffs_cycle_dep)
}



# tunnel_states <- function(gmod_obj){
#   #gmod_obj$layers
#   events_df <- get_event_df(gmod_obj)
#   # capture all tunnel states 
#   matches <- str_match_all(events_df$with_probs, 'cycle_in_state\\("(.*?)"\\)')
#   second_elements <- lapply(matches, function(x) x[2])
#   # Convert the list to a vector if needed
#   tunnel_states <- unique(unlist(second_elements))
#   tunnel_states <- tunnel_states[!is.na(tunnel_states)]
#   return(tunnel_states)
# }
tunnel_states <- function(gmod_obj){
  #gmod_obj$layers
  events_df <- get_event_df(gmod_obj)
  payoffs_layer <- retrieve_layer_by_type(gmod_obj, type = "payoffs")
  payoffs_str <- sapply(payoffs_layer$payoffs, deparse)
  # capture all tunnel states 
  matches_transitions <- str_match_all(events_df$with_probs, 'cycle_in_state\\("(.*?)"\\)')
  tunnel_transitions <- lapply(matches_transitions, function(x) x[2])
  # Convert the list to a vector if needed
  matches_payoffs <- str_match_all(payoffs_str, 'cycle_in_state\\("(.*?)"\\)')
  tunnel_payoffs <- lapply(matches_payoffs, function(x) x[2])
  
  # combine tunnel states from both payoffs and transitions
  tunnel_states <- unique(c(unlist(tunnel_transitions), unlist(tunnel_payoffs)))
  tunnel_states <- tunnel_states[!is.na(tunnel_states)]
  return(tunnel_states)
}

# gets the state and tunnel out of a tunnel state
tunnel2state <- function(tunnel_state){
  state_comp <- strsplit(tunnel_state, "_tnl")[[1]]
  if (length(state_comp) == 1){
    state_comp[2] <- 0
  }
  #names(state_comp) <- c("state", "tunnel")
  return(state_comp)
}

# event_dependencies <- function(gmod_obj){
#   event_layers <- retrieve_layer_by_type(gmod_obj, type = "event")
#   dependencies <- list()
#   for (l in event_layers){
#     for (k in l$goto){
#       dependencies <- append(dependencies, 
#                              c(from = l$name, from_type = l$type,
#                                to = k, to_type = obj_type(k)))
#     }
#   }
#   return(dependencies)
# }

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

# if class is Decision
print.gmod_decision <- function(gmod_obj){
  # here we will have an environment to parse the gmod_object
  model_obj <- list()
  model_obj <- add_decision_info(gmod_obj, model_obj)
  model_obj <- event_mapping_info(gmod_obj, model_obj)
  model_obj <- add_outcome_info(gmod_obj, model_obj)
  model_obj <- add_payoffs(gmod_obj, model_obj)
  model_obj <- add_outcome_probs(gmod_obj, model_obj)
  #return(model_obj)
  print(model_obj)
}

# add_decision_info <- function(gmod_obj, model_obj){
#   # retrieve states layer
#   decision_layer <- retrieve_layer_by_type(gmod_obj, type = "decisions")
#   model_obj$decisions <- decision_layer$decisions
#   model_obj$n_decisions <- length(model_obj$decisions)
#   return(model_obj)
# }
add_outcome_info <- function(gmod_obj, model_obj){
  # retrieve states layer
  #outcomes_layer <- retrieve_layer_by_type(gmod_obj, type = "outcomes")
  events_df <- get_event_df(gmod_obj)
  model_obj$outcomes <- get_outcomes(events_df)
  model_obj$n_outcomes <- length(model_obj$outcomes)
  return(model_obj)
}
# event_mapping_info <- function(gmod_obj, model_obj){
#   # retrieve states layer
#   event_layer <- retrieve_layer_by_type(gmod_obj, type = "events")
#   model_obj$events <- event_layer$events
#   model_obj$n_events <- length(model_obj$events)
#   return(model_obj) 
# }

add_payoffs <- function(gmod_obj, model_obj){
  payoffs_layer <- retrieve_layer_by_type(gmod_obj, type = "payoffs")
  model_obj$payoffs <- payoffs_layer$payoffs
  model_obj$payoff_names <- names(payoffs_layer$payoffs)
  model_obj$n_payoffs <- length(model_obj$payoffs)
  return(model_obj) 
}

add_outcome_probs <- function(gmod_obj, model_obj){
  outcomes <- model_obj$outcomes
  n_outcomes <- model_obj$n_outcomes
  events_df <- get_event_df(gmod_obj)
  first_event <- get_first_event(events_df)
  
  # payoffs
  payoffs <- model_obj$payoffs
  payoff_names <- names(payoffs)
  
  vec_p_raw <- vec_p_stay <- rep("0", n_outcomes)
  names(vec_p_raw) <- names(vec_p_stay) <- outcomes
  for (outcome in outcomes){
    vec_p_raw[outcome] <- get_prob_chain(gmod_obj, events_df, end_state = outcome)
    #vec_p_stay[outcome] <- get_prob_chain(gmod_obj, events_df, end_outcome = "curr_outcome")
  }
  
  # add "curr_outcome" ones as well here to form P_raw as a matrix
  for (decision in model_obj$decisions){
    model_obj[[decision]]$P_raw <- rep("0", n_outcomes)
    model_obj[[decision]]$P <- rep(0, n_outcomes)
    names(model_obj[[decision]]$P_raw) <- outcomes
    names(model_obj[[decision]]$P) <- outcomes
    
    # iterate through payoffs
    for (payoff_name in payoff_names){
      model_obj[[decision]]$payoffs[[payoff_name]] <- rep(0, n_outcomes)
      names(model_obj[[decision]]$payoffs[[payoff_name]]) <- outcomes
    }
    
    for (outcome in outcomes){
      p_trans_formula <- get_prob_chain(gmod_obj, events_df, end_state = outcome)
      p_trans_value <- eval(parse(text = p_trans_formula)) 
      # if (p_trans_value == 0){
      #   p_trans_formula <- "0"
      # }
      # if (state == dest){
      #   p_stay_formula <- get_prob_chain(gmod_obj, events_df, end_state = "curr_state")
      #   p_stay_value <- eval(parse(text = p_stay_formula)) 
      #   if (p_stay_value > 0){ # can be added to the p_transformula
      #     if (p_trans_value > 0){
      #       p_trans_formula <- paste0(p_trans_formula, "+", p_stay_formula)
      #     } else { # if the transformula is empty, then just keep the stay formula
      #       p_trans_formula <- p_stay_formula
      #     }
      #   }
      #   }
      
      model_obj[[decision]]$P[outcome] <- eval(parse(text = p_trans_formula))
      
      #p_trans_formula <- gsub("\\bstate\\b", state, p_trans_formula)
      p_trans_formula <- gsub("\\bdecision\\b", decision, p_trans_formula)
      model_obj[[decision]]$P_raw[outcome] <- p_trans_formula
      for (payoff_name in payoff_names){
        model_obj[[decision]]$payoffs[[payoff_name]][outcome] <- eval(payoffs[[payoff_name]])
      } # end payoffs 
    } # end outcome
    # EVs
    for (payoff_name in payoff_names){
      model_obj[[decision]]$ev[[payoff_name]] <- 
        model_obj[[decision]]$payoffs[[payoff_name]] %*% 
        model_obj[[decision]]$P
      
    } # end payoffs
  } # end decision
  
  return(model_obj)
}


# if class is Markov
gmod_build <- function(x, ...) UseMethod("gmod_build")
gmod_build.gmod_markov <- function(gmod_obj){
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
  model_obj <- add_markov_transition_eqns(gmod_obj, model_obj, events_df)
  model_obj <- add_payoffs(gmod_obj, model_obj)
  model_obj <- add_markov_payoff_eqns(gmod_obj, model_obj, events_df)
  class(model_obj) <- "gmod_markov"
  return(model_obj)
  #print(model_obj)
}


add_decision_info <- function(gmod_obj, model_obj){
  # retrieve states layer
  decision_layer <- retrieve_layer_by_type(gmod_obj, type = "decisions")
  model_obj$decisions <- decision_layer$decisions
  model_obj$n_decisions <- length(model_obj$decisions)
  return(model_obj)
}


add_markov_info <- function(gmod_obj, model_obj){
  # retrieve states layer
  states_layer <- retrieve_layer_by_type(gmod_obj, type = "states")
  states <- states_layer$states# no tunnel states
  n_cycles <- gmod_obj$n_cycles
  
  tunnel_states <- model_obj$tunnel_states
  if (length(tunnel_states) > 0){ #there are tunnel states
    #for each tunnel state expand states vector
    is_tunnel <- states %in% tunnel_states # F,T,F since severe is the tunnel state 
    # now we need to expand the state space to incorporate the tunnels and all other vectors
    rep_states <- is_tunnel * (n_cycles - 1) + 1 # {1, 40, 1} a trick to get number of replication for each state
    states_expanded <- rep(states, rep_states)
    for (tunnel_state in tunnel_states){
      states_expanded[states_expanded == tunnel_state] <- paste0(tunnel_state, "_tnl", 1:n_cycles) # we need to replace severe with Severe Yr1, Severe Yr2, ... etc
    }
  } else {
    states_expanded <- states
  }
  model_obj$states <- states
  model_obj$n_states <- length(states)
  model_obj$states_expanded <- states_expanded
  model_obj$n_states_expanded <- length(states_expanded)    
  return(model_obj)
}
event_mapping_info <- function(gmod_obj, model_obj){
  # retrieve states layer
  event_layer <- retrieve_layer_by_type(gmod_obj, type = "events")
  model_obj$events <- event_layer$events
  model_obj$n_events <- length(model_obj$events)
  return(model_obj) 
}

add_discounts_info <- function(gmod_obj, model_obj){
  discounts_info <- retrieve_layer_by_type(gmod_obj, type = "discounts")
  model_obj$discounts <- discounts_info$discounts
  names(model_obj$discounts) <- discounts_info$payoffs
  return(model_obj)
}

add_markov_initial_probs <- function(gmod_obj, model_obj){
  # all prob are 0, just replace the ones provided with their values
  markov_p0 <- retrieve_layer_by_type(gmod_obj, type = "initial_prob")
  init_p0 <- markov_p0$probs
  init_states <- markov_p0$states
  tunnel_states <- model_obj$tunnel_states
  p0 <- rep(0, model_obj$n_states_expanded)  # empty vector
  names(p0) <- model_obj$states_expanded
  
  for (d in model_obj$decisions){
    model_obj$p0[[d]] <- p0
    for (i in 1:length(init_p0)){
      state <- init_states[i]
      if (state %in% tunnel_states){
        state <- paste0(state, "_tnl1")
      }
      model_obj$p0[[d]][state] <- init_p0[i]
    }
    model_obj$p0[[d]] <- check_prob_vector(model_obj$p0[[d]])
  }
  return(model_obj)
}



add_markov_transition_eqns <- function(gmod_obj, model_obj, events_df){
  states <- model_obj$states
  #n_states <- model_obj$n_states
  states_expanded <- model_obj$states_expanded
  n_states_expanded <- length(states_expanded)
  tunnel_states <- model_obj$tunnel_states
  n_cycles <- gmod_obj$n_cycles
  # get expanded states 
  
  # add "curr_state" ones as well here to form P_raw as a matrix
  for (decision in model_obj$decisions){
    if (model_obj$is_cycle_dep){ # array
      n_cycles <- gmod_obj$n_cycles
      cycle <- 1:n_cycles
      #cycle <- cycle_range # try vector format!
      cycles <- paste0("cycle", cycle)
      # change n_states to n_states_expanded
      model_obj$P[[decision]] <- array("0", dim = c(n_states_expanded, n_states_expanded, n_cycles), 
                                       dimnames = list(states_expanded, states_expanded, cycles))
    } else { # matrix
      model_obj$P[[decision]] <- matrix("0", nrow = n_states_expanded, ncol = n_states_expanded, 
                                        dimnames = list(states_expanded, states_expanded))
    }
    
    for (dest_expanded in states_expanded){
      dest_comp <- tunnel2state(dest_expanded)
      dest <- dest_comp[1]
      dest_idx <- as.integer(dest_comp[2])
      for (state_expanded in states_expanded){
        state_comp <- tunnel2state(state_expanded)
        state <- state_comp[1]
        state_idx <- as.integer(state_comp[2])
        
        p_trans_formula <- get_prob_chain(gmod_obj, events_df, end_state = dest)
        if (state == dest){
          p_stay_formula <- get_prob_chain(gmod_obj, events_df, end_state = "curr_state")
          p_trans_formula <- paste0(p_trans_formula, "+", p_stay_formula)
        }
        p_trans_formula <- gsub("\\bstate\\b", paste0("'", state, "'"), p_trans_formula)
        p_trans_formula <- gsub("\\bdecision\\b", paste0("'", decision, "'"), p_trans_formula)
        
        # write error trap if state within cycle_in_state doesn't match state
        p_trans_formula <- gsub("cycle_in_state\\([^)]+\\)", paste0("cycle_in_state=",state_idx), p_trans_formula)
        
        if (!(state %in% tunnel_states & state == dest & dest_idx != state_idx+1)){ #otherwise keep at "0"
          if (model_obj$is_cycle_dep){ # array
            p_trans_formula <- sapply(cycle, function(x) gsub("\\bcycle\\b", paste0("cycle=",x), p_trans_formula))
            model_obj$P[[decision]][state_expanded, dest_expanded, ] <- p_trans_formula
          } else {
            model_obj$P[[decision]][state_expanded, dest_expanded] <- p_trans_formula
          }
        }
      } # end origin state
    } # end dest state 
  } # end decision
  return(model_obj)
}

add_markov_payoff_eqns <- function(gmod_obj, model_obj, events_df){
  states <- model_obj$states
  #n_states <- model_obj$n_states
  states_expanded <- model_obj$states_expanded
  n_states_expanded <- length(states_expanded)
  tunnel_states <- model_obj$tunnel_states
  n_cycles <- gmod_obj$n_cycles
  payoffs <- model_obj$payoffs
  payoff_names <- names(payoffs)
  model_obj$Payoffs <- list()
  # add "curr_state" ones as well here to form P_raw as a matrix
  for (payoff in payoff_names){
    for (decision in model_obj$decisions){
      if (model_obj$is_cycle_dep){ # matrix of rewards states x cycle
        n_cycles <- gmod_obj$n_cycles
        cycle <- 1:n_cycles
        #cycle <- cycle_range # try vector format!
        cycles <- paste0("cycle", cycle)
        # change n_states to n_states_expanded
        model_obj$Payoffs[[payoff]][[decision]] <- matrix("0", nrow=n_cycles, ncol = n_states_expanded, 
                                                          dimnames = list(cycles, states_expanded))
      } else { # a single vector 
        model_obj$Payoffs[[payoff]][[decision]] <- rep("0", n_states_expanded)
        names(model_obj$Payoffs[[payoff]][[decision]]) <- states_expanded
      }
      
      for (state_expanded in states_expanded){
        state_comp <- tunnel2state(state_expanded)
        state <- state_comp[1]
        state_idx <- as.integer(state_comp[2])
        
        payoff_formula <- deparse(payoffs[[payoff]])
        payoff_formula <- gsub("\\bstate\\b", paste0("'", state, "'"), payoff_formula)
        payoff_formula <- gsub("\\bdecision\\b", paste0("'", decision, "'"), payoff_formula)
        # write error trap if state within cycle_in_state doesn't match state
        payoff_formula <- gsub("cycle_in_state\\([^)]+\\)", paste0("cycle_in_state=",state_idx), payoff_formula)
        
        if (model_obj$is_cycle_dep){ # matrix
          payoff_formula <- sapply(cycle, function(x) gsub("\\bcycle\\b", paste0("cycle=",x), payoff_formula))
          model_obj$Payoffs[[payoff]][[decision]][, state_expanded] <- payoff_formula
        } else {
          model_obj$Payoffs[[payoff]][[decision]][state_expanded] <- payoff_formula
        }
        
      } # end origin state
    } # end decision
  } # end payoff
  return(model_obj)
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


# Define a method for the `+` operator for `gmod` objects
`+.gmod_class` <- function(gmod_obj, layer) {
  # Add the layer to the gmod object
  gmod_obj$layers <- c(gmod_obj$layers, list(layer))
  # Return the modified gmod object
  gmod_obj
}

event_mapping <- function(name, if_event, goto, with_probs){
  # events are the links that can either go to states or other events
  input_string <- deparse(substitute(with_probs))
  #input_string <- as.list(match.call())$with_probs
  list(type = "event", 
       name = name, 
       if_event = if_event, 
       goto = goto, 
       with_probs = probs2string(input_string)
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
events <- function(...){
  list(type = "events", events = c(...))
}
outcomes <- function(...){
  list(type = "outcomes", outcomes = c(...))
}
payoffs <- function(...){
  input_string <- as.list(match.call())
  # argument_list <- list(...)
  # for (arg in argument_list){
  #   arg_string <- deparse(substitute(arg))
  # }
  list(type = "payoffs", payoffs = input_string[-1])
}
curr_state <- function(){
  return("curr_state")
}

# Define a placeholder function for prob_left()
prob_left <- function() {
  return(Inf)
}

construct_prob_vec <- function(x, v_prob) {
  # Check if prob_left() function is used in v_prob
  v_prob <- check_prob_vector(v_prob) 
  # Check if the length of the vectors matches
  if (length(x) != length(v_prob)) {
    print(paste(x, v_prob))
    stop("Lengths of 'x' and 'v_prob' vectors should match.")
  }
  
  # Create a named vector of probabilities
  prob_vector <- setNames(v_prob, x)
  return(prob_vector)
}

check_prob_vector <- function(v_prob){
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
  if (sum(v_prob) != 1) {
    print(paste(v_prob))
    stop("Probabilities must add to 1. Inf can be used as complement for one of the probabilities.")
  }
  if (any(v_prob > 1) | any(v_prob < 0) ){
    print(paste(v_prob))
    stop("Probabilities must be between 0 and 1. Inf can be used as complement for one of the probabilities.")
  }
  return(v_prob)
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





# building transition prob matrix logic =======
get_event_df <- function(gmod_obj){
  event_layers <- retrieve_layer_by_type(gmod_obj, type = "event")
  bind_rows(event_layers) 
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
get_outcomes <- function(events_df){
  unique(events_df$goto[!(events_df$goto %in% events_df$name)])
}

get_prob_chain <- function(gmod_obj, events_df, end_state){
  first_event <- get_first_event(events_df)
  # read rows where then == end_state
  sel_events_df <- events_df %>% 
    filter(goto == end_state)
  n_row <- nrow(sel_events_df)
  prob_chain <- ""
  # for each row, create chain
  if (n_row > 0){ # at least one states leads to the current state
    for (i in 1:n_row){
      # from each then go to "name" and 
      # concatenate until reaching first_event 
      # there should be a single first_event
      # otherwise error
      curr_row <- sel_events_df[i,]
      p <- ""
      while(TRUE){
        # cumulate and get p where goto pf prev row == name of curr_row
        if (p == ""){
          p <- paste0("(",curr_row$with_probs,")")
        } else {
          p <- paste0("(",curr_row$with_probs,")", "*", p)
        }
        # stop if curr_row$name == first_event
        if (curr_row$name == first_event) break
        # set prev_row to current row
        curr_row <- events_df[events_df$goto == curr_row$name, ]
        if (nrow(curr_row)>1){
          stop(paste("Multiple outcomes lead to the same event", curr_row))
        }
      }
      if (prob_chain == ""){
        prob_chain <- p
      } else {
        prob_chain <- paste0(prob_chain, "+", p)
      }
    }
  } else { # if there are no events leading to the current state
    prob_chain <- "0"
  }
  # remove row
  return(prob_chain)
}



# Function to return the name of a function as a string
probs2string <- function(input_string) {
  # https://stackoverflow.com/questions/35347537/using-strsplit-in-r-ignoring-anything-in-parentheses
  #input_string <- deparse(substitute(probs))
  # Extract elements within c() using regex
  #wo_white_spaces <- input_string #
  wo_white_spaces <- gsub("\\s+", "", input_string)
  cleaned_string <- sub("^c\\((.*)\\)$", "\\1", wo_white_spaces)
  #y <- strsplit(cleaned_string, ", |(?>\\(.*?\\).*?\\K(, |$))", perl = TRUE)[[1]]
  y <- strsplit(cleaned_string, ",|(?>\\(.*?\\).*?\\K(,|$))", perl = TRUE)[[1]]
  #extracted_elements <- gsub("^c\\((.*)\\)$", "\\1", input_string)
  # Split the elements by comma (,) and remove leading/trailing spaces
  # remove complement in "inf"
  inf_index <- y == "Inf"
  if (any(inf_index)){
    sum_others <- paste0(y[!inf_index], collapse = "+")
    y[inf_index] <- paste0("1-(", sum_others, ")")
  }
  return(y)
}


gmod_parse <- function(x, ...) UseMethod("gmod_parse")
gmod_parse.gmod_markov <- function(model_struc, params = NULL){
  # for Markov structure, parse P, p0 and Payoffs and replace
  model_num_str <- model_struc
  decisions <- model_struc$decisions
  for (decision in decisions){
    model_num_str$p0[[decision]] <- parse_object(model_struc$p0[[decision]])
    model_num_str$P[[decision]] <- parse_object(model_struc$P[[decision]])
    for (payoff in model_struc$payoff_names){
      model_num_str$Payoffs[[payoff]][[decision]] <- 
        parse_object(model_struc$Payoffs[[payoff]][[decision]])
    }
  }
  class(model_num_str) <- "gmod_markov"
  return(model_num_str)
}

parse_object <- function(x){
  d <- dim(x)
  # Convert the array to a vector
  vectorized_x <- c(x)
  n <- length(vectorized_x)
  eval_x <- rep(0, n)
  for (i in 1:n){
    eval_x[i] <- eval(parse(text = vectorized_x[i]))
  }
  # Convert the vector back to an array
  if (is.null(d)){
    y <- as.vector(eval_x)
    names(y) <- names(x)
  } else {
    y <- array(eval_x, dim = d, dimnames = dimnames(x))
  }
  return(y)
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
      Payoff <- model_num_struc$Payoffs[[payoff]][[decision]] 
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
