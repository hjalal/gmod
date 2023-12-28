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

is_cycle_dep <- function(gmod_obj){
  #gmod_obj$layers
  events_df <- get_event_df(gmod_obj)
  any(grepl("\\bcycle\\b", events_df$with_probs)) 
}

tunnel_states <- function(gmod_obj){
  #gmod_obj$layers
  events_df <- get_event_df(gmod_obj)
  # capture all tunnel states 
  matches <- str_match_all(events_df$with_probs, 'cycle_in_state\\("(.*?)"\\)')
  second_elements <- lapply(matches, function(x) x[2])
  # Convert the list to a vector if needed
  tunnel_states <- unique(unlist(second_elements))
  tunnel_states <- tunnel_states[!is.na(tunnel_states)]
  return(tunnel_states)
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

# if class is Decision
print.gmod_decision <- function(gmod_obj){
  # here we will have an environment to parse the gmod_object
  model_obj <- list()
  add_decision_info <- function(){
    # retrieve states layer
    decision_layer <- retrieve_layer_by_type(gmod_obj, type = "decisions")
    model_obj$decisions <- decision_layer$decisions
    model_obj$n_decisions <- length(model_obj$decisions)
    return(model_obj)
  }
  add_outcome_info <- function(){
    # retrieve states layer
    #outcomes_layer <- retrieve_layer_by_type(gmod_obj, type = "outcomes")
    events_df <- get_event_df(gmod_obj)
    model_obj$outcomes <- get_outcomes(events_df)
    model_obj$n_outcomes <- length(model_obj$outcomes)
    return(model_obj)
  }
  add_event_info <- function(){
    # retrieve states layer
    event_layer <- retrieve_layer_by_type(gmod_obj, type = "events")
    model_obj$events <- event_layer$events
    model_obj$n_events <- length(model_obj$events)
    return(model_obj) 
  }
  model_obj <- add_decision_info()
  model_obj <- add_event_info()
  model_obj <- add_outcome_info()
  model_obj <- add_payoffs(gmod_obj, model_obj)
  model_obj <- add_outcome_probs(gmod_obj, model_obj)
  #return(model_obj)
  print(model_obj)
}


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
print.gmod_markov <- function(gmod_obj){
  # here we will have an environment to parse the gmod_object
  n_cycles <- gmod_obj$n_cycles
  model_obj <- list()
  model_obj$is_cycle_dep <- is_cycle_dep(gmod_obj)
  model_obj$tunnel_states <- tunnel_states(gmod_obj)

  model_obj <- add_decision_info(gmod_obj, model_obj)
  model_obj <- add_markov_info(gmod_obj, model_obj)
  model_obj <- add_event_info(gmod_obj, model_obj)
  model_obj <- add_markov_initial_probs(gmod_obj, model_obj)
  events_df <- get_event_df(gmod_obj)
  model_obj <- add_markov_transition_eqns(gmod_obj, model_obj, events_df)
  #return(model_obj)
  print(model_obj)
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
        states_expanded[states_expanded == tunnel_state] <- paste0(tunnel_state, "_t", 1:n_cycles) # we need to replace severe with Severe Yr1, Severe Yr2, ... etc
      }
  }
  model_obj$states <- states
  model_obj$n_states <- length(states)
  model_obj$expanded_states <- states_expanded
  model_obj$n_expanded_states <- length(states_expanded)    
  return(model_obj)
}
add_event_info <- function(gmod_obj, model_obj){
  # retrieve states layer
  event_layer <- retrieve_layer_by_type(gmod_obj, type = "events")
  model_obj$events <- event_layer$events
  model_obj$n_events <- length(model_obj$events)
  return(model_obj) 
}

add_markov_initial_probs <- function(gmod_obj, model_obj){
  # all prob are 0, just replace the ones provided with their values
  markov_p0 <- retrieve_layer_by_type(gmod_obj, type = "initial_prob")
  init_p0 <- markov_p0$probs
  init_states <- markov_p0$states
  p0 <- rep(0, model_obj$n_states)  # empty vector
  names(p0) <- model_obj$states
  for (d in model_obj$decisions){
    model_obj$p0[[d]] <- p0
    for (i in 1:length(init_p0)){
      model_obj$p0[[d]][init_states[i]] <- init_p0[i]
    }
    model_obj$p0[[d]] <- check_prob_vector(model_obj$p0[[d]])
  }
  return(model_obj)
}



add_markov_transition_eqns <- function(gmod_obj, model_obj, events_df){
  states <- model_obj$states
  n_states <- model_obj$n_states
  # add "curr_state" ones as well here to form P_raw as a matrix
  for (decision in model_obj$decisions){
    if (model_obj$is_cycle_dep){ # array
      n_cycles <- gmod_obj$n_cycles
      cycle <- 1:n_cycles
      #cycle <- cycle_range # try vector format!
      cycles <- paste0("cycle", cycle)
      model_obj$P[[decision]] <- array("0", dim = c(n_states, n_states, n_cycles), dimnames = list(states, states, cycles))
    } else { # matrix
      model_obj$P[[decision]] <- matrix("0", nrow = n_states, ncol = n_states, dimnames = list(states, states))
    }
  for (dest in states){
    for (state in states){
      p_trans_formula <- get_prob_chain(gmod_obj, events_df, end_state = dest)
      if (state == dest){
        p_stay_formula <- get_prob_chain(gmod_obj, events_df, end_state = "curr_state")
        p_trans_formula <- paste0(p_trans_formula, "+", p_stay_formula)
        }
      #p_trans_formula <- gsub("\\bstate\\b", paste0("\"", state, "\""), p_trans_formula)
      #p_trans_formula <- gsub("\\bdecision\\b", paste0("\"", decision, "\""), p_trans_formula)
      p_trans_formula <- gsub("\\bstate\\b", paste0("'", state, "'"), p_trans_formula)
      p_trans_formula <- gsub("\\bdecision\\b", paste0("'", decision, "'"), p_trans_formula)
      if (model_obj$is_cycle_dep){ # array
        p_trans_formula <- sapply(cycle, function(x) gsub("\\bcycle\\b", paste0("cycle=",x), p_trans_formula))
        model_obj$P[[decision]][state, dest, ] <- p_trans_formula
      } else {
        model_obj$P[[decision]][state, dest] <- p_trans_formula
      }
    } # end origin state
    } # end dest state 
  } # end decision
  return(model_obj)
}


evaluate_model <- function(model_obj){
  
  eval(parse(text = model_obj$P$TrtA[1,3,5])) 
  states <- model_obj$states
  n_states <- model_obj$n_states
  #events_df <- get_event_df(gmod_obj)
  #first_event <- get_first_event(events_df)
  # vec_p_raw <- vec_p_stay <- rep("0", n_states)
  # names(vec_p_raw) <- names(vec_p_stay) <- states
  # for (state in states){
  #   vec_p_raw[state] <- get_prob_chain(gmod_obj, events_df, end_state = state)
  #   vec_p_stay[state] <- get_prob_chain(gmod_obj, events_df, end_state = "curr_state")
  # }
  
  # add "curr_state" ones as well here to form P_raw as a matrix
  for (decision in model_obj$decisions){
    model_obj[[decision]]$P_raw <- matrix("0", nrow = n_states, ncol = n_states)
    model_obj[[decision]]$P <- matrix(0, nrow = n_states, ncol = n_states)
    rownames(model_obj[[decision]]$P_raw) <- colnames(model_obj[[decision]]$P_raw) <- states
    rownames(model_obj[[decision]]$P) <- colnames(model_obj[[decision]]$P) <- states
    
    for (dest in states){
      for (state in states){
        p_trans_formula <- get_prob_chain(gmod_obj, events_df, end_state = dest)
        p_trans_value <- eval(parse(text = p_trans_formula)) 
        if (p_trans_value == 0){
          p_trans_formula <- "0"
        }
        if (state == dest){
          p_stay_formula <- get_prob_chain(gmod_obj, events_df, end_state = "curr_state")
          p_stay_value <- eval(parse(text = p_stay_formula)) 
          if (p_stay_value > 0){ # can be added to the p_transformula
            if (p_trans_value > 0){
              p_trans_formula <- paste0(p_trans_formula, "+", p_stay_formula)
            } else { # if the transformula is empty, then just keep the stay formula
              p_trans_formula <- p_stay_formula
            }
          }
        }
        model_obj[[decision]]$P[state, dest] <- eval(parse(text = p_trans_formula))
        
        p_trans_formula <- gsub("\\bstate\\b", state, p_trans_formula)
        p_trans_formula <- gsub("\\bdecision\\b", decision, p_trans_formula)
        model_obj[[decision]]$P_raw[state, dest] <- p_trans_formula
      }
    }
  }
  return(model_obj)
}



# transition array
add_markov_transition_array <- function(gmod_obj, model_obj, events_df){
  states <- model_obj$states
  n_states <- model_obj$n_states
  events_df <- get_event_df(gmod_obj)
  for (decision in model_obj$decisions){
    model_obj[[decision]]$P_raw <- matrix("0", nrow = n_states, ncol = n_states)
    model_obj[[decision]]$P <- array(0, dim = c(n_states, n_states, n_cycles), 
                                     dimnames = list(states, states, cycles))
    rownames(model_obj[[decision]]$P_raw) <- colnames(model_obj[[decision]]$P_raw) <- states
    #rownames(model_obj[[decision]]$P) <- colnames(model_obj[[decision]]$P) <- states
    
    for (dest in states){
      for (state in states){
        p_trans_formula <- get_prob_chain(gmod_obj, events_df, end_state = dest)
        p_trans_value <- eval(parse(text = p_trans_formula)) 
        if (any(p_trans_value == 0)){
          p_trans_formula <- "0"
        }
        if (state == dest){
          p_stay_formula <- get_prob_chain(gmod_obj, events_df, end_state = "curr_state")
          p_stay_value <- eval(parse(text = p_stay_formula)) 
          if (any(p_stay_value > 0)){ # can be added to the p_transformula
            if (any(p_trans_value > 0)){
              p_trans_formula <- paste0(p_trans_formula, "+", p_stay_formula)
            } else { # if the transformula is empty, then just keep the stay formula
              p_trans_formula <- p_stay_formula
            }
          }
        }
        #for (cycle in cycle_range){
          model_obj[[decision]]$P[state, dest, ] <- eval(parse(text = p_trans_formula))
        #}
        p_trans_formula <- gsub("\\bstate\\b", state, p_trans_formula)
        p_trans_formula <- gsub("\\bdecision\\b", decision, p_trans_formula)
        p_trans_formula <- gsub("\\bcycle\\b", paste0(1,":",n_cycles), p_trans_formula)
        model_obj[[decision]]$P_raw[state, dest] <- p_trans_formula
      }
    }
  }
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


#}

# markov functions

# # build transition prob matrix 
# convert_events_to_df <- function(gmod_obj){
#   event_layers <- retrieve_layer_by_type(gmod_obj, type = "event")
#   events_df <- bind_rows(event_layers)
#   return(events_df)
# }
# 
# # build transition prob from the events df
# build_trans_prob_matrix <- function(gmod_obj){
#   events_df <- convert_events_to_df(gmod_obj)
#   states <- gmod_obj$states
#   
# }


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
  bind_rows(event_layers) #%>% 
    #group_by(name) %>% 
    #mutate(with_probs = ifelse(is.infinite(with_probs), 
    #                           1 - sum(with_probs[is.finite(with_probs)]), 
    #                           with_probs)) %>% 
    #ungroup()
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
  cleaned_string <- sub("^c\\((.*)\\)$", "\\1", input_string)
  y <- strsplit(cleaned_string, ", |(?>\\(.*?\\).*?\\K(, |$))", perl = TRUE)[[1]]
  #extracted_elements <- gsub("^c\\((.*)\\)$", "\\1", input_string)
  # Split the elements by comma (,) and remove leading/trailing spaces
  #y <- trimws(unlist(strsplit(extracted_elements, ",")))
  # remove complement in "inf"
  inf_index <- y == "Inf"
  if (any(inf_index)){
    sum_others <- paste0(y[!inf_index], collapse = "+")
    y[inf_index] <- paste0("1-(", sum_others, ")")
  }
  return(y)
}
