

add_markov_info <- function(gmod_obj, model_obj){
  # retrieve states layer
  states_layer <- retrieve_layer_by_type(gmod_obj, type = "states")
  states <- states_layer$states# no tunnel states
  n_cycles <- gmod_obj$n_cycles
  
  tunnel_states <- model_obj$tunnel_states
  tunnel_lengths <- model_obj$tunnel_lengths
  
  states_expanded <- character(0)
  for (state in states){
    if (state %in% tunnel_states){
      tunnel_length <- tunnel_lengths[tunnel_states == state]
      for (j in 1:tunnel_length){
        states_expanded <- c(states_expanded, paste0(state, "_tnl", j))
      }
    } else {
      states_expanded <- c(states_expanded, state)
    }
  }
  
  model_obj$states <- states
  model_obj$n_states <- length(states)
  model_obj$states_expanded <- states_expanded
  model_obj$n_states_expanded <- length(states_expanded)    
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
    if (length(init_p0)>0){
      for (i in 1:length(init_p0)){
        state <- init_states[i]
        if (state %in% tunnel_states){
          state <- paste0(state, "_tnl1")
        }
        model_obj$p0[[d]][state] <- init_p0[i]
      }
    } else {
      stop("Initial probabilities must be specified using initial_probs() function. See vignettes.")
    }

    model_obj$p0[[d]] <- check_prob_vector(model_obj$p0[[d]])
  }
  return(model_obj)
}


add_markov_eqns <- function(gmod_obj, model_obj, events_df, simplify = FALSE){
  states <- model_obj$states
  #n_states <- model_obj$n_states
  states_expanded <- model_obj$states_expanded
  n_states_expanded <- length(states_expanded)
  tunnel_states <- model_obj$tunnel_states
  #n_cycles <- gmod_obj$n_cycles
  # get expanded states 

  # final_outcomes <- model_obj$final_outcomes
  # n_final_outcomes <- model_obj$n_final_outcomes
  events <- model_obj$events
  n_events <- model_obj$n_events
  events_df <- get_event_df(gmod_obj)
  first_event <- get_first_event(events_df)
  
  payoffs <- model_obj$payoffs
  payoff_names <- names(payoffs)
  
  gmod_obj$path_id <- 0
  gmod_obj$path_df_list <- list()
  
  for (dest in states){
    # get probabililites of transitioning to each state
    gmod_obj <- get_prob_chain_markov(gmod_obj, events_df, end_state = dest)
  }
  gmod_obj <- get_prob_chain_markov(gmod_obj, events_df, end_state = "curr_state")
  
  path_df <- dplyr::bind_rows(gmod_obj$path_df_list) %>% 
    dplyr::inner_join(events_df, by = c("chain_id" = "id")) 
  
  # collapse chain probs and create list of all events and values
  path_df1 <- path_df %>% 
    tidyr::pivot_wider(names_from = event, values_from = values) %>% #, values_fill = "FALSE")
    dplyr::group_by(dest, path_id) %>% 
    dplyr::summarize(probs = paste0("(",probs, ")",collapse = "*"), 
                     dplyr::across(events, ~ event_value(.x)))
  
  # add payoffs ====
  for (payoff_name in payoff_names){
    path_df1[[payoff_name]] <- deparse(model_obj$payoffs[[payoff_name]])
  }
  
  # replace events with their values 
  path_df1$probs <- replace_event_with_value(x = path_df1$probs, input_df = path_df1, events = events)
  for (payoff_name in payoff_names){
    path_df1[[payoff_name]] <- replace_event_with_value(x = path_df1[[payoff_name]], input_df = path_df1, events = events)
  }
  
  # aggregate over destination states 
  # try without aggregation - aggregate probs in the gmod_gen function
  # keeps curr_state 
  path_df2 <- path_df1 %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(dplyr::across(payoff_names, ~ paste0(probs, "*", .x))) %>% 
    dplyr::group_by(dest) %>% 
    dplyr::summarize(dplyr::across(c(probs, payoff_names), ~ paste0(.x, collapse="+")))
    # 
  model_obj$model_equations <- path_df2
  return(model_obj)
}




# gmod 

add_tunnels <- function(gmod_obj, model_obj){
  tunnel_layer <- retrieve_layer_by_type(gmod_obj, type = "tunnels")
  states <- tunnel_layer$states
  lengths <- tunnel_layer$lengths
  # if (is.null(lengths)){
  #   lengths <- rep(gmod_obj$n_cycles, length(states))
  # } else if(any(is.na(lengths))){
  #   na_id <- is.na(lengths)
  #   lengths[na_id] <- gmod_obj$n_cycles
  # } 
  model_obj$tunnel_states <- states
  model_obj$tunnel_lengths <- lengths
  names(model_obj$tunnel_lengths) <- states
  return(model_obj)
}



is_cycle_dep <- function(gmod_obj){
  #gmod_obj$layers
  events_df <- get_event_df(gmod_obj)
  payoffs <- retrieve_layer_by_type(gmod_obj, type = "payoffs")
  transitions_cycle_dep <- any(grepl("\\bcycle\\b", events_df$probs)) 
  payoffs_cycle_dep <- any(grepl("\\bcycle\\b", payoffs$payoffs)) 
  any(transitions_cycle_dep, payoffs_cycle_dep)
}
