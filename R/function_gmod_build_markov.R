

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

  # outcomes <- model_obj$outcomes
  # n_outcomes <- model_obj$n_outcomes
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
    # get probabilities of staying at the same state
    #vec_p_stay[outcome] <- get_prob_chain(gmod_obj, events_df, end_outcome = "curr_outcome")
  }
  gmod_obj <- get_prob_chain_markov(gmod_obj, events_df, end_state = "curr_state")
  
  path_df <- bind_rows(gmod_obj$path_df_list) %>% 
    inner_join(events_df, by = c("chain_id" = "id")) #%>% 
    #mutate(results = ifelse(results == "curr_state", dest, results))
  
  # add payoffs ====
  for (payoff_name in payoff_names){
    path_df[[payoff_name]] <- deparse(model_obj$payoffs[[payoff_name]])
  }
  
  # add originating states 
  states_df <- data.frame(state = states) 
  path_df0 <- path_df %>% 
    #crossing(decisions_df) %>% 
    crossing(states_df) %>% 
    mutate(dest = ifelse(dest == "curr_state", state, dest)) %>% 
    rowwise() %>% 
    mutate(across(c(probs, payoff_names), ~ gsub("\\bstate\\b", paste0("'",state,"'"), .x)))
  
  
  # deal with tunnels 
  # if there is a tunnel, loop through them and expand the state and dest columns
  path_df1 <- path_df0
  tunnel_states <- model_obj$tunnel_states
  # n <- nrow(path_df1)
  if (length(tunnel_states) > 0){
    tunnel_df <- expand.grid(state = tunnel_states, state_idx = 1:(n_cycles-1))
    path_df1 <- path_df1 %>% 
      left_join(tunnel_df, by = "state") %>% 
      #left_join(tunnel_df %>% rename(dest_idx = state_idx), by = c("dest"="state")) %>% 
      mutate(state_idx = ifelse(is.na(state_idx), 0, state_idx)) 
  } else {
    path_df1 <- path_df1 %>% 
      mutate(state_idx = 0, dest_idx = 0)
  }
  # replaces cycle_in_state with cycle_in_[STATE]=Tunnel No.
  path_df2 <- path_df1 %>% 
    mutate(across(c(probs, payoff_names), ~ gsub('cycle_in_state\\("([^"]+)"\\)', paste0("cycle_in_\\1","=",state_idx), .x)),
           dest_idx = ifelse(state == dest & state_idx > 0, state_idx + 1, 0),
           dest_idx = ifelse(state != dest & dest %in% tunnel_states, 1, dest_idx),
           state_expanded = paste0(state, ifelse(state_idx==0,"", paste0("_tnl",state_idx))), 
           dest_expanded = paste0(dest, ifelse(dest_idx==0,"",paste0("_tnl", dest_idx)))) %>% 
    filter(dest_expanded %in% states_expanded)
  # dest expanded can include some unexpanded states - these can be ignored in the evaluations
  
  # compute weighted payoffs by probabilities 
  path_df2 <- path_df2 %>% 
    mutate(across(payoff_names, ~ paste0("(", probs, ")*(", .x, ")")))
  
  # collapse chain probs and create list of all events and values
  path_df3 <- path_df2 %>% 
    pivot_wider(names_from = event, values_from = values) %>% #, values_fill = "FALSE")
    ungroup() %>%
    group_by(state_expanded, dest_expanded, path_id) %>% 
    summarize(probs = paste0("(",probs, ")",collapse = "*"), 
              across(payoff_names, ~ paste0(.x, collapse="+")), 
              across(events, ~ event_value(.x)))
  
  
  # replace event names in probabilities with event name value pairs 
  path_df3$probs <- replace_event_with_value(x = path_df3$probs, input_df = path_df3, events = events)
  
  
  # add decisions
  decisions_df <- data.frame(decision = model_obj$decisions)
  path_df4 <- path_df3 %>% 
    crossing(decisions_df) %>% 
    rowwise() %>% 
    mutate(across(c(probs, payoff_names), ~ gsub("\\bdecision\\b", paste0("'",decision,"'"), .x)))
  
  
  # add cycle column and parse if cycle dependent
  if (model_obj$is_cycle_dep){
    cycles_df <- data.frame(cycle = 1:n_cycles)
    path_df4 <- path_df4 %>% 
      crossing(cycles_df) %>% 
      rowwise() %>% 
      mutate(across(c(probs, payoff_names), ~ gsub("\\bcycle\\b", paste0("cycle=",cycle,""), .x)))
  }
  
  # group transition probs by originating state, dest state, decision and cycle
  path_df5 <- path_df4 %>% 
    ungroup() %>% 
    select(-events, -path_id, -payoff_names) %>% 
    group_by(across(-c(probs))) %>% 
    summarize(probs = paste0(probs, collapse="+"))
  # group payoffs by originating state, decision, cycle
  path_df6 <- path_df4 %>% 
    ungroup() %>% 
    select(-events, -dest_expanded, -path_id, -probs) %>% 
    group_by(across(-payoff_names)) %>% 
    summarize(across(payoff_names, ~ paste0(.x, collapse="+")))
  
  model_obj$markov_eqns <- path_df5
  model_obj$payoff_eqns <- path_df6

  return(model_obj)
}




# gmod 

tunnel_states <- function(gmod_obj){
  #gmod_obj$layers
  events_df <- get_event_df(gmod_obj)
  payoffs_layer <- retrieve_layer_by_type(gmod_obj, type = "payoffs")
  payoffs_str <- sapply(payoffs_layer$payoffs, my_deparse)
  # capture all tunnel states in probabilities
  tunnel_states <- character(0)
  probs <- events_df$probs
  # probs
  for (i in 1:length(probs)){
    matches_transitions <- str_match_all(probs[i], 'cycle_in_state\\("(.*?)"\\)')
    tunnel_states <- c(tunnel_states, matches_transitions[[1]][,2])
  }
  # probs
  for (i in 1:length(payoffs_str)){
    matches_payoffs <- str_match_all(payoffs_str[i], 'cycle_in_state\\("(.*?)"\\)')
    tunnel_states <- c(tunnel_states, matches_payoffs[[1]][,2])
  }
  tunnel_states <- unique(tunnel_states)
  
  return(tunnel_states)
}


is_cycle_dep <- function(gmod_obj){
  #gmod_obj$layers
  events_df <- get_event_df(gmod_obj)
  payoffs <- retrieve_layer_by_type(gmod_obj, type = "payoffs")
  transitions_cycle_dep <- any(grepl("\\bcycle\\b", events_df$probs)) 
  payoffs_cycle_dep <- any(grepl("\\bcycle\\b", payoffs$payoffs)) 
  any(transitions_cycle_dep, payoffs_cycle_dep)
}
