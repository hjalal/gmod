# building transition prob matrix logic =======
get_event_df <- function(gmod_obj){
  event_layers <- retrieve_layer_by_type(gmod_obj, type = "event") 
  event_df_list <- list()
  i <- 0
  for (event_layer in event_layers){
    i <- i + 1
    temp_df <- event_layers[[i]] %>% 
      as.data.frame() %>% 
      dplyr::mutate(values = as.character(values)) #%>%
      #dplyr::select(-payoffs) 
    # # process payoffs separately for each layer
    # payoffs_string <- event_layers[[i]]$payoffs 
    # if (payoffs_string == ""){
    #   # empty string
    # } else {
    #   payoffs <- payoff2liststring(payoffs_string)
    #   for (payoff_name in payoffs){
    #     temp_df[[names(payoff_name)]] <- payoff_name
    #   }
    # }
    event_df_list[[i]] <- temp_df
  }
  event_df <- dplyr::bind_rows(event_df_list) %>% 
    dplyr::mutate(id = dplyr::row_number())
  return(event_df)
}


# identify the event chain
get_first_event <- function(events_df){
  event_names <- events_df$event
  event_dest <- events_df$outcomes
  first_event <- unique(event_names[!(event_names %in% event_dest)])
  if (length(first_event) > 1){
    stop(paste(first_event, "are originating events. There must be only a single event."))
  }
  return(first_event)
}

get_prob_chain <- function(gmod_obj, events_df, end_state, is_curr_state = FALSE){
  if (is_curr_state){
    end_state_call <- "curr_state"
  } else {
    end_state_call <- end_state
  }
  # get the row id sequences for for each event chain
  event_chains <- get_event_chain_ids(events_df, outcomes_id = end_state_call)
  # convert to strings with * between each element and + between each chain
  for (event_chain in event_chains){
    gmod_obj$path_id <- gmod_obj$path_id + 1
    gmod_obj$path_df_list[[gmod_obj$path_id]] <- data.frame(
      path_id = rep(gmod_obj$path_id, length(event_chain)),
      chain_id = event_chain,
      final_outcome = end_state)
  }
  #prob_chain <- build_prob_chain(events_df, event_chains)
  #if (prob_chain == "()"){prob_chain <- "0"}
  return(gmod_obj)
}

get_prob_chain_markov <- function(gmod_obj, events_dt){
  path_id <- 0
  states_layer <- gmod:::retrieve_layer_by_type(gmod_obj, type = "states")
  states <- states_layer$states# no tunnel states
  path_df_list <- list()
  for (dest in c(states, "curr_state")){
  # get the row id sequences for for each event chain
  event_chains <- gmod:::get_event_chain_ids(events_dt, outcomes_id = dest)
  # convert to strings with * between each element and + between each chain
  for (event_chain0 in event_chains){
    event_chain <- event_chain0[event_chain0 > 0]
    path_id <- path_id + 1
    path_df_list[[path_id]] <- data.table(
      path_id = rep(path_id, length(event_chain)),
      id = event_chain,
      dest = dest)
  }
  }
  
  result_dt <- rbindlist(path_df_list, fill = TRUE)
  return(result_dt)
  #prob_chain <- build_prob_chain(events_df, event_chains)
  #if (prob_chain == "()"){prob_chain <- "0"}
}


# Function to retrieve the value 'X' based on the 'event'
get_id_with_events <- function(data, outcomes_id) {
  #return(paste0("(",data$probs[data$outcomes == outcomes_id],")"))
  return(data$id[data$outcomes == outcomes_id])
}

get_event_chain_ids <- function(data, outcomes_id) {
  events <- data$event[data$outcomes == outcomes_id]
  all_lineages <- list()
  
  for (event in events) {
    all_lineages[[length(all_lineages) + 1]] <- get_event_chain_ids(data, event)
  }
  
  if (length(events) == 0) {
    return(0) #list(as.character(outcomes_id)))
  } else {
    individual_lineages <- list()
    if (length(all_lineages)>0){
      for (i in 1:length(all_lineages)) {
        X <- get_id_with_events(data, outcomes_id)[i]
        individual_lineages <- c(
          individual_lineages, 
          lapply(all_lineages[[i]], function(x) c(x, X))
        )
      }
    } else {
      stop("An error occured in the event_mappings.  Make sure the events are correctly mapped. For more information please refer to the vignettes")
    }

    return(individual_lineages)
  }
}


# builds prob chain from event id chains
build_prob_chain <- function(events_df, event_chains){
  prob_prod_chain <- character()
  i <- 0
  for (event_chain in event_chains){
    i <- i + 1
    event_chain <- event_chains[[i]]
    event_chain <- event_chain[event_chain > 0]
    probs <- filter_probs_by_order(x = events_df$probs, id = events_df$id, sel_ids = event_chain)
    # if any has "prev_event(" in it, replace it with the value of the event=event in the same chain
    probs <- prev_event_value(events_df, probs, chain_ids = event_chain)
    prob_prod_chain[i] <- paste0("(", probs,")", collapse = "*")
  }
  event_chain_list <- unlist(prob_prod_chain)
  prob_chain <- paste(event_chain_list, collapse = "+")
  return(prob_chain)
}

# return df$X1 based on values in df$id where X1 are in the same order as id
filter_probs_by_order <- function(x, id, sel_ids){
  n <- length(sel_ids)
  if (n == 0){
    y <- "0"
  } else {
    y <- c()
    for (i in 1:n){
      y[i] <- x[id == sel_ids[i]]
    }
  }
  return(y)
}

prev_event_value <- function(events_df, probs, chain_ids){
  prev_event_ids <- grep(x = probs, pattern = "\\bprev_event\\b")
  if (length(prev_event_ids)>0){
    for (prev_event_id in prev_event_ids){
      # look forward through event ids to get the value of the event inside the parenthesis
      prev_event_ref <- probs[prev_event_id]
      pattern <- 'prev_event\\(([^\\)]+)\\)'
      extracted_strings <- regmatches(prev_event_ref, gregexpr(pattern, prev_event_ref))[[1]]
      extracted_events <- gsub('.*\\((.*)\\)', '\\1', extracted_strings)
      clean_string <- gsub("\\\"", "", extracted_events)
      replacement_string <- character()
      for (event in clean_string){
        value <- events_df$values[events_df$event==event & events_df$id %in% chain_ids]
        if (length(value)==0) {
          warning(paste("if event ", event, "doesn't have a prior event, a 0 is returned. 
                         Check if this is the intended behavior.  This is not necessarily an error."))
          value <- 0
        }
        if (length(value)>1) stop(paste(event, ": There are multiple prior events with the same name."))
        replacement_string <- c(replacement_string, paste0("prev_event(\"",event,"\"=",value,")"))
      }
      probs[prev_event_id] <- replace_prev_event(probs[prev_event_id], replacement_string)
    } # end for
  } # end if
  return(probs)
}

get_final_outcomes <- function(events_df){
  unique(events_df$outcomes[!(events_df$outcomes %in% events_df$event)])
}
get_events <- function(events_df){
  unique(events_df$event)
}

# get_events_with_payoffs_df <- function(df){
#   # Columns to check for NAs
#   col_names <- colnames(df)
#   keep_cols <- c("type", "event", "values", "outcomes", "probs","id")
#   columns_to_check <- col_names[!(col_names %in% keep_cols)]  # Specify columns here
#   
#   # Exclude rows with NA values in all specified columns
#   df[!apply(df[columns_to_check], 1, function(row) all(is.na(row)|row=="NA"|row==0)), ]
# }

