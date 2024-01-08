# basic gmod info
add_decision_info <- function(gmod_obj, model_obj){
  # retrieve states layer
  decision_layer <- retrieve_layer_by_type(gmod_obj, type = "decisions")
  model_obj$decisions <- decision_layer$decisions
  model_obj$n_decisions <- length(model_obj$decisions)
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

add_outcome_info <- function(gmod_obj, model_obj){
  # retrieve states layer
  #outcomes_layer <- retrieve_layer_by_type(gmod_obj, type = "outcomes")
  events_df <- get_event_df(gmod_obj)
  model_obj$outcomes <- get_outcomes(events_df)
  model_obj$n_outcomes <- length(model_obj$outcomes)
  return(model_obj)
}
add_event_info <- function(gmod_obj, model_obj){
  # retrieve states layer
  #events_layer <- retrieve_layer_by_type(gmod_obj, type = "events")
  events_df <- get_event_df(gmod_obj)
  model_obj$events <- get_events(events_df)
  model_obj$n_events <- length(model_obj$events)
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

retrieve_layer_by_type <- function(gmod_obj, type){
  # Use lapply to filter the list based on the condition
  result <- lapply(gmod_obj$layers, function(x) if (x$type == type) x else NULL)
  # Remove NULL elements from the list
  lyr<-Filter(Negate(is.null), result)
  if (length(lyr)==1 & type != "event"){
    lyr <- lyr[[1]] #only select the first element if there is no more
  }
  lyr
}
