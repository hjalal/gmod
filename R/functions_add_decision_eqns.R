# main decision model file to add all equations to object 
# adds all equations to object
add_decision_eqns <- function(gmod_obj, model_obj, simplify = FALSE){
  final_outcomes <- model_obj$final_outcomes
  n_final_outcomes <- model_obj$n_final_outcomes
  events <- model_obj$events
  n_events <- model_obj$n_events
  events_df <- get_event_df(gmod_obj)
  first_event <- get_first_event(events_df)

  decisions <- data.frame(decision = model_obj$decisions)
  
  payoffs <- model_obj$payoffs
  payoff_names <- names(payoffs)
  
  
  gmod_obj$path_id <- 0
  gmod_obj$path_df_list <- list()
  
  for (final_outcome in final_outcomes){
    gmod_obj <- get_prob_chain(gmod_obj, events_df, end_state = final_outcome)
    #vec_p_stay[final_outcome] <- get_prob_chain(gmod_obj, events_df, end_final_outcome = "curr_final_outcome")
  }
  path_df <- dplyr::bind_rows(gmod_obj$path_df_list) %>% 
    dplyr::inner_join(events_df, by = c("chain_id" = "id"))
  

  path_df1 <- path_df %>% tidyr::crossing(decisions) %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(probs = gsub("\\bdecision\\b", paste0("'",decision,"'"), probs))
  # deal with prev_event() 
  path_df1_1 <- path_df1 %>% 
    tidyr::pivot_wider(names_from = event, values_from = values)
  
  
  # collapse chain probs and create list of all events and values
  path_df2 <- path_df1_1 %>% 
    dplyr::ungroup() %>%
    dplyr::group_by(decision, final_outcome, path_id) %>% 
    dplyr::summarize(probs = paste0("(",probs, ")",collapse = "*"), 
              dplyr::across(events, ~ event_value(.x)))
  
  # replace event names in probabilities with event name value pairs 
  path_df2$probs <- replace_event_with_value(x = path_df2$probs, input_df = path_df2, events = events)
  
  # add payoff formula
  for (payoff_name in payoff_names){
    path_df2[[payoff_name]] <- paste0(deparse(payoffs[[payoff_name]]), collapse = "")
    for (i in 1:nrow(path_df2)){
      path_df2[[payoff_name]][i] <- gsub("\\bdecision\\b", paste0("'",path_df2$decision[i],"'"), path_df2[[payoff_name]][i])
      path_df2[[payoff_name]][i] <- gsub("\\bfinal_outcome\\b", paste0("'",path_df2$final_outcome[i],"'"), path_df2[[payoff_name]][i])
    }      
    path_df2[[payoff_name]] <- replace_event_with_value(x = path_df2[[payoff_name]], input_df = path_df2, events = events)
  }
  model_obj$final_outcome_formulae <- path_df2
  
  
  # multiply final_outcomes by probabilities and aggregate by decision
  path_df3 <- path_df2 %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(dplyr::across(payoff_names, ~ paste0(probs, "*", .x)))
  
  if (simplify){
    path_df3 <- path_df3 %>%
      dplyr::rowwise() %>% 
      dplyr::mutate(prob_value = eval(parse(text = probs)))  %>%  # filter out probs that are evaluate to 0.
      dplyr::filter(prob_value != 0) %>% 
      dplyr::mutate(probs = ifelse(prob_value==1, "1", probs)) %>%
      dplyr::select(-prob_value)
  }
  
  # aggregate by decision
  path_df4 <- path_df3 %>% 
    dplyr::ungroup() %>% 
    dplyr::group_by(decision) %>% 
    dplyr::summarize(dplyr::across(payoff_names, ~ paste0(.x, collapse = "+"))) #\n\t")))
  
  model_obj$summary_formulae <- path_df4
  return(model_obj)
}


event_value <- function(x, default_na_value = "FALSE"){
  if(all(is.na(x))){
    default_na_value
    #"NA" #returns NA if the event is missing
  } else {
    unique(stats::na.omit(x))
  }
}


replace_event_with_value <- function(x, input_df, events){
  n <- nrow(input_df)
  for (event in events){
    for (i in 1:n){
      #x[i] <- gsub(paste0("\\b", event, "\\b"), paste0(event, "=\"", input_df[i,event]), "\"", x[i])
      x[i] <- gsub(paste0("\\b", event, "\\b"), paste0(event, "=", is_numeric_or_logical(input_df[i,event])), x[i])
    }
  }
  return(x)
}

is_numeric_or_logical <- function(input_string){
  if ((input_string %in% c("FALSE", "F", "TRUE", "T")) | grepl("^-?\\d*\\.?\\d+$", input_string)){
    input_string
  } else {
    paste0("'", input_string, "'")
  }
}
