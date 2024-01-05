#' Parses a gmod model formula structure
#'
#' @param gmod_obj a gmod object containing the equations for the Markov model
#' @param params a list containing the model parameters
#' @description parses the string formulae and computes the numerical results
#' @return a nuemerical gmod object structure 
#' @export
#' 
#' @examples gmod_parse(mygmod, params = list(param1 = 0.5, param2 = 0.6))
gmod_parse <- function(x, ...) UseMethod("gmod_parse")


#' Parses a gmod Markov object 
#' @description given gmod equations and a list of parameters it evaluates the model equations and returns the numerical values
#'
#' @param model_struc a gmod representing the model structure
#' @param params a parameter list containing the model parameters
#'
#' @return model_num_struc
#' @export
#'
#' @examples gmod_parse(mygmod)
#' @examples gmod_parse(mygmod, params = list(param1 = 0.5, param2 = 0.6))
gmod_parse.gmod_markov <- function(model_struc, params = NULL){
  if(is.null(params)){
    warning("No parameters were provided. Will use the parameters from the global environment. 
            If instead you want to evaluate the model with specific parameter values, please provide 
            them here as list.")
  } else {
    list2env(params)
  }
  # for Markov structure, parse P, p0, Payoffs, event_payoff and replace
  model_num_str <- model_struc
  decisions <- model_struc$decisions
  events <- model_struc$events
  event_with_payoffs_df <- model_struc$events_with_payoffs_df
  for (decision in decisions){
    model_num_str$p0[[decision]] <- parse_object(model_struc$p0[[decision]])
    model_num_str$P[[decision]] <- parse_object(model_struc$P[[decision]])
    for (payoff in model_struc$payoff_names){
      model_num_str$State_payoffs[[payoff]][[decision]] <- 
        parse_object(model_struc$State_payoffs[[payoff]][[decision]])
      # initialize overall payoffs as the state payoffs
      model_num_str$Overall_payoff[[payoff]][[decision]] <- model_num_str$State_payoffs[[payoff]][[decision]] 
      for (i in 1:nrow(event_with_payoffs_df)){
        event <- event_with_payoffs_df$event[i]
        event_value <- event_with_payoffs_df$values[i]
        event_payoff <- model_struc$Event_payoff[[payoff]][[event]][[event_value]][[decision]]
        if (!is.null(event_payoff)){ # only a single type of payoff may be available
          model_num_str$Event_payoff[[payoff]][[event]][[event_value]][[decision]] <- 
            parse_object(event_payoff)
          model_num_str$Overall_payoff[[payoff]][[decision]] <-
            model_num_str$Overall_payoff[[payoff]][[decision]] + #overall payoffs
            model_num_str$Event_payoff[[payoff]][[event]][[event_value]][[decision]] #event payoffs
        } # end is null event_payoff
      } # end rows of events with payoffs
      
    } # end payoffs
  } # end decisions
  #for (event in events){
  #  model_num_str$event_prop[[event]] <- parse_object(model_struc$event_prop[[event]])
  #} # end event
  class(model_num_str) <- "gmod_markov"
  return(model_num_str)
}

#' Parses a gmod Decision Tree object 
#' @description given gmod equations and a list of parameters it evaluates the model equations and returns the numerical values
#'
#' @param model_struc a gmod representing the model structure
#' @param params a parameter list containing the model parameters
#'
#' @return model_num_struc
#' @export
#'
#' @examples gmod_parse(mygmod)
#' @examples gmod_parse(mygmod, params = list(param1 = 0.5, param2 = 0.6))
gmod_parse.gmod_decision <- function(model_struc, params = NULL){
  if(is.null(params)){
    warning("No parameters were provided. Will use the parameters from the global environment. 
            If instead you want to evaluate the model with specific parameter values, please provide 
            them here as list.")
  } else {
    list2env(params)
  }
  # for Decison structure, parse P and Payoffs 
  model_num_str <- model_struc
  decisions <- model_struc$decisions
  events <- model_struc$events
  for (decision in decisions){
    model_num_str$P[[decision]] <- parse_object(model_struc$P[[decision]])
    for (payoff in model_struc$payoff_names){
      model_num_str$Outcome_payoffs[[payoff]][[decision]] <- 
        parse_object(model_struc$Outcome_payoffs[[payoff]][[decision]])
    }
  }
  for (event in events){
    # Event prop
    model_num_str$event_prop[[event]] <- parse_object(model_struc$event_prop[[event]])
    # Event payoffs
    for (payoff in model_struc$payoff_names){
      event_payoff <- model_struc$Event_payoffs[[payoff]][[event]]
      if(!is.null(event_payoff)){
        temp_mat <- parse_object(event_payoff)
        temp_mat[is.na(temp_mat)] <- 0
        model_num_str$Event_payoffs[[payoff]][[event]] <- temp_mat
      } # end of event payoff loop
    } # end payoff
  } # end event
  class(model_num_str) <- "gmod_decision"
  return(model_num_str)
  # 
}

