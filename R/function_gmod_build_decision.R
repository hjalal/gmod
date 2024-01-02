# if class is Decision
gmod_build.gmod_decision <- function(gmod_obj){
  # here we will have an environment to parse the gmod_object
  model_obj <- list()
  model_obj <- add_decision_info(gmod_obj, model_obj)
  model_obj <- event_mapping_info(gmod_obj, model_obj)
  model_obj <- add_outcome_info(gmod_obj, model_obj)
  model_obj <- add_event_info(gmod_obj, model_obj)
  model_obj <- add_payoffs(gmod_obj, model_obj)
  # adding stirng equations 
  model_obj <- add_decision_eqns(gmod_obj, model_obj)
  class(model_obj) <- "gmod_decision"
  return(model_obj)
}


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


gmod_evaluate.gmod_decision <- function(model_num_struc){
  payoffs <- model_num_struc$payoff_names
  decisions <- model_num_struc$decisions
  n_payoffs <- model_num_struc$n_payoffs
  n_decisions <- model_num_struc$n_decisions
  events <- model_num_struc$events
  model_results <- model_num_struc #list()
  mat_outcome_payoffs <- matrix(0, nrow = n_decisions, ncol = n_payoffs, dimnames = list(decisions, payoffs))
  
  for (decision in decisions){
    for (payoff in payoffs){
      mat_outcome_payoffs[decision, payoff] <- 
        matrix(model_num_struc$Outcome_payoffs[[payoff]][[decision]], nrow = 1) %*% 
        matrix(model_num_struc$P[[decision]], ncol = 1)
    } # end payoffs
  } # end decision
  model_results$Outcome_payoff_summary <- mat_outcome_payoffs
  
  # Event payoffs 
  mat_event_payoffs <- matrix(0, nrow = n_decisions, ncol = n_payoffs, 
                              dimnames = list(decisions, payoffs))
  
  for (payoff in payoffs){
    event_payoffs <- model_num_struc$Event_payoffs[[payoff]] #[[event]]
    if (!is.null(event_payoffs)){
      temp_mat <- 0
      for (event in events){
        # compute event rewards = p_event*event_payoff
        temp_mat <- temp_mat + model_num_struc$event_prop[[event]] * 
          model_num_struc$Event_payoffs[[payoff]][[event]]
      } # end event
      mat_event_payoffs[,payoff] <- colSums(temp_mat)
    }
  } # end payoffs
  if (sum(mat_event_payoffs)!=0){
    model_results$Event_payoff_summary <- mat_event_payoffs
  }
  model_results$Overall_summary <- mat_outcome_payoffs + mat_event_payoffs
  return(model_results)
}