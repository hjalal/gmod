# main decision model file to add all equations to object 
# adds all equations to object
add_decision_eqns <- function(gmod_obj, model_obj){
  outcomes <- model_obj$outcomes
  n_outcomes <- model_obj$n_outcomes
  events <- model_obj$events
  n_events <- model_obj$n_events
  events_df <- get_event_df(gmod_obj)
  first_event <- get_first_event(events_df)
  
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
    model_obj$P[[decision]] <- rep("0", n_outcomes)
    names(model_obj$P[[decision]]) <- outcomes
    # iterate through payoffs
    for (payoff_name in payoff_names){
      model_obj$Outcome_payoffs[[payoff_name]][[decision]] <- rep("0", n_outcomes)
      names(model_obj$Outcome_payoffs[[payoff_name]][[decision]]) <- outcomes
    }
    for (outcome in outcomes){
      p_trans_formula <- get_prob_chain(gmod_obj, events_df, end_state = outcome)
      #p_trans_formula <- gsub("\\bstate\\b", state, p_trans_formula)
      p_trans_formula <- gsub("\\bdecision\\b", paste0("'",decision,"'"), p_trans_formula)
      model_obj$P[[decision]][outcome] <- p_trans_formula
      for (payoff_name in payoff_names){
        payoff_formula <- paste0(deparse(payoffs[[payoff_name]]), collapse = "")
        payoff_formula <- gsub("\\bdecision\\b", paste0("'",decision,"'"), payoff_formula)
        payoff_formula <- gsub("\\outcome\\b", paste0("'",outcome,"'"), payoff_formula)
        model_obj$Outcome_payoffs[[payoff_name]][[decision]][outcome] <- payoff_formula
      } # end payoffs 
    } # end outcome
    for (event in events){
      event_values <- events_df$values[events_df$event == event]
      n_values <- length(event_values)
      if (is.null(model_obj$event_prop[[event]])){
        model_obj$event_prop[[event]] <- matrix(0, nrow = n_values, ncol = model_obj$n_decisions, 
                                                dimnames = list(event_values, model_obj$decisions))
      }
      for (event_value in event_values){
        # exclude rows where selected event has other values 
        sel_events_df <- events_df[!(events_df$event == event & events_df$value != event_value),]
        end_state <- sel_events_df$results[sel_events_df$event == event]
        if (length(end_state) != 1){
          stop(paste("results of event",event,"is none or not unique:", end_state))
        }
        p_event_formula <- get_prob_chain(gmod_obj, sel_events_df, end_state = end_state)
        p_event_formula <- gsub("\\bdecision\\b", paste0("'",decision,"'"), p_event_formula)
        model_obj$event_prop[[event]][event_value, decision] <- p_event_formula
        
        # Add Event Payoffs 
        for (payoff_name in payoff_names){
          if(!is.null(sel_events_df[[payoff_name]])){ 
            event_payoff_formula <- sel_events_df[[payoff_name]][sel_events_df$event == event & sel_events_df$value == event_value]
            event_payoff_formula <- gsub("\\bdecision\\b", paste0("'",decision,"'"), event_payoff_formula)
            # assign values to the matrix
            if (is.null(model_obj$Event_payoffs[[payoff_name]][[event]])){
              model_obj$Event_payoffs[[payoff_name]][[event]] <- matrix(0, nrow = n_values, ncol = model_obj$n_decisions, 
                                                                        dimnames = list(event_values, model_obj$decisions))
            }
            model_obj$Event_payoffs[[payoff_name]][[event]][event_value, decision] <- event_payoff_formula
          } # end of is.null payoff
        } # end payoff name
      } # end event value
    } # end events 
  } # end decision
  return(model_obj)
}
