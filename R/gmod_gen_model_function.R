#' Runs a decision tree or Markov model
#' @description runs the decision tree or markov model and returns the outcomes either traces and summary outcomes
#' @param model_num_struc a matrix containing the numerical gmod object from the gmod_parse() function
#'
#' @return
#' @export
#'
#' @examples gmod_evaluate(numerical_model_structure)
gmod_gen_model_function <- function(x, ...) UseMethod("gmod_gen_model_function")

#' Runs the markov model
#' @description runs the markov model and returns the traces and summary outcomes
#' @param model_struc a matrix containing the numerical gmod object from the gmod_parse() function
#' @export
#' 
#' @return
#'
#' @examples gmod_evaluate(numerical_model_structure)
gmod_gen_model_function.gmod_markov <- function(model_struc, model_function_name = "my_markov_model", print_model_function = FALSE){
  
  model_function_name = "my_markov_model"
  print_model_function = T
  
  model_lines <- paste0(model_function_name, "<- function(params=NULL,return_payoffs=FALSE,return_trace=FALSE,return_transition_prob=FALSE, return_detailed_outcomes=FALSE){")
  model_lines <- c(model_lines, "if (!is.null(params)) list2env(params, envir=.GlobalEnv)")
  #model_lines <- c(model_lines, "attach(params)")
  #model_lines <- c(model_lines, "create_variables(params, envir = environment())")
  #model_lines <- c(model_lines, "print(r_HS1)")
  #model_lines <- c(model_lines, "print(environment())")
  
  # is model cycle dep? 
  is_cycle_dep <- model_struc$is_cycle_dep
  model_equations <- model_struc$model_equations
  # loop through each row: 
  tunnel_states <- model_struc$tunnel_states
  # get row dependencies 
  
  decisions <- model_struc$decisions
  model_lines <- c(model_lines, paste0("decisions <- model_struc$decisions"))
  model_lines <- c(model_lines, paste0("tunnel_lengths <- model_struc$tunnel_lengths"))
  
  model_lines <- c(model_lines, "n_decisions <- model_struc$n_decisions")
  model_lines <- c(model_lines, "n_cycles <- model_struc$n_cycles")
  model_lines <- c(model_lines, "cycles <- 1:n_cycles")
  #model_lines <- c(model_lines, "cycle_names <- paste0('cycle_', cycles)")
  states <- model_struc$states
  n_states <- model_struc$n_states
  model_lines <- c(model_lines, "states_expanded <- model_struc$states_expanded")
  model_lines <- c(model_lines, "n_states_expanded <- model_struc$n_states_expanded")
  
  payoff_names <- model_struc$payoff_names
  model_lines <- c(model_lines, paste0("payoff_names <- model_struc$payoff_names"))
  
  n_payoffs <- model_struc$n_payoffs
  model_lines <- c(model_lines,"discounts <- model_struc$discounts") # a named vector with discount rates
  
  
  # initialize the transition array and payoff arrays 
  # follow the same order of dimensions: payoffs -> decisions -> cycles -> state1 -> state2
  if (is_cycle_dep){
    model_lines <- c(model_lines, "P <- array(0, dim = c(n_states_expanded, n_states_expanded, n_cycles, n_decisions), dimnames = list(states_expanded,states_expanded, cycles, decisions ))")
    model_lines <- c(model_lines, "Payoff <- array(0, dim = c(n_states_expanded, n_cycles, n_decisions,n_payoffs), dimnames = list(states_expanded, cycles, decisions, payoff_names))")
  } else {
    model_lines <- c(model_lines, "P <- array(0, dim = c(n_states_expanded, n_states_expanded, n_decisions), dimnames = list(states_expanded,states_expanded, decisions))")
    model_lines <- c(model_lines, "Payoff <- array(0, dim = c(n_states_expanded, n_decisions,n_payoffs), dimnames = list(states_expanded, decisions, payoff_names))")
  }
  
  # iterate throught the equation lines and send them to the model text builder 
  for (i in 1:nrow(model_equations)){
    # get prob dependencies 
    dest <- model_equations$dest[i]
    
    if(dest %in% tunnel_states) dest <- paste0(dest, "_tnl1")
    dest <- paste0("'",dest, "'")
    prob <- model_equations$probs[i]
    is_decision <- grepl("\\bdecision\\b", prob)
    is_state <- grepl("\\bstate\\b", prob)
    is_cycle <- grepl("\\bcycle\\b", prob)
    is_tunnel <- grepl("\\bcycle_in_state\\b", prob)
    # is the line a function of state? 
    model_lines <- c(model_lines, paste0("\n# destination:", dest, "; eqns: ", prob))
    model_lines <- c(model_lines, paste0("dest<-", dest))
    
    # if the destination is a tunnel state, then use it with state_tnl1
    if(is_decision){
      model_lines <- c(model_lines, "for (decision in decisions){")
      decision_str <- "decision"
    } else decision_str <- ""
    if(is_cycle){
      model_lines <- c(model_lines, "for (cycle in cycles){")
      cycle_str <- "cycle"
    } else cycle_str <- ""
    if(is_state){
      model_lines <- c(model_lines, "for (state_expanded in states_expanded){")
      state_str <- "state_expanded"
    } else state_str <- ""
    if(is_tunnel){
      if (!is_state){
        stop("cycle_in_state cannot be used without state.  If cycle_in_state is passed as an argumetn for a user-defined function, state must also be passed as an argument")
      }
      model_lines <- c(model_lines, "state_comp <- tunnel2state(state_expanded)")
      model_lines <- c(model_lines, "state <- state_comp$state")
      model_lines <- c(model_lines, "cycle_in_state <- state_comp$cycle_in_state")
    } 
    if(dest=="'curr_state'"){ # if dest == current state, then add prob to existing prob
      dest <- "state_expanded"
      model_lines <- c(model_lines, "if (state %in% tunnel_states & cycle_in_state < tunnel_lengths[state]){")
      model_lines <- c(model_lines, "dest <- paste0(state,'_tnl', cycle_in_state+1)")
      model_lines <- c(model_lines, "} else dest <- state_expanded")
      model_lines <- c(model_lines, paste0("P[", state_str, ", dest,", cycle_str, ",", decision_str, "] <- P[", state_str, ", dest,", cycle_str, ",", decision_str, "] + ", prob))
    } else { # if dest != current state, then just use the prob since dest are unique
      model_lines <- c(model_lines, paste0("P[", state_str, ", dest,", cycle_str, ",", decision_str, "] <- ", prob))
    }
    
    
    if(is_state) model_lines <- c(model_lines, "} # end state")
    if(is_cycle) model_lines <- c(model_lines, "} # end cycle")
    if(is_decision) model_lines <- c(model_lines, "} # end decisions")
    
  }
  
  # Add payoff code ===== 
  
  # iterate throught the equation lines and send them to the model code builder
  for (payoff_name in payoff_names){
    model_lines <- c(model_lines, paste0("\n #State Payoffs:", payoff_name))
    
    # if the destination is a tunnel state, then use it with state_tnl1
    if(is_decision){
      model_lines <- c(model_lines, "for (decision in decisions){")
      decision_str <- "decision"
    } else decision_str <- ""
    if(is_cycle){
      model_lines <- c(model_lines, "for (cycle in cycles){")
      cycle_str <- "cycle"
    } else cycle_str <- ""
    if(is_state){
      model_lines <- c(model_lines, "for (state_expanded in states_expanded){")
      state_str <- "state_expanded"
    } else state_str <- ""
    if(is_tunnel){
      if (!is_state){
        stop("cycle_in_state cannot be used without state.  If cycle_in_state is passed as an argumetn for a user-defined function, state must also be passed as an argument")
      }
      model_lines <- c(model_lines, "state_comp <- tunnel2state(state_expanded)")
      model_lines <- c(model_lines, "state <- state_comp$state")
      model_lines <- c(model_lines, "cycle_in_state <- state_comp$cycle_in_state")
    } 
    
    for (i in 1:nrow(model_equations)){
      # get new_payoff dependencies 
      dest <- model_equations$dest[i]
      
      new_payoff <- model_equations[[payoff_name]][i]
      is_decision <- grepl("\\bdecision\\b", new_payoff)
      is_state <- grepl("\\bstate\\b", new_payoff)
      is_cycle <- grepl("\\bcycle\\b", new_payoff)
      is_tunnel <- grepl("\\bcycle_in_state\\b", new_payoff)
      # is the line a function of state? 
      model_lines <- c(model_lines, paste0("# destination:", dest))
      #model_lines <- c(model_lines, paste0("dest<-", dest))
      
      # cumulate payoffs across all destinations
      # if dest is the same as state, then add payoff to the same state 
      #if(dest=="curr_state"){
      if (i == 1){
        model_lines <- c(model_lines, paste0("Payoff[", state_str, ",", cycle_str, ",", decision_str,",'", payoff_name,"'] <- ", new_payoff, "+"))
      } else if (i == nrow(model_equations)){
        model_lines <- c(model_lines, new_payoff)
      } else {
        model_lines <- c(model_lines, paste0(new_payoff, "+"))
      }
      #} else {
      #  model_lines <- c(model_lines, paste0("Payoff[", state_str, ",", cycle_str, ",", decision_str,",'", payoff_name,"'] <- ", new_payoff))
      #}
    } # end model equations loop
    
    #model_lines <- c(model_lines, paste0("Payoff[", state_str, ",", cycle_str, ",", decision_str,",'", payoff_name,"'] <- Payoff[", state_str, ",", cycle_str, ",", decision_str,",'", payoff_name,"'] + ", new_payoff))
    
    if(is_state) model_lines <- c(model_lines, "} # end state")
    if(is_cycle) model_lines <- c(model_lines, "} # end cycle")
    if(is_decision) model_lines <- c(model_lines, "} # end decisions")
    
  } # end payoff_names loop
  
  
  
  # add calculation code ======
  model_lines <- c(model_lines, paste0("\n # Model runs", payoff_name))
  
  
  model_lines <- c(model_lines, "} # end function")
  model_string <- paste(model_lines, collapse = "\n")
  
  if (print_model_function){
    cat(model_string)
  }
  
  
  
  
  
  
  
  
  new_func <- eval(parse(text = model_string)) # generates the function
  
  # Assign the new function to the global environment
  assign(model_function_name, new_func, envir = .GlobalEnv)
  cat(paste0("\n\n\033[94mNote:Model function ", model_function_name, 
             " is generated. It can be run by calling it directly:\n", model_function_name, 
             "(params,return_payoffs=FALSE,return_trace=FALSE,return_transition_prob=FALSE,return_detailed_outcomes=FALSE)\033[0m\n"))
  return(TRUE)
  
  
  # iterate through the equations and populate P
  state <- markov_eqns$state_expanded[i]
  dest <- markov_eqns$dest_expanded[i]
  decision <- markov_eqns$decision[i]
  prob <- markov_eqns$probs[i]
  if (is_cycle_dep){
    cycle <- markov_eqns$cycle[i]
    model_lines <- c(model_lines, paste0("P[['", decision, "']]['",state, "','", dest,"','", cycle,"'] <- ", prob))
  } else {
    model_lines <- c(model_lines, paste0("P[['", decision, "']]['",state, "','", dest,"'] <- ", prob))
  }
  #}
  return(model_lines)
  
  model_lines <- c(model_lines, paste0("model_results <- list()"))
  # construct P 
  model_lines <- construct_P_str(model_struc, is_cycle_dep, model_lines = model_lines)
  
  
  for (decision in decisions){
    model_lines <- c(model_lines, paste0("Trace <- matrix(NA, nrow = ", 
                                         n_cycles,",ncol = ",n_states_expanded,
                                         ", dimnames=list(cycles,states_expanded))"))
    model_lines <- c(model_lines, paste0("Trace[1, ] <- model_struc$p0[['",decision,"']]"))
    model_lines <- c(model_lines, paste0("for (i in 2:",n_cycles,"){"))
    if (is_cycle_dep){ # use array syntax
      model_lines <- c(model_lines, paste0("P_temp <- P[['",decision,"']][,,i]"))
    } else { #use matrix syntax
      model_lines <- c(model_lines, paste0("P_temp <- P[['",decision,"']]"))
    }
    model_lines <- c(model_lines, "Trace[i,] <- Trace[i-1,] %*% P_temp")
    model_lines <- c(model_lines, "}")
    model_lines <- c(model_lines, paste0("model_results$Trace[['",decision,"']] <- Trace"))
  } # end decision
  
  # Add payoffs multiplied by traces '
  model_lines <- construct_Payoff_str(model_struc, model_lines = model_lines)  
  model_lines <- c(model_lines, paste0("cycle_ones <- matrix(1,nrow=", n_cycles,", ncol=1)"))
  model_lines <- c(model_lines, paste0("state_ones <- matrix(1,nrow=1, ncol=", n_states_expanded, ")"))
  model_lines <- c(model_lines, paste0("mat_summary <- matrix(0, nrow=", n_decisions,",ncol=", n_payoffs,",dimnames=list(decisions,payoff_names))"))
  for (decision in decisions){
    for (payoff_name in payoff_names){
      model_lines <- c(model_lines, paste0("mat_discounts <- matrix(1/(1+discounts['",payoff_name,"'])^(cycles-1),ncol=1)%*%state_ones"))
      model_lines <- c(model_lines, paste0("Payoff_temp <- Payoff[['",payoff_name,"']][['",decision,"']]"))
      if (!is_cycle_dep){ # if model is not cycle dependent, we will get a single vector of values
        model_lines <- c(model_lines, "Payoff_temp <- cycle_ones %*% Payoff_temp")
      } 
      model_lines <- c(model_lines, paste0("R <- model_results$Trace[['",decision,"']] * Payoff_temp * mat_discounts"))
      model_lines <- c(model_lines, paste0("model_results$Results_detailed[['",payoff_name,"']][['",decision,"']] <- R"))
      model_lines <- c(model_lines, paste0("mat_summary['",decision, "','", payoff_name,"'] <- sum(R)"))
    } # end payoff
    
  } # end decision
  
  #model_lines <- c(model_lines, "detach(params)")
  
  # suppress final_outcomes, if only the summary outcomes is needed
  model_lines <- c(model_lines, "if(return_payoffs) model_results$Payoff <- Payoff")
  model_lines <- c(model_lines, "if(!return_trace) model_results$Trace <- NULL")
  model_lines <- c(model_lines, "if(!return_detailed_outcomes) model_results$Results_detailed <- NULL")
  model_lines <- c(model_lines, "if(return_transition_prob) model_results$P <- P")
  
  model_lines <- c(model_lines, "model_results$Summary <- mat_summary")
  model_lines <- c(model_lines, "return(model_results)")
  model_lines <- c(model_lines, "}")
  
  model_string <- paste(model_lines, collapse = "\n")
  
  if (print_model_function){
    cat(model_string)
  }
  new_func <- eval(parse(text = model_string)) # generates the function
  
  # Assign the new function to the global environment
  assign(model_function_name, new_func, envir = .GlobalEnv)
  cat(paste0("\n\n\033[94mNote:Model function ", model_function_name, 
             " is generated. It can be run by calling it directly:\n", model_function_name, 
             "(params,return_payoffs=FALSE,return_trace=FALSE,return_transition_prob=FALSE,return_detailed_outcomes=FALSE)\033[0m\n"))
  return(TRUE)
}


construct_P_str <- function(model_struc, is_cycle_dep, model_lines = ""){
  
  
  markov_eqns <- model_struc$markov_eqns
  is_cycle_dep <- model_struc$is_cycle_dep
  decisions <- model_struc$decisions
  n_cycles <- model_struc$n_cycles
  #cycles <- 1:n_cycles
  #cycle_names <- paste0("cycle_", cycles)
  # states <- model_num_struc$states
  # n_states <- model_num_struc$n_states
  #states_expanded <- model_struc$states_expanded
  n_states_expanded <- model_struc$n_states_expanded
  model_lines <- c(model_lines, "P <- list()")
  for (decision in decisions){
    if (is_cycle_dep){
      model_lines <- c(model_lines, paste0("P[['", decision, "']] <- array(0, dim = c(", n_states_expanded, ",", n_states_expanded, ",", n_cycles, 
                                           "), dimnames = list(states_expanded,states_expanded, cycles))"))
    } else {
      model_lines <- c(model_lines, paste0("P[['", decision, "']] <- matrix(0, nrow = ", n_states_expanded, ", ncol = ", n_states_expanded, 
                                           ", dimnames = list(states_expanded, states_expanded))"))
    }
  }
  # iterate through the equations and populate P
  for (i in 1:nrow(markov_eqns)){
    state <- markov_eqns$state_expanded[i]
    dest <- markov_eqns$dest_expanded[i]
    decision <- markov_eqns$decision[i]
    prob <- markov_eqns$probs[i]
    if (is_cycle_dep){
      cycle <- markov_eqns$cycle[i]
      model_lines <- c(model_lines, paste0("P[['", decision, "']]['",state, "','", dest,"','", cycle,"'] <- ", prob))
    } else {
      model_lines <- c(model_lines, paste0("P[['", decision, "']]['",state, "','", dest,"'] <- ", prob))
    }
  }
  return(model_lines)
}


construct_Payoff_str <- function(model_struc, model_lines = ""){
  markov_eqns <- model_struc$markov_eqns
  is_cycle_dep <- model_struc$is_cycle_dep
  decisions <- model_struc$decisions
  n_cycles <- model_struc$n_cycles
  cycles <- 1:n_cycles
  #cycle_names <- paste0("cycle_", cycles)
  # states <- model_num_struc$states
  # n_states <- model_num_struc$n_states
  #states_expanded <- model_struc$states_expanded
  n_states_expanded <- model_struc$n_states_expanded
  payoff_names <- model_struc$payoff_names
  model_lines <- c(model_lines, "Payoff <- list()")
  for (payoff_name in payoff_names){
    for (decision in decisions){
      
    }
  }
  payoff_eqns <- model_struc$payoff_eqns
  for (i in 1:nrow(payoff_eqns)){
    state <- payoff_eqns$state_expanded[i]
    decision <- payoff_eqns$decision[i]
    for (payoff_name in payoff_names){
      if (is_cycle_dep){
        cycle <- payoff_eqns$cycle[i]
        model_lines <- c(model_lines, paste0("Payoff[['", payoff_name, "']][['", decision, "']][", cycle, ",'", state, "'] <- ", payoff_eqns[[payoff_name]][i]))
      } else {
        model_lines <- c(model_lines, paste0("Payoff[['", payoff_name, "']][['", decision, "']][1, '", state, "'] <- ", payoff_eqns[[payoff_name]][i]))
      }
    } # end payoff_name
  }
  return(model_lines)
}




#' Title
#'
#' @param model_struc 
#' @param model_function_name 
#' @param print_model_function 
#'
#' @return
#' @export
#'
#' @examples
gmod_gen_model_function.gmod_decision <- function(model_struc, model_function_name = "my_decision_model", print_model_function = FALSE){
  # build model as a vector of strings 
  model_lines <- paste0(model_function_name, "<- function(params=NULL){")
  model_lines <- c(model_lines, "if (!is.null(params)) list2env(params, envir = .GlobalEnv)")
  # for Decison structure, parse P and Payoffs 
  summary_formulae <- model_struc$summary_formulae
  payoff_names <- model_struc$payoff_names
  decisions <- summary_formulae$decision
  decisions_str <- paste0(decisions, collapse="','")
  payoffs_str <- paste0(payoff_names, collapse="','")
  n_decisions <- length(decisions)
  n_payoffs <- length(payoff_names)
  n <- nrow(summary_formulae)
  #model_lines <- c(model_lines, paste0("summary_results <- data.frame(decision=c('", paste0(summary_formulae$decision, collapse = "','"), "'))"))
  model_lines <- c(model_lines, paste0("summary_results <- matrix(0,nrow=",n_decisions,",ncol=", n_payoffs, 
                                       ",dimnames=list(c('",decisions_str,"'),c('",payoffs_str,"')))"))
  for (payoff_name in payoff_names){
    for (i in 1:n){
      decision <- summary_formulae$decision[i]
      model_lines <- c(model_lines, paste0("summary_results['",decision,"','",payoff_name,"'] <- ", summary_formulae[[payoff_name]][i]))
    }
  }
  model_lines <- c(model_lines, "return(summary_results)")
  model_lines <- c(model_lines, "}")
  
  model_string <- paste(model_lines, collapse = "\n")
  
  if (print_model_function){
    cat(model_string)
  }
  new_func <- eval(parse(text = model_string)) # generates the function
  # Assign the new function to the global environment
  assign(model_function_name, new_func, envir = .GlobalEnv)
  cat(paste0("\n\n\033[94mNote:Model function ", model_function_name, " is generated. It can be run by calling it directly:\n", model_function_name, "(params)\033[0m\n"))
  return(TRUE)
}

create_variables <- function(params, envir) {
  for (name in names(params)) {
    assign(name, params[[name]], envir = envir)
  }
}
