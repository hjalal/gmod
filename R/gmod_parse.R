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
  payoff_names <- model_num_str$payoff_names
  model_num_str$markov_eqns <- model_num_str$markov_eqns %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(probs = eval(parse(text = probs)))
  
  model_num_str$payoff_eqns <- model_num_str$payoff_eqns %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(dplyr::across(payoff_names, ~ eval(parse(text = .x))))
  
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
  # if(is.null(params)){
  #   warning("No parameters were provided. Will use the parameters from the global environment. 
  #           If instead you want to evaluate the model with specific parameter values, please provide 
  #           them here as list.")
  # } else {
  #   list2env(params)
  # }
  # # for Decison structure, parse P and Payoffs 
  # 
  # summary_formulae <- model_struc$summary_formulae
  # payoff_names <- model_struc$payoff_names
  # n <- nrow(summary_formulae)
  # summary_results <- summary_formulae
  # for (payoff_name in payoff_names){
  #   for (i in 1:n){
  #     summary_results[[payoff_name]][i] <- eval(parse(text = summary_formulae[[payoff_name]][i]))
  #   }
  # }
  # 
  # class(model_num_str) <- "gmod_decision"
  # return(model_num_str)
  # # 
}

