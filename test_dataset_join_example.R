# test advanced markov model
rm(list = ls())
library(gmod)
library(magrittr)

n_cycles <- 5
mygmod <- gmod() + # for illustration it is 75 in the tutorial 
  decisions("StandardOfCare", "StrategyA", "StrategyB", "StrategyAB") + 
  states(names=c("H", "S1", "S2", "D"), 
         init_probs=c(1,0,0,0),
         max_cycle_in_states=c(1,n_cycles,1,1)) + 
  event(name = "die",  
        scenarios = c("T","F"), 
        probs = c("pDie", "#"), 
        outcomes = c("D", "get_event")) +  
  event(name = "get_event",  
        scenarios = c("recover", "getsick", "progress", "stay"), 
        probs = c("pRecover", 
                  "pGetSick", 
                  "pProgress", 
                  "#"), 
        outcomes = c("H", "S1", "S2", "curr_state")) +  
  payoffs(cost = cost(state, decision, get_event, die), 
          utility = utility(state, decision, get_event),
          discount_rates=c(0,0))

n_age_init <- 25 - 1 # age at baseline gmod starts at cycle 1 instead of 0 in the tutorial
n_age_max  <- 100 # maximum age of follow up



## Age-dependent mortality rates ----
lt_usa_2015 <- read.csv("~/github/gmod/inst/extdata/LifeTable_USA_Mx_2015.csv")
#* Extract age-specific all-cause mortality for ages in model time horizon
v_r_mort_by_age <- lt_usa_2015 %>% 
  dplyr::filter(Age >= n_age_init & Age < n_age_max) %>%
  dplyr::select(Total) %>%
  as.matrix() # anyone above 100 have the same mortality

n_age_init <- 25 - 1 # age at baseline gmod starts at cycle 1 instead of 0 in the tutorial
n_age_max  <- 100 # maximum age of follow up


#
### Age-dependent mortality rates ----
#lt_usa_2015 <- read.csv("../data/LifeTable_USA_Mx_2015.csv")
#* Extract age-specific all-cause mortality for ages in model time horizon
v_r_mort_by_age0 <- lt_usa_2015 %>% 
  dplyr::filter(Age >= n_age_init & Age < n_age_max) %>%
  dplyr::select(Total) %>%
  as.matrix() # anyone above 100 have the same mortality

params <- list(
  ### Transition rates (annual), and hazard ratios (HRs) ----
  r_HS1  = 0.15,  # constant annual rate of becoming Sick when Healthy
  r_S1H  = 0.5 ,  # constant annual rate of becoming Healthy when Sick
  hr_S1  = 3   ,  # hazard ratio of death in Sick vs Healthy 
  hr_S2  = 10  ,  # hazard ratio of death in Sicker vs Healthy 
  
  ### Effectiveness of treatment B ----
  hr_S1S2_trtB = 0.6,  # hazard ratio of becoming Sicker when Sick under treatment B
  
  #* Weibull parameters for state-residence-dependent transition probability of 
  #* becoming Sicker when Sick conditional on surviving
  r_S1S2_scale = 0.08, # scale
  r_S1S2_shape = 1.1 , # shape
  
  v_r_mort_by_age = v_r_mort_by_age0, 
  
  ### State rewards ----
  #### Costs ----
  c_H    = 2000 , # annual cost of being Healthy
  c_S1   = 4000 , # annual cost of being Sick
  c_S2   = 15000, # annual cost of being Sicker
  c_D    = 0    , # annual cost of being dead
  c_trtA = 12000, # annual cost of receiving treatment A
  c_trtB = 13000, # annual cost of receiving treatment B
  #### Utilities ----
  u_H    = 1   ,  # annual utility of being Healthy
  u_S1   = 0.75,  # annual utility of being Sick
  u_S2   = 0.5 ,  # annual utility of being Sicker
  u_D    = 0   ,  # annual utility of being dead
  u_trtA = 0.95,  # annual utility when receiving treatment A
  
  ### Transition rewards ----
  du_HS1 = 0.01,  # disutility when transitioning from Healthy to Sick
  ic_HS1 = 1000,  # increase in cost when transitioning from Healthy to Sick
  ic_D   = 2000  # increase in cost when dying
)

pRecover <- function(state){
  rRecover <- if (state=="S1") r_S1H else 0
  rate2prob(rRecover)
}

pGetSick <- function(state){
  rGetSick <- if (state=="H") r_HS1 else 0
  rate2prob(rGetSick)
}

pProgress <- function(state, decision, cycle_in_state){
  
  if (state=="S1"){
    hr_S1S2 <- if (decision %in% c("StrategyB", "StrategyAB")) hr_S1S2_trtB else 1
    
    r_S1S2_tunnels <- (cycle_in_state*r_S1S2_scale)^r_S1S2_shape - 
      ((cycle_in_state - 1)*r_S1S2_scale)^r_S1S2_shape
    
    #p_S1S2_tunnels <- rate2prob(r_S1S2_tunnels)
    #rProgress <- (state=="S1") * r_S1S2_tunnels * hr_S1S2
    rProgress <- r_S1S2_tunnels * hr_S1S2
    
    rate2prob(rProgress)
  } else {
    0
  }
  
}


pDie <- function(state, cycle){
  r_HD <- v_r_mort_by_age[cycle]
  rDie <- switch(state, 
                 "H" = r_HD,
                 "S1" = r_HD*hr_S1,
                 "S2" = r_HD*hr_S2,
                 "D" = 0) #translates to probability of 1
  rate2prob(rDie)
}

cost <- function(state, decision, get_event, die){
  # cost of decision is only applied if the state is either S1 or S2
  trans_cost_getting_sick <- (get_event=="getsick")*ic_HS1 # increase in cost when transitioning from Healthy to Sick
  trans_cost_dying <- (die==TRUE)*ic_D # increase in cost when dying
  
  if (state %in% c("S1","S2")){
    c_decision <- switch(decision,
                         "StandardOfCare" = 0,
                         "StrategyA" = c_trtA ,
                         "StrategyB" = c_trtB,
                         "StrategyAB" = c_trtA + c_trtB)}
  else {
    c_decision <- 0
  }
  # cost of the state is a function of the state
  c_state <- switch(state, 
                    "H" = c_H,
                    "S1" = c_S1,
                    "S2" = c_S2,
                    "D" = c_D)
  # combine both
  return(c_decision + c_state + trans_cost_getting_sick + trans_cost_dying)
}

utility <- function(state, decision, get_event){
  trans_util_getting_sick <- -du_HS1*(get_event=="getsick")
  u_state <- switch(state, 
                    "H" = u_H,
                    "S1" = if (decision %in% c("StrategyA", "StrategyAB")) u_trtA else u_S1,
                    "S2" = u_S2,
                    "D" = u_D)
  return(u_state + trans_util_getting_sick)
}

#model_struc <- gmod_gen_model_function(mygmod)
#model_struc

#source("../R/myfun.R")

#model_results <- my_markov_model(model_struc, params, return_trace = T)
#dim(model_results$Trace)
#model_results


