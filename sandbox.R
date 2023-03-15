# sandbox 

# to do: 
# - define keywards (self, all, age, tau/state_residence, cycle, )
# - functions like start_with(), all(), all_states(), all_decisions() ... 
# - checks of non-overlap between decisions/states/events 
# - keywards vs. functions (e.g., all(), starts_with(), ... ) 
# - - if funcitons - delay execution?


# Define a simple S3 class for a plot
gmod <- function(n_cycles = 40, 
                 discount_rate = 0.15,
                 start_age = 0, 
                 half_cycle_correction = FALSE,
                 sparse = FALSE, 
                 #DES, Microsim, 
                 #
                 decision_names = NULL,
                 state_names = NULL,
                 event_names = NULL) {
  gmod_obj <- list(n_cycles = n_cycles, 
                   discount_rate = discount_rate,
                   start_age = start_age, 
                   half_cycle_correction = half_cycle_correction,
                   sparse = sparse, 
                   state_names = state_names,
                   event_names = event_names,
                   state_names = state_names)
  class(gmod_obj) <- "gmod_class"
  gmod_obj
}

# Define a method for the `+` operator for `gmod` objects
`+.gmod_class` <- function(gmod_obj, layer) {
  # Add the layer to the gmod object
  gmod_obj$layers <- append(gmod_obj$layers, list(layer))
  # Return the modified gmod object
  gmod_obj
}

states <- function(state_names){
  list(layer = "state_names", state_names = state_names)
}
events <- function(event_names){
  list(layer = "event_names", event_names = event_names)
}
decisions <- function(decision_names){
  list(layer = "decisions", decision_names = decision_names)
}
initial_prob <- function(initial_prob){
  list(layer = "initial_prob", initial_prob = initial_prob)
}
transition <- function(name, from, to, value){
  # complementary transitoin = "#" - or just use the name "complement"
  # conditions could be a function or a long table that gets converted to a discrete table
  # table columns = conditions, rows = values 
  list(layer = "transition", name = name, from = from, to = to, value = value)
} 
state_reward <- function(reward_type, state_rewards, conditions = NULL, value){
  list(layer = "state_reward", state_rewards = state_rewards, conditions = conditions, value = value)
}
transition_rewards <- function(reward_type, from, to, value){
  list(layer = "transition_reward", reward_type = reward_type, from = from, to = to, value = value)
}
# Create a `myplot` object
mygmod <- gmod(decision_names = c("No Trt", "Trt A", "Trt B"), # these names must be unique and distinct / later they woudl be referenced 
               state_names = c("Moderate", "Severe", "Dead"), # write routine to detect mis-named or miscalculated names
               event_names = c("Progress", "Die")) + 
  #multidimensional state space 
  # microsim - continuosus variables --- 
  # starting 
  #decisions() + # may be keep it to do specialized functions on these types of objects
  #states() + 
  #events() + 
  initial_prob(initial_prob = c("Moderate" = 1)) + 
  transition(name = "Progress", from = c("Moderate", "Moderate2"), to = "Severe", 
             value = "f(from, age, state_t-1, residence_time, decisions, attribute_state)") + 
  transition(name = "complement", from = "all", to = "self", value = "#") + 
  transition(name = "Die", from = "Moderate", to = "Dead", value = 0.5) + 
  transition(name = "Die", from = "Severe", to = "Dead", value = 0.6) + 
  transition(name = "Die", from = "severe", to = "Dead", value = 0.6) + 
  state_reward(reward_type = "cost", state = "all", value = c(Moderate = 1000, Severe = 2000, Dead = 0)) + 
  state_reward(reward_type = "effectiveness", state = "all", value = c(0.8, 0.5, 0)) + 
  transition_rewards(reward_type = "cost", from = "Severe", to = "Dead", value = -0.02) #+
  #complement_transitions(states = "all")
mygmod


mygmod <- mygmod + 
  transition(name = "complement", from = "all", to = "self", value = "#")

# less complete represenatation --- 
# stratification 
# tunnels 
# age-
# tabulate inputs ---. 
# univariate distributions on parameters 
# some of these parameters fixed vs. life 

# non-homogenous cycle length month / year / 

# create a plot --->


  