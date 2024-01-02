# sandbox 

# to do: 
# - define keywards (stay, all, age, cycle_in_state, cycle, )
# - functions like start_with(), all(), all_states(), all_decisions() ... 
# - checks of non-overlap between decisions/states/events 
# - keywards vs. functions (e.g., all(), starts_with(), ... ) 
# - - if funcitons - delay execution?
# features - sparse, 1/2 cycle correction, 
# model types DES, microsim, decision tree
# rewards, transition vs. state rewards
# compleemtn transition # by default for all states unless otherwise specified
# feature - multi-dimensional states, add state dimensions - where these dimensions can be referenced
# for example, disease can have two dimensions - bp level and dm.
# 
# Define a simple S3 class for a plot

file_list <- list.files("R/", full.names = TRUE)
for (file in file_list){
  source(file)
}



