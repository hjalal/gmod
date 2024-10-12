

# ==============

library(data.table)
source("test_dataset_join_example.R")
source("test_dataset_join_funs.R")

mygmod
list2env(params, envir = .GlobalEnv)
dt_fun_list <- gmod_expand_functions(mygmod)
#gmod:::fun_in_gmod(mygmod)

events_df <- gmod:::get_event_df(mygmod)
events_dt <- as.data.table(events_df)
events_dt

# merge with the probs 
dt_fun_list$pDie
# Load the data.table library




# Perform the join
# 1. curr_state


# compute complementary ========
# iterate through each noncompl event and join the prob datasets and do 1-sum(p)

# Full join function

# create a list of probs data frames that includes each event scenario id's full expanded probs

#n <- nrow(events_dt)
dt_prob_list <- list() #as.list(rep(NA,n))

# Event/Scenario level =======
# iterate through each id in events_dt and assign htem to the dt_prob_list
for (i in 1:nrow(events_dt)){
  # if probs == hash
  prob <- events_dt[i,probs]
  id <- events_dt[i]$id
  if(prob=="#"){ # compute and return the complement
    dt_prob_list_temp  <- return_complement(id)
    # if it is a complement add origin state if not already there
    # if(events_dt[i]$outcomes == "curr_state"){
    #   dt_prob_list_temp[,dest := state]
    # }
    dt_prob_list[[id]] <- dt_prob_list_temp
  } else { # just get the dataset
    dt_prob_list[[id]] <- dt_fun_list[[prob]]
  }
  
}
print(dt_prob_list) #[[1]])



# path level ======
# compute product of prob by joining first then multiplying.
path_dt <- gmod:::get_prob_chain_markov(mygmod, events_dt)
path_dt
dt_pathprob_list <- list()
# for each path join & multiply ids 
path_ids <- unique(path_dt$path_id)
path_i <- 5
for (path_i in path_ids){
  ids <- path_dt[path_id == path_i]$id
  dt_list <- dt_prob_list[ids]
  if (length(ids)==1){
    dt_pathprob_list[[path_i]] <- dt_list[[1]]
  } else {
    dt_pathprob_list[[path_i]] <- full_join_and_mult(dt_list)
  }
}


dt_pathprob_list
