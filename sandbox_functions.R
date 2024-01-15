# Function to retrieve the value 'X' based on the 'name'
get_value_probs <- function(data, outcomes_id) {
  #return(paste0("(",data$probs[data$outcomes == outcomes_id],")"))
  return(data$id[data$outcomes == outcomes_id])
}

get_prob_chains <- function(data, outcomes_id) {
  names <- data$name[data$outcomes == outcomes_id]
  all_lineages <- list()
  
  for (name in names) {
    all_lineages[[length(all_lineages) + 1]] <- get_lineages_recursive(data, name)
  }
  
  if (length(names) == 0) {
    return("") #list(as.character(outcomes_id)))
  } else {
    individual_lineages <- list()
    if (length(all_lineages)>0){
      for (i in 1:length(all_lineages)) {
        X <- get_value_probs(data, outcomes_id)[i]
        individual_lineages <- c(
          individual_lineages,
          lapply(all_lineages[[i]], function(x) if (x=="") X else c(x, X))
        )
      }
    } else {
      stop("An error occured in the event mappings.  Please refer to the definitions and the vignettes for more help.")
    }

    return(individual_lineages)
  }
}
# get_lineages_recursive <- function(data, outcomes_id) {
#   names <- data$name[data$outcomes == outcomes_id]
#   all_lineages <- list()
#   
#   for (name in names) {
#     all_lineages[[length(all_lineages) + 1]] <- get_lineages_recursive(data, name)
#   }
#   
#   if (length(names) == 0) {
#     return(list(get_value_X(data, outcomes_id)))
#   } else {
#     individual_lineages <- list()
#     for (i in 1:length(all_lineages)) {
#       individual_lineages <- c(
#         individual_lineages,
#         lapply(all_lineages[[i]], function(x) c(get_value_X(data, outcomes_id), x))
#       )
#     }
#     return(individual_lineages)
#   }
# }
