# Function to retrieve the value 'X' based on the 'name'
get_value_with_probs <- function(data, goto_id) {
  #return(paste0("(",data$with_probs[data$goto == goto_id],")"))
  return(data$id[data$goto == goto_id])
}

get_prob_chains <- function(data, goto_id) {
  names <- data$name[data$goto == goto_id]
  all_lineages <- list()
  
  for (name in names) {
    all_lineages[[length(all_lineages) + 1]] <- get_lineages_recursive(data, name)
  }
  
  if (length(names) == 0) {
    return("") #list(as.character(goto_id)))
  } else {
    individual_lineages <- list()
    for (i in 1:length(all_lineages)) {
      X <- get_value_with_probs(data, goto_id)[i]
      individual_lineages <- c(
        individual_lineages,
        lapply(all_lineages[[i]], function(x) if (x=="") X else c(x, X))
      )
    }
    return(individual_lineages)
  }
}
# get_lineages_recursive <- function(data, goto_id) {
#   names <- data$name[data$goto == goto_id]
#   all_lineages <- list()
#   
#   for (name in names) {
#     all_lineages[[length(all_lineages) + 1]] <- get_lineages_recursive(data, name)
#   }
#   
#   if (length(names) == 0) {
#     return(list(get_value_X(data, goto_id)))
#   } else {
#     individual_lineages <- list()
#     for (i in 1:length(all_lineages)) {
#       individual_lineages <- c(
#         individual_lineages,
#         lapply(all_lineages[[i]], function(x) c(get_value_X(data, goto_id), x))
#       )
#     }
#     return(individual_lineages)
#   }
# }