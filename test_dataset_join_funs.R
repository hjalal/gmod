# Function to dynamically join events_df with corresponding data.table
join_events_dt <- function(events_dt) {
  result_list <- list()  # To store results of each join
  
  for (i in seq_len(nrow(events_dt))) {
    # Get the corresponding data.table based on the prob column
    dt_name <- dt_fun_list[[events_dt$prob[i]]]
    #df_to_join <- get(dt_name)
    
    # Take the current row of events_df
    current_row <- events_dt[i]
    #if (current_row$probs != "#"){
    # Add an identifier (like row number) to facilitate joining
    # Perform a Cartesian join by adding the current_row to each row of df_to_join
    joined_dt <- df_to_join[, c(current_row, .SD), .SDcols = names(df_to_join)]
    #} else {
    #  joined_dt <- 
    #}
    # Append the result to the list
    result_list[[i]] <- joined_dt
  }
  
  # Combine all the joined results into a single data.table
  result_dt <- rbindlist(result_list, fill = TRUE)
  return(result_dt)
}

# Function to find the common columns except 'p'
find_common_cols <- function(dt_list) {
  # Get common columns other than 'p' for joining
  common_cols <- Reduce(intersect, lapply(dt_list, function(dt) setdiff(names(dt), "p")))
  return(common_cols)
}

# Full join function that excludes 'p' from join
full_join_and_sum <- function(dt_list) {
  # Perform full join on all columns except 'p'
  result_dt <- Reduce(function(x, y) {
    join_cols <- intersect(setdiff(names(x), "p"), setdiff(names(y), "p")) # exclude 'p' from join columns
    merge(x, y, by = join_cols, all = TRUE)
  }, dt_list)
  
  # Sum the 'p' columns after joining
  p_cols <- grep("^p", names(result_dt), value = TRUE) # find all 'p' columns
  result_dt[, p_sum := 1-rowSums(.SD, na.rm = TRUE), .SDcols = p_cols]
  # Remove the 'p' columns
  result_dt[, (p_cols) := NULL]
  setnames(result_dt, "p_sum", "p")
  return(result_dt)
}


return_complement <- function(compl_id){
  event_i <- events_dt[id == compl_id]$event
  p_names <- events_dt[event == event_i & probs != "#"]$probs
  if (length(p_names) == 1){ # just compute complementary
    result_dt <- dt_fun_list[[p_names]] #get(paste0("dt_", p_names))
    result_dt[,p := 1 - p]
  } else { # join, sum and compute complementary
    dt_list <- dt_fun_list[p_names] #mget(paste0("dt_", p_names))
    
    # Call the function with your list of data.tables
    result_dt <- full_join_and_sum(dt_list)
  }
  return(result_dt)
}

# get path_ids ========
full_join_and_mult <- function(dt_list) {
  # Perform full join on all columns except 'p'
  result_dt <- Reduce(function(x, y) {
    join_cols <- intersect(setdiff(names(x), "p"), setdiff(names(y), "p")) # exclude 'p' from join columns
    merge(x, y, by = join_cols, all = TRUE, allow.cartesian=TRUE)
  }, dt_list)
  
  # Sum the 'p' columns after joining
  p_cols <- grep("^p", names(result_dt), value = TRUE) # find all 'p' columns
  result_dt[, p_product := apply(.SD, 1, prod, na.rm = TRUE), .SDcols = p_cols]
  # Remove the 'p' columns
  result_dt[, (p_cols) := NULL]
  result_dt <- result_dt[p_product>0]
  setnames(result_dt, "p_product", "p")
  return(result_dt)
}

