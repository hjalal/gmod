construct_prob_vec <- function(x, v_prob) {
  # Check if prob_left() function is used in v_prob
  v_prob <- check_prob_vector(v_prob) 
  # Check if the length of the vectors matches
  if (length(x) != length(v_prob)) {
    print(paste(x, v_prob))
    stop("Lengths of 'x' and 'v_prob' vectors should match.")
  }
  
  # Create a named vector of probabilities
  prob_vector <- setNames(v_prob, x)
  return(prob_vector)
}

check_prob_vector <- function(v_prob){
  if (Inf %in% v_prob) {
    if (length(v_prob[is.infinite(v_prob)])>1){
      stop("Only one probability can be Inf as a complementary probability = 1-sum(other probs).")
    }
    # Calculate the complement probability
    complement_index <- which(v_prob == Inf)
    complement_prob <- 1 - sum(v_prob[-complement_index])
    # Replace prob_left() in the vector with its calculated value
    v_prob[complement_index] <- complement_prob
  }
  
  # Check if the length of the vectors matches
  if (sum(v_prob) != 1) {
    print(paste(v_prob))
    stop("Probabilities must add to 1. Inf can be used as complement for one of the probabilities.")
  }
  if (any(v_prob > 1) | any(v_prob < 0) ){
    print(paste(v_prob))
    stop("Probabilities must be between 0 and 1. Inf can be used as complement for one of the probabilities.")
  }
  return(v_prob)
}

