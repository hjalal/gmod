# Helper functions
.onAttach <- function(libname, pkgname) {
  packageStartupMessage('To get started with GMOD, please check the vignettes by typing browseVignettes(package = "gmod").')
}

get_named_pairs <- function(...) {
  args <- list(...)
  print(args)
  if (length(args) != 1 || length(names(args)) != 1) {
    stop("Please provide a single pair argument.")
  }
  
  name <- names(args)
  value <- args[[name]]
  
  # Perform operations using the provided pair
  cat("Name:", name, "\n")
  cat("Value:", value, "\n")
  # Add your logic or operations here based on the provided pair
}

# converts a list to a single string without evaluation
my_deparse <- function(x){
  paste0(deparse(x), collapse = "")
}

# function to replace part of a string with a variable length string
substr2 <- function(text, replacement, start, stop) {
  new_text <- paste0(substr(text, 1, start - 1), replacement, substr(text, stop + 1, nchar(text)))
  return(new_text)
}

# Function to return the name of a function as a string
probs2string <- function(input_string) {
  # https://stackoverflow.com/questions/35347537/using-strsplit-in-r-ignoring-anything-in-parentheses
  #input_string <- deparse(substitute(probs))
  # Extract elements within c() using regex
  #wo_white_spaces <- input_string #
  wo_white_spaces <- gsub("\\s+", "", input_string)
  cleaned_string <- sub("^c\\((.*)\\)$", "\\1", wo_white_spaces)
  #y <- strsplit(cleaned_string, ", |(?>\\(.*?\\).*?\\K(, |$))", perl = TRUE)[[1]]
  # y <- strsplit(cleaned_string, "(?![^(]*\\)),(?!.*\\))", perl = TRUE)[[1]]
  y <- strsplit(cleaned_string, ",(?![^()]*\\))", perl = TRUE)[[1]]

  #extracted_elements <- gsub("^c\\((.*)\\)$", "\\1", input_string)
  # Split the elements by comma (,) and remove leading/trailing spaces
  # remove complement in "inf"
  inf_index <- y == "#"
  if (any(inf_index)){
    sum_others <- paste0(y[!inf_index], collapse = "+")
    y[inf_index] <- paste0("1-(", sum_others, ")")
  }
  return(y)
}


# extract location of c() in a string ignoring inside parenthesis 
extract_c_matches <- function(x){
  n <- nchar(x)
  start <- c()
  end <- c()
  n_open <- 0
  c1_open <- FALSE # ignoring ( before the first c(
  for (i in 1:n){
    y <- substr(x,i,i)
    if (y == "("){
      if (substr(x,i-1,i-1) == "c" ){ #take the position
        start <- c(start, i + 1)
        c1_open <- TRUE
      } 
      if (c1_open){
        n_open <- n_open + 1
      }
    }
    if (y == ")"){
      n_open <- n_open - 1
      if (n_open == 0){
        end <- c(end, i-1)
      }
    }
  }
  return(list(start = start, end = end))
}

# function to convert event payoffs list into a list of strings 
payoff2liststring <- function(input_string){
  # remove all white spaces from string
  text <- gsub("\\s+", "", input_string)
  # get location of c() inside the string
  #matches <- gregexpr("\\b[c]\\([^()]*\\)", text, perl = TRUE)
  matches <- extract_c_matches(text)
  # get each matching string
  
  # for each string, do the following
  for (i in rev(1:length(matches$start))) {
    # put quotes around each element 
    extracted_match <- substr(text, matches$start[i], matches$end[i])
    split_elements <- unlist(strsplit(extracted_match, "(,)(?![^(]*\\))", perl = TRUE))
    replacement <- paste0("\"", paste0(split_elements, collapse = "\",\""), "\"")
    # replace the original text starting from the end
    text <- substr2(text, replacement, matches$start[i], matches$end[i])
  }
  eval(parse(text = text))
}

# gets the state and tunnel out of a tunnel state
#' Title
#'
#' @param tunnel_state the name of the tunnel state
#' @description
#' decomposes an expanded state into the state name and cycle_in_state. If the state is not a tunnel, it will return cycle_in_state=1
#' 
#' @return list consisting of the name of the state and it's cycle_in_state value
#' @export
#'
#' @examples
#' tunnel2state("sick_tnl5")
tunnel2state <- function(tunnel_state){
  state_comp <- strsplit(tunnel_state, "_tnl")[[1]]
  if (length(state_comp) == 1){
    state_comp[2] <- 1 #0
  }
  #names(state_comp) <- c("state", "tunnel")
  return(list(state = state_comp[1], cycle_in_state = as.numeric(state_comp[2])))
}



# function to make sure elements legnths are either 1 or n
check_element_lengths <- function(layer){
  length_of_elements <- length(layer)
  unique_lengths <- unique(length_of_elements)
  n_unique_lengths <- length(unique_lengths)
  if (n_unique_lengths > 2 | (n_unique_lengths == 2 & min(unique_lengths) > 1)){ # print error and stop
    print("Error in ", layer, ". lengths of arguments can either be 1 or n")
  }
  return(TRUE)
}

# Function to expand the original list
expand_list <- function(original_list, output = "df") {
  check_element_lengths(original_list)
  max_length <- max(lengths(original_list))
  new_list <- vector("list", length = max_length)
  
  for (i in 1:max_length) {
    new_element <- lapply(original_list, function(x) if(length(x) == 1) x else x[i])
    new_list[[i]] <- new_element
  }
  
  if (output == "df"){
    return(data.frame(do.call(rbind, new_list)))
  } else {
    return(new_list)
  }
}

replace_prev_event <- function(text, replacements) {
  pattern <- "prev_event\\((\"[^\"]+\")\\)"
  matches <- gregexpr(pattern, text)[[1]]
  match_lengths <- attr(matches, "match.length")
  for (i in rev(seq_along(matches))) {
    #replacement <- replacements[i]
    text <- substr2(text, replacements[i], matches[i], matches[i]+match_lengths[i]-1)
  }
  return(text)
}


parse_object <- function(x){
  d <- dim(x)
  # Convert the array to a vector
  vectorized_x <- c(x)
  n <- length(vectorized_x)
  eval_x <- rep(0, n)
  for (i in 1:n){
    eval_x[i] <- eval(parse(text = vectorized_x[i]))
  }
  # Convert the vector back to an array
  if (is.null(d)){
    y <- as.vector(eval_x)
    names(y) <- names(x)
  } else {
    y <- array(eval_x, dim = d, dimnames = dimnames(x))
  }
  return(y)
}


#' Title
#'
#' @param rate 
#' @description converts rate to probability using prob = 1-exp(-rate)
#' @return probability 
#' @export
#'
#' @examples
#' rate2prob(rate = 0.3)
rate2prob <- function(rate){
  1 - exp(-rate)
}

#' Title
#'
#' @param prob 
#' @description converts prob to rate using rate = -log(1-prob)
#' @return rate 
#' @export
#'
#' @examples
#' prob2rate(prob = 0.5)
prob2rate <- function(prob){
  -log(1-prob)
}