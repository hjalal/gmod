# generate probs and payoffs excel file and datasets
#' Title
#'
#' @param gmod_obj 
#' @param fun_names 
#' @param excel_file_name 
#'
#' @return 
#' @export
#'
#' @examples 
#' 
gmod_expand_functions <- function(gmod_obj, fun_names = NULL, excel_file_name=NULL){
  # for each function get the function arguments
  if (is.null(fun_names)){
    fun_names <- fun_in_gmod(mygmod)
  }
  #fun_name <- fun_names
  if (!is.null(excel_file_name)){
    wb <- openxlsx::createWorkbook()
  }
  for (fun_name in fun_names){
    arg_list <- get_function_arguments(fun_name)
    # get the values for these arguments
    arg_value_list <- list()
    for (arg_name in arg_list){ #arg_name <- arg_list[1]
      arg_value_list[[arg_name]] <- fun_get_arg_values(gmod_obj, arg_name)
      
    }
    # create the combinatorials of the arguments
    values_df <- expand.grid(arg_value_list, stringsAsFactors = FALSE)
    # evaluate the function based on these arguments
    n <- nrow(values_df)
    x <- rep(NA,n)
    for (i in 1:n){
      y <- values_df[i,]
      if(length(y)==1){
        y <- list(y)
      }
      x[i] <- do.call(fun_name, y)
    }
    values_df[fun_name] <- x
    variable_name <- paste0("df_", fun_name)
    cat("Note: The dataset ", variable_name, " created for function ", fun_name, ".\n", sep = "")
    assign(variable_name, values_df, envir = .GlobalEnv)
    # add sheets to workbook
    if (!is.null(excel_file_name)){
      openxlsx::addWorksheet(wb, fun_name)
      openxlsx::writeData(wb, sheet = fun_name, values_df, startCol = 1, startRow = 1)
    }
  }
  if (!is.null(excel_file_name)){
    openxlsx::saveWorkbook(wb, excel_file_name, overwrite = TRUE)
  }
  
}

fun_get_arg_values <- function(gmod_obj, arg_name){
  if (arg_name %in% c("decision", "state", "cycle", "cycle_in_state")){
    arg_name <- paste0(arg_name, "s")
    if (arg_name %in% c("decisions", "states")){
      # Use lapply to filter the list based on the condition
      index <- which(sapply(gmod_obj$layers, function(x) arg_name %in% x$type))
      # Remove NULL elements from the list
      lyr <- gmod_obj$layers[[index]]
      arg_values <- lyr[[arg_name]]
    } else if (arg_name %in% c("cycles","cycle_in_states")){
      arg_values <- 1:n_cycles
    }

  } else { #event name
    # Use lapply to filter the list based on the condition
    index <- which(sapply(gmod_obj$layers, function(x) arg_name %in% x$event))
    lyr <- gmod_obj$layers[[index]]
    arg_values <- lyr$values
  }
  return(arg_values)

}

get_function_arguments <- function(func_name) {
  # Get the function object
  func <- get(func_name)
  
  # Get the formal arguments of the function
  arguments <- names(formals(func))
  
  return(arguments)
}



fun_in_gmod <- function(gmod_obj){
  # Get all objects in the global environment
  all_objects <- ls(envir = .GlobalEnv)
  
  # Filter out functions
  function_names <- all_objects[sapply(all_objects, function(x) is.function(get(x)))]
  
  gmod_text <- paste(nested_list_to_string(gmod_obj$layers), collapse=" ")
  # iterate through the functions and only returns that occur
  keep_fun <- c()
  for(fun_name in function_names){
    if (grepl(paste0("\\b", fun_name, "\\b"), gmod_text)){
      keep_fun <- c(keep_fun, fun_name)
    }
  }
  return(keep_fun)
}


# Function to convert a nested list to a single long string
nested_list_to_string <- function(lst) {
  result <- ""
  for (item in lst) {
    if (is.list(item)) {
      result <- paste0(result, nested_list_to_string(item))
    } else if (is.character(item)) {
      result <- paste0(result, item, " ")
    } else if (inherits(item, "language")) {
      result <- paste0(result, deparse(item), " ")
    } else if (inherits(item, "call")) {
      #print("call")
      result <- paste0(result, as.character(item), " ")
    }
  }
  return(result)
}