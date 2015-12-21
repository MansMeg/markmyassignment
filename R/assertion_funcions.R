#' @title
#' Check that tasks exist in assignment.
#' 
#' @details
#' Checks that tasks that are inputted in mark_my_assignment exists. If not, a warning is produced.
#' 
#' @param tasks
#' The \code{task} vector from \code{mark_my_assignment}.
#' @param path
#' Path to assignment file. Passed to \code{read_assignment_yml}.
#' @return
#' A warning message or nothing.
check_existance_tasks <- function(tasks, path = NULL){
  res <- read_assignment_yml(path = path)
  if(!all(tasks %in% names(res$tasks))){
    message("Warning: The following tasks do not exist:")
    message(paste(tasks[!(tasks %in% names(res$tasks))], collapse = ", "))
  }  
}



# mark_my_file 
# (tasks = NULL, mark_file=file.choose(), lab_file, force_get_tests = FALSE, quiet = FALSE, reporter)
# mark_my_assignment
# (tasks = NULL, mark_file = NULL, force_get_tests = FALSE, quiet = FALSE, reporter = NULL)
assert_function_arguments_in_API <- function(
  tasks, mark_file, lab_file, force_get_tests, quiet, reporter){
  
  if(!is.null(tasks) & !is.character(tasks))
    stop("Tasks must be a character vector or NULL.")
  
  if(!is.logical(force_get_tests) | !is.logical(quiet))
    stop("force_get_tests and quiet must be logical.")
  
}

