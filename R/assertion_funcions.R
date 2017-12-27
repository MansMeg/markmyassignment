#' @title
#' Check that tasks exist in assignment.
#' 
#' @description
#' Checks that tasks that are inputted in mark_my_assignment exists. If not, a warning is produced.
#' 
#' @param tasks
#' The \code{task} vector from \code{mark_my_assignment}.
#' @param path
#' Path to assignment file. Passed to \code{read_assignment_yml}.
#' @return
#' A warning message or nothing.
#' 
#' @keywords internal
#' 
check_existance_tasks <- function(tasks, path = NULL){
  res <- read_assignment_yml(path = path)
  if(!all(tasks %in% names(res$tasks))){
    warning(paste("The following tasks do not exist:", paste(
      tasks[!(tasks %in% names(res$tasks))], collapse = ", ")))
  }  
}
