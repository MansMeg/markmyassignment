#' @title
#' Mark assignment file
#' 
#' @description
#' Mark a specific assignment file
#' 
#' @param tasks
#'   Which task should be corrected (if more than one). Default is all. 
#'   To see the different task, see \code{\link{show_tasks}}.
#' @param mark_file
#'   Path to the file to mark.
#' @param assignment_path
#'   Assignment file to set before marking the assignment (url or local path).
#' @param force_get_tests
#'   Argument is deprecated, use \code{set_assignment()} instead.
#' @param quiet
#'   Should test be run without output?
#' @param ... further arguments sent to \code{test_dir()}.
#' 
#' @examples
#' \dontrun{
#' assignment_path <- 
#'   file.path(system.file(package = "markmyassignment"), "extdata", "example_assignment01.yml")
#' file_path <- file.path(system.file(package = "markmyassignment"), "extdata", "example_lab_file.R")
#' mark_my_file(mark_file = file_path, assignment_path = assignment_path)
#' }
#' @export
mark_my_file <- function(tasks = NULL, mark_file=file.choose(), assignment_path = NULL, force_get_tests = FALSE, quiet = FALSE, ...){
  checkmate::assert_character(tasks, null.ok = TRUE)
  checkmate::assert_file_exists(mark_file)
  checkmate::assert_string(assignment_path, null.ok = TRUE)
  checkmate::assert_flag(force_get_tests)
  checkmate::assert_flag(quiet)
  
  if(force_get_tests){
    .Deprecated("set_assignment()", old = "force_get_tests")
  }
  
  if(!is.null(assignment_path)) {
    old_warn_opt <- options(warn = 2)
    set_assgn_result <- try(suppressMessages(set_assignment(assignment_path)), silent = TRUE)
    options(warn = old_warn_opt$warn)
    if(methods::is(set_assgn_result, "try-error"))
      stop(set_assgn_result[1])
  }
  
  if(quiet){
    testthat::capture_output(test_results <- suppressMessages(run_test_suite(caller = "mark_my_file", tasks, mark_file, quiet, ...)))
  } else {
    test_results <- run_test_suite(caller = "mark_my_file", tasks, mark_file, quiet, ...)
  }

  test_results_df <- as.data.frame(test_results)
  if(!any(test_results_df$error) & sum(test_results_df$failed) == 0 & is.null(tasks) & !quiet) cheer()

  if(!is.null(assignment_path)) remove_assignment()
  
  return(invisible(test_results))
}

