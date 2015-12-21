#' @title
#' Mark assignment file
#' 
#' @details
#' Mark a specific assignment file
#' 
#' @param tasks
#'   Which task should be corrected (if more than one). Default is all. 
#'   To see the different task, see \code{\link{show_tasks}}.
#' @param mark_file
#'   Path to the file to mark.
#' @param lab_file
#'   Assignment file to set before marking the assignment (url or local path).
#' @param force_get_tests
#'   Force download of test files before marking of assignments. Default is FALSE.
#' @param quiet
#'   Should test be run without output?
#' @param reporter to use. Default is the 'summary' or specified in assignment yml file.
#' 
#' @examples
#' \donttest{
#' assignment_path <- 
#'   paste0(system.file(package = "markmyassignment"), "/extdata/example_assignment.yml.R")
#' file_path <- paste0(system.file(package = "markmyassignment"), "/extdata/example_lab_file.R")
#' mark_my_file(mark_file = file_path, lab_file = assignment_path)
#' mark_my_assignment()
#' }
#' 
#' @export
mark_my_file <- function(tasks = NULL, mark_file=file.choose(), lab_file, force_get_tests = FALSE, quiet = FALSE, reporter){
  
  assert_function_arguments_in_API(
    tasks = tasks, mark_file = mark_file, lab_file = lab_file,
    force_get_tests = force_get_tests, quiet = quiet, reporter = reporter)
  
  if(length(ls(.GlobalEnv)) > 0) stop("Clean global environment before running tests on file.", call. = FALSE)

  if(!missing(lab_file)) suppressMessages(set_assignment(lab_file))
  
  get_tests(tasks = tasks, force_get_tests = force_get_tests)
  if(missing(reporter)) reporter <- get_mark_my_reporter()
  test_results <- run_test_suite(tasks, mark_file, quiet, reporter = reporter)
  if(!any(test_results$error) & sum(test_results$failed) == 0 & is.null(tasks) & !quiet) cheer()
  return(invisible(test_results))
}

