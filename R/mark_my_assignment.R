#' @title
#' Mark assignment in global environment
#' 
#' @details
#' Mark assignment in global environment.
#' 
#' @param task
#'   Which task should be corrected (if more than one).
#' @param file
#'   Run tests on a R-file. Default is NULL means global environment.
#' @param force_files
#'   Force download of test files before marking of assignments. Default is FALSE.
#' 
#' @export
mark_my_assignment <- function(){}



#' @title
#' Mark assignments in a directory
#' 
#' @details
#' Marks assignments in a directory. Stores the results.
#' 
#' @param dir
#'   Directory with assignments files.
#' @param force_files
#'   Force download of test files before marking of assignments. Default is FALSE.
#' 
#' @export
mark_my_dir <- function(){}



#' @title
#' Get test files
#' 
#' @details
#' Downloads the test files for the current assignment and save them to 
#' temp directory.
#' 
#' @param task
#'  Which task should be downloaded. Default is "all".
#' 
get_test_that_files <- function(task="all"){}



#' @title
#' Check for cached assignments.
#' 
#' @details
#'   Checks which assignments that are cached (ie downloaded to temp dir).
#' 
#' 
#' @return
#'   character vector with cached assignments.
#' 
check_cached_assignments <- function(){}


#' @title
#'   Run test suite.
#' 
#' @details
#'   Runs test on the tasks. Always run mandatory tests.
#' 
#' @param task
#'  Which task should be tested
#' @param file
#'  Run tests on a R-file. Default is NULL means global environment.
#' 
#' @return
#'   Character vector with cached assignments.
#' 
run_test_suite <- function(){}

