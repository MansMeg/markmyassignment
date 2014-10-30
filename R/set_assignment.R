#' @title
#' Set assignment to mark
#' 
#' @details
#' Sets the assignment to mark. Behind the scenes it download the test suite for the assignment.
#' 
#' @param path
#' Path to the yml file
#' @param auth_token 
#' Authorization token (for github)
#' 
#' @export
set_assignment <- function(){}

# Get the yml file and download the test suite and store in a temp folder.


#' @title
#'   Check yml file to be correct.
#' 
#' @details
#'   Check the yml file that it is correct and can be used. Otherwise warn.
#' 
#' @return 
#'   boolean 
#' 
check_assignment_yml <- function(){}


#' @title
#' Get the path type.
#' 
#' @details
#' Check the path type. 
#' 
#' @return path object
#' 
#' 
path_type <- function(){}




#' @title
#' Get the file from the path
#' 
#' @details
#' Get/download the file from the path.
#' 
#' @param path
#' Path object
#' @param dest
#' Destination for the file
#' 
#' 
get_file <- function(){}



#' @title
#' Load assignment information
#' 
#' @details
#' Check if there is an assignmentfile and load it.
#' 
#' @return assignment object
#' 
load_assignment_info <- function(){}


#' @title
#'   Show the tasks in the assignment.
#' 
#' 
#' @export
show_tasks <- function(){}
