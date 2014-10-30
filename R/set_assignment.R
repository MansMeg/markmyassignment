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
#' @examples
#' set_assignment(path = "https://raw.githubusercontent.com/MansMeg/KursRprgm/master/Labs/Test/mandatory.R")
#' 
#' @export
set_assignment <- function(path, auth_token = NULL){
  temp_folder_check_create()
  dest <- paste0(tempdir(), "/markmyassignment/assignment.yml")
  if(file.exists(dest)) file.remove(dest)
  get_file(path, dest, auth_token)  
  if(!check_assignment_yml()) file.remove(dest)
  invisible(dest)
}

#' @title
#'  Check and create folder if missing.
#' 
#' @details
#'   Checks if markmyassignment folder exist in R temp directory.
#'   If not, the folder is created.
temp_folder_check_create <- function() {
  if(!"markmyassignment" %in% dir(tempdir())){
    dir.create(path = paste0(tempdir(), "/", "markmyassignment"))
  }
}

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
