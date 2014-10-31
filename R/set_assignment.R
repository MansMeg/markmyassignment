#' @title
#' Set assignment to mark
#' 
#' @details
#' Sets the assignment to mark. Behind the scenes it download the test suite for the assignment.
#' 
#' @param path
#' Path to the yml file
#' @param auth_token 
#' Authorization token (for github). Not implemented.
#' 
#' @examples
#' set_assignment(path = "https://raw.githubusercontent.com/MansMeg/KursRprgm/master/Labs/Test/mandatory.R")
#' 
#' @export
set_assignment <- function(path, auth_token = NULL){
  path <- path_type(path)
  if(inherits(path,what = "path_error")) stop("Path does not exist", call. = FALSE)
  temp_folder_check_create()
  dest <- paste0(tempdir(), "/markmyassignment/assignment.yml")
  if(file.exists(dest)) file.remove(dest)
  get_file(path, dest)  
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
check_assignment_yml <- function(){
  assignment <- read_assignment_yml()
}


#' @title
#' Get the path type.
#' 
#' @details
#' Check the path type. 
#' 
#' @return path type as character element c("local", "http", "error")
#' 
#' @examples
#'  path_type(path = "README.md")
#'  path_type(path = "https://raw.githubusercontent.com/MansMeg/KursRprgm/master/Labs/Test/mandatory.R")
#'  path_type(path = "XXX")
path_type <- function(path){
  if(file.exists(path)){
    class(path) <- c("path_local", "character")
  } else {
    try_http <- try(httr::url_ok(path), silent = TRUE)
    if (!is(try_http, "try-error") && try_http){
      class(path) <- c("path_http", "character")
    } else {
      class(path) <- c("path_error", "character")
    }
  }  
  path
}

#' @title
#' Get the file from the path
#' 
#' @details
#' Get/download the file from the path.
#' 
#' @param path
#'  Path object
#' @param dest
#'  Destination for the file
#' 
#' 
get_file <- function(path, dest){
  stopifnot(!inherits(path, "path_error"))
}


#' @title
#' Load assignment information
#' 
#' @details
#' Check if there exist an assignmentfile and then load it.
#' 
#' @return assignment object
#' 
read_assignment_yml <- function(){
  assignment_file <- paste0(tempdir(),"/markmyassignment/assignment.yml")
  if(file.exists(assignment_file)){
    return(yaml::yaml.load_file(assignment_file))
  } else {
    stop("No assignment has been set. Please use set_assignment().", call. = FALSE)
  }
}

#' @title
#'   Show the tasks in the assignment.
#' 
#' 
#' @export
show_tasks <- function(){}
