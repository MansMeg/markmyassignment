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
#' set_assignment(path = "https://raw.githubusercontent.com/MansMeg/KursRprgm/master/Labs/Test/d1.yml")
#' 
#' @export
set_assignment <- function(path, auth_token = NULL){
  path <- path_type(path)
  if(inherits(path,what = "path_error")) stop("Path/url does not work.")
  temp_folder_check_create()
  temp_file <- tempfile()
  on.exit(unlink(temp_file))
  dest <- paste0(tempdir(), "/markmyassignment/assignment1.yml")
  get_file(path, temp_file)  
  if(!assignment_yml_ok(path = temp_file)) {
    stop("Not a correct assignment file.")
  }
  if(file.exists(dest)) file.remove(dest)
  file.rename(from = temp_file, to = dest)
  assignment <- read_assignment_yml()
  message("Assignment set:\n", assignment$name, " : ", assignment$description)
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
#'   Check the yml file to be a correct assignment yml.
#' 
#' @details
#'   Check the yml file that it is correct and can be used. Otherwise warn.
#' 
#' @return 
#'   boolean 
#' 
assignment_yml_ok <- function(path = NULL){
  assignment <- try(read_assignment_yml(path), silent = TRUE)
  if(inherits(assignment, "try-error")) return(FALSE)
  
  # Check the structure here!
  
  TRUE
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
#' @examples
#'   get_file(path, dest)
#' 
get_file <- function(path, dest, ...){
  stopifnot(!inherits(path, "path_error"))
  UseMethod("get_file")
}

get_file.path_local <- function(path, dest, ...){
  file.copy(from = path, to = dest, overwrite = TRUE)
}

get_file.path_http <- function(path, dest, ...){
  request <- httr::GET(path, ...)
  httr::stop_for_status(request)
  writeBin(httr::content(request, "raw"), dest)
}

#' @title
#' Load assignment information
#' 
#' @details
#' Check if there exist an assignmentfile and then load it.
#' 
#' @return assignment object
#' 
#' @examples
#'  read_assignment_yml()
read_assignment_yml <- function(path = NULL){
  if(is.null(path)){
    assignment_file <- paste0(tempdir(),"/markmyassignment/assignment1.yml")
  } else {
    assignment_file <- path
  }
  if(file.exists(assignment_file)){
    res <- yaml::yaml.load_file(assignment_file)
    if(is.list(res)) return(res) else stop("Not a correct .yml file")
  } else {
    stop("No assignment has been set. Please use set_assignment().", call. = FALSE)
  }
}


#' @title
#'   Get the name of the tasks in the assignment.
#' 
#' @examples
#'  show_tasks()
#' 
#' @export
show_tasks <- function(){
  assignment <- read_assignment_yml()
  names(assignment$task)
}
