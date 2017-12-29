#' Check if a path is from github
#'
#' @param path a path to check
#'
#' @return a \code{bool} that is named \code{raw}, \code{http} or \code{api} if a github path.
#' @keywords internal
is_github_path <- function(path){
  checkmate::assert_string(path)
  is_raw <- grepl("//raw\\.githubusercontent\\.com", path)
  is_http <- grepl("//github\\.com", path)
  is_api <- grepl("//api\\.github\\.com", path)
  split_path <- strsplit(path, "/")[[1]]
  is_contents_api_path <- any(grepl("^contents$", split_path)) & is_api
  is_git_api_path <- any(grepl("^git$", split_path)) & is_api
  is_github <- is_raw | is_http | is_api 
  if(is_github) names(is_github) <- c("raw", "http", "api_contents", "api_git")[c(is_raw, is_http, is_contents_api_path, is_git_api_path)]
  is_github
}



#' Get github information from a github path
#'
#' @param path a path to check
#'
#' @return a list 
#' @keywords internal 
get_github_path_info <- function(path){
  checkmate::assert_string(path)
  github_type <- is_github_path(path)
  is_github_url <- unname(github_type)
  checkmate::assert_true(is_github_url)
  path_info <- list()
  class(path_info) <- c("github_path_info", "list")
  split_path <- strsplit(path, "/")[[1]]
  
  if(names(github_type) == "raw"){
    pos_domain <- which(grepl("^raw\\.githubusercontent\\.com$", split_path))
    path_info$owner <- split_path[pos_domain + 1]
    path_info$repo <- split_path[pos_domain + 2]
    path_info$branch <- split_path[pos_domain + 3]
    path_info$path <- paste(split_path[(pos_domain + 4):length(split_path)], collapse = "/")
    return(path_info)
  }
  
  if(names(github_type) == "api_contents"){
    pos_domain <- which(grepl("^api\\.github\\.com$", split_path))
    path_info$owner <- split_path[pos_domain + 2]
    path_info$repo <- split_path[pos_domain + 3]
    path_info$branch <- "master"
    message("Assume master branch for ", path)
    path_info$path <- paste(split_path[(pos_domain + 5):length(split_path)], collapse = "/")
    return(path_info)
  }
  
  if(names(github_type) == "api_git"){
    pos_domain <- which(grepl("^api\\.github\\.com$", split_path))
    path_info$owner <- split_path[pos_domain + 2]
    path_info$repo <- split_path[pos_domain + 3]
    path_info$branch <- "master"
    message("Assume master branch for ", path)
    path_info$path <- as.character(NA)
    return(path_info)
  }
  
  if(names(github_type) == "http"){
    pos_domain <- which(grepl("^github\\.com$", split_path))
    path_info$owner <- split_path[pos_domain + 1]
    path_info$repo <- split_path[pos_domain + 2]
    path_info$branch <- split_path[pos_domain + 4]
    path_info$path <- paste(split_path[(pos_domain + 5):length(split_path)], collapse = "/")
    return(path_info)
  }
  
  stop(path, " is an incorrect github URL.")
  
}


#' Create a github_download_url from a path_github object.
#' 
#' @param path a \code{path_github} object.
#' @keywords internal
create_github_download_url <- function(path){
  checkmate::assert_class(path, "path_github")
  paste0("https://raw.githubusercontent.com/", path$owner, "/", path$repo, "/", path$branch, "/", path$subpath)
}
