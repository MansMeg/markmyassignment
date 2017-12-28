#' Check if a path is from github
#'
#' @param path a path to check
#'
#' @return a \code{bool} that is named \code{raw}, \code{http} or \code{api} if a github path.
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
