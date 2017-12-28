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
  is_github <- is_raw | is_http | is_api 
  if(is_github) names(is_github) <- c("raw", "http", "api")[c(is_raw, is_http, is_api)]
  is_github
}
