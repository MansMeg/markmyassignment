#' @name set_test_suite
#' @rdname set_test_suite
#'
#' @title Set test suite to use to \code{markmyassignment}.
#'
NULL

#' @rdname set_test_suite
#' @details
#'   Set test suite file for github repositories.
#' @param repo Repository address in the format
#'   \code{username/repo[/subdir][@@ref]}. Alternatively, you can
#'   specify \code{subdir} and/or \code{ref} using the respective parameters
#'   (see below); if both is specified, the values in \code{repo} take
#'   precedence.
#' @param ref Desired git reference. Could be a commit, tag, or branch
#'   name. Defaults to \code{"master"}.
#' @param subdir subdirectory within repo that contains the test file.
#' @param auth_token To install from a private repo, generate a personal
#'   access token (PAT) in \url{https://github.com/settings/applications} and
#'   supply to this argument. This is safer than using a password because
#'   you can easily delete a PAT without affecting any others. 
#' @param host Github API host to use. Override with your own github hostname.
#' @example
#' set_github_test_suite(repo = "MansMeg/KursRprgm/Labs/Test/bmiTest.R")
#' set_github_test_suite(repo = "hadley/httr/R/body.R@@v0.4")
#' @export
set_github_test_suite <- 
  function(repo, auth_token = NULL,
           host = "api.github.com"){
        
    test_file_url <- github_test_file_url(repo, auth_token = auth_token, host = host)
    check_test_file_url(test_file_url)
    store_test_file_url(test_file_url)
  }

#'
github_test_file <- 
  function(repo, auth_token = auth_token, host = host){
    prepo <- parse_git_repo(repo)
    url <- paste0("https://", host,"/repos/", 
                  prepo$username, "/", prepo$repo, 
                  "/contents/", prepo$subdir, 
                  if(!is.null(prepo$ref)) paste0("?ref=", parsed_repo$ref)
    )
    test_file_url <- list(url=url, auth_token=auth_token)
    class(test_file_url) <- c("github_test_file_url", "list")
    test_file_url
  }


#' Parse github repo of format: username/repo[/subdir][@ref]
parse_git_repo <- function(path) {
  username_rx <- "(?:([^/]+)/)?"
  repo_rx <- "([^/@#]+)"
  subdir_rx <- "(?:/([^@#]*[^@#/]))?"
  ref_rx <- "(?:@([^*].*))"
  github_rx <- sprintf("^(?:%s%s%s|(.*))$",
                       username_rx, repo_rx, subdir_rx, ref_rx)
  
  param_names <- c("username", "repo", "subdir", "ref", "invalid")
  replace <- setNames(sprintf("\\%d", seq_along(param_names)), param_names)
  params <- lapply(replace, function(r) gsub(github_rx, r, path, perl = TRUE))
  if (params$invalid != "")
    stop(sprintf("Invalid git repo: %s", path))
  params <- params[sapply(params, nchar) > 0]    
  params
}


check_test_file_url.github_test_file_url <- function(x)
  
check_test_file_url.github_test_file_url <- function(x)  


#' @rdname set_test_suite
#' @details
#'   Set test suite file for http/https files.
#' @param
#'   path url path to test file.
#' 
#' @example
#'   set_http_test_suite(path = "https://github.com/MansMeg/KursRprgm/raw/master/Labs/Test/bmiTest.R")
#' @export
set_http_test_suite <- function(path) {
  test_file_url <- http_test_file_url(path)
  check_test_file_url(test_file_url)
  store_test_file_url(test_file_url)
}



# Generics check_test_file
check_test_file_url <- function(x) UseMethod("check_test_file_url")

store_test_file_url <- function(x) UseMethod("store_test_file_url")

get_test_file_url <- function(x) UseMethod("get_test_file_url")

download_test_file <- function(x, ...) UseMethod("download_test_file")

get_test_suite <- function()



  if (!quiet) {
    message("Downloading test suite from github repo ", x$username, "/", x$repo, "@", x$ref)
  }

# dest <- tempfile(fileext = paste0(".zip"))
# src <- paste0("https://", x$host, "/repos/", x$username, "/", x$repo,
#               "/zipball/", x$ref)
# 
# if (!is.null(x$auth_token)) {
#   auth <- httr::authenticate(
#     user = x$auth_token,
#     password = "x-oauth-basic",
#     type = "basic"
#   )
# } else {
#   auth <- NULL
# }
  
  
#request <- httr::GET(url, ...)
#httr::stop_for_status(request)
#writeBin(httr::content(request, "raw"), path)
#path
