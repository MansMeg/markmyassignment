#' @title
#' Get configuration for \code{markmyassignment}.
#' 
#' @examples
#' get_config()
#' 
get_config <- function(){
  config <- RJSONIO::fromJSON(system.file("config/config.json", package = "markmyassignment"))
  class(config) <- c("markmyassignment_config", "list")
  config
}


#' @title
#' Set source for testsuite
#' 
#' @details
#' Currently the following sources can be used for storing testsuites.
#' - Github API adress to folder
#' - Local directory
#' 
#' Multiple sources can be set
#' 
#' @param test_suite
#' Test source 
#' 
#' @export
#' 
#' @examples
#' set_test_suite(test_suite = "https://api.github.com/repos/MansMeg/KursRprgm/contents/Labs/Test/")
#' 
set_test_suite <- function(test_suite){
  stopifnot(is.character(test_suite), length(test_suite) > 0)
  
  config_list <- get_config()
  config_list$test_suite$path <- test_suite
  config_list$test_suite$type <- get_test_suite_type(test_suite)
  
  check_config(config_list)
  
  writeLines(text = RJSONIO::toJSON(x = config_list, asIs = TRUE), 
             con = system.file("config/config.json", package = "markmyassignment"))
}

#' @title
#' Get type of source for test suite.
#' 
#' @details
#' Computes the type of test suite source.
#' 
#' @param test_suite
#' Path to test suite.
#' 
#' @examples
#' get_test_suite_type("https://api.github.com/repos/MansMeg/KursRprgm/contents/Labs/Test/")
#' 
get_test_suite_type <- function(test_suite){
  test_suite_type <- character(length(test_suite))
  test_suite_type[grepl(x = test_suite, pattern = "^http:")] <- "http"
  test_suite_type[grepl(x = test_suite, pattern = "^https:")] <- "https"
  test_suite_type[grepl(x = test_suite, pattern = "http.*api.*github")] <- "github"
  test_suite_type[!grepl(x = test_suite, pattern = "^http")] <- "local"
  if(any(test_suite_type == "")) warning("Package can't use tests in:\n", 
                                     paste(test_suite[test_suite_type==""], collapse="\n"),
                                     call. = FALSE)
  test_suite_type
}


#' @title
#' Check the config file.
#' 
#' @details
#' Check that the test suites in the config file is possible to use
#' 
#' @param test_suite
#' Path to test suite.
#' 
#' @examples
#' get_test_suite_type("https://api.github.com/repos/MansMeg/KursRprgm/contents/Labs/Test/")
#' 
check_config <- function(config_list=NULL){
  
  if(is.null(config_list)) config_list <- get_config()
    
  test_suite_ok <- !logical(length(config_list$test_suite$path))
  for(i in seq_along(config_list$test_suite$path)){
    if(config_list$test_suite$type[i] == "local"){
      test_suite_ok[i] <- length(dir("dir")) == 0
      next()
    }
    if(config_list$test_suite$type[i] %in% c("github", "http", "https")){
      url_ok <- try(httr::url_success(config_list$test_suite$path[i]), silent = TRUE)
      test_suite_ok[i] <- !(class(url_ok) == "try-error" || !url_ok)
      next()
    }
  }
  if(any(!test_suite_ok)) warning("Error in accessing:\n",  
                              paste(config_list$test_suite$path[!test_suite_ok], collapse = "\n"),
                              call. = FALSE)
}


