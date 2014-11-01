#' @title
#' Expect that the tested function is self-contained.
#' 
#' @details
#' Tests if a fuction is self-contained (i.e. do not use any global variables).
#' 
#' @param object 
#' Function to test if it is self-contained.
#' 
#' @export

expect_self_contained <- function(object, ..., info = NULL, label = NULL) {
  if (is.null(label)) {
    label <- testthat:::find_expr("object")
  }
  expect_that(object, is_self_contained() , info = info, label = label)
}

is_self_contained <- 
  function (expected) 
  {
    function(actual) {
      self <- list()
      self$global_vars <- codetools::findGlobals(actual, merge = F)$variables
      self$self_contained <- length(self$global_vars) == 0
      expectation(self$self_contained, 
                  paste0("contains global variable(s): ", 
                         paste(self$global_vars, collapse = ", ")), 
                  "is self contained.")
    }
  }



#' @title
#' Expect that packages are not used
#' 
#' @details
#' Tests that the following packages are not used.
#' 
#' @param pkg 
#' Package to check for.
#' 
#' @export
expect_package_not_used <- function(object, ..., info = NULL, label = NULL){
  if (is.null(label)) {
    label <- testthat:::find_expr("object")
  }
  expect_that(object, do_not_use_package() , info = info, label = label)
}

do_not_use_package <- 
  function(){
    function(pkg) {
      expectation(!any(grepl(pkg, search())), 
                  paste0("package is used"), 
                  paste0("package is not used"))
    }
  }


#' @title
#' Expect function arguments
#' 
#' @details
#'  Test that an object with a given name exist in the environment.
#' 
#' @param object
#'  Object that is expected to exist.
#' 
#' @export
expect_function_arguments <- function(object, expected, ..., info = NULL, label = NULL, expected.label = NULL) 
{
  if (is.null(label)) {
    label <- testthat:::find_expr("object")
  }
  expect_that(object, 
              has_function_arguments(expected, label = label, ...), 
              info = info, label = label)
}

has_function_arguments <- 
  function (expected, label = NULL, ...) 
  {
    function(actual) {
      self <- list()
      self$formals <- names(formals(actual))
      self$missing <- !self$formals %in% expected
      expectation(all(expected %in% self$formals),
                  failure_msg = paste0(paste(expected[self$missing], collapse = ", "), 
                                       " is missing"),
                  success_msg = "all arguments exist")
    }
  }


#' @title
#' Expect tidy format (to be constructed)
#' 
#' @details
#' Test that the format used in a function is tidy (see formatR)
#' 
#' @param object
#' Function to test.
#' 
expect_tidy_code <- function(){}

