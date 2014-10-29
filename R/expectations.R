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
expect_not_use_package <- function()

  
#' @title
#' Expect tidy format
#' 
#' @details
#' Test that the format used in a function is tidy (see formatR)
#' 
#' @param object
#' Function to test.
#' 
#' @export
expect_tidy_format <- function()
  