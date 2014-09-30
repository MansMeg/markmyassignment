#' @title
#' Expect that the tested functiopn is self-contained.
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


