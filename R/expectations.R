#' @title
#' Expect that the tested function is self-contained
#' 
#' @description
#'   Tests if a fuction is self-contained (i.e. do not use any global variables).
#' 
#' @param object 
#'   Function to test if it is self-contained.
#' @param label
#'   For full form, label of expected object used in error messages. 
#'   Useful to override default (deparsed expected expression) when doing 
#'   tests in a loop. For short cut form, object label. When NULL, computed from 
#'   deparsed object.
#' @param info 
#'   Extra information to be included in the message (useful when writing tests in loops).
#' 
#' @export

expect_self_contained <- function(object, info = NULL, label = NULL) {
  if (is.null(label)) {
    label <- find_expr("object")
  }
  expect_that(object, is_self_contained() , info = info, label = label)
}

#' @title
#' Function is self contained test
#' 
#' @description
#' Tests if a function is self contained (no global variables)
#' 
#' @param expected
#'   Function to test if it is self contained.
#' 
#' @export
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
#' Expect that a given package is used
#' 
#' @description
#'   Tests that the following packages is used.
#' 
#' @param object
#'   Package to check for.
#' @param label
#'   For full form, label of expected object used in error messages. 
#'   Useful to override default (deparsed expected expression) when doing 
#'   tests in a loop. For short cut form, object label. When NULL, computed from 
#'   deparsed object.
#' @param info 
#'   Extra information to be included in the message (useful when writing tests in loops).
#' 
#' @export
expect_package <- function(object, info = NULL, label = NULL){
  if (is.null(label)) {
    label <- find_expr("object")
  }
  expect_that(object, use_package() , info = info, label = label)
}

#' @title
#' Package is used test
#' 
#' @description
#'    test if a packages is loaded.
#' 
#' @export
use_package <- 
  function(){
    function(pkg) {
      expectation(any(grepl(pkg, search())), 
                  paste0("package '", pkg,"' is not used"), 
                  paste0("package '", pkg,"' is used"))
    }
  }


#' @title
#' Expect function arguments
#' 
#' @description
#'  Test that an object with a given name exist in the environment.
#' 
#' @param object
#'   Function to check the arguments of.
#' @param expected
#'   Expected arguments in function.
#' @param label
#'   For full form, label of expected object used in error messages. 
#'   Useful to override default (deparsed expected expression) when doing 
#'   tests in a loop. For short cut form, object label. When NULL, computed from 
#'   deparsed object.
#' @param info 
#'   Extra information to be included in the message (useful when writing tests in loops).
#' @param expected.label Equivalent of \code{label} for shortcut form.
#' 
#' @export
expect_function_arguments <- 
  function(object, expected, info = NULL, label = NULL, expected.label = NULL) 
{
  if (is.null(label)) {
    label <- find_expr("object")
  }
  expect_that(object, 
              has_function_arguments(expected, label = expected.label), 
              info = info, label = label)
}

#' @title
#' Function has argument test
#' 
#' @description
#'  Test if a function has the given arguments
#' 
#' @param expected
#'   Arguments as text vector to test for.
#' @param label
#'   Expectation label used by \code{expect_function_arguments()}
#' 
#' @export
has_function_arguments <- 
  function (expected, label = NULL) 
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
#' Expect function contain code
#' 
#' @description
#'  Test that a given code code exists in function
#' 
#' @param object
#'   Function to check the body
#' @param expected
#'   Expected arguments in function.
#' @param label
#'   For full form, label of expected object used in error messages. 
#'   Useful to override default (deparsed expected expression) when doing 
#'   tests in a loop. For short cut form, object label. When NULL, computed from 
#'   deparsed object.
#' @param info 
#'   Extra information to be included in the message (useful when writing tests in loops).
#' @param expected.label Equivalent of \code{label} for shortcut form.
#' 
#' @export
expect_function_code <- 
  function(object, expected, info = NULL, label = NULL, expected.label = NULL) 
  {
    if (is.null(label)) {
      label <- find_expr("object")
    }
    expect_that(object, 
                function_code(expected, label = expected.label), 
                info = info, label = label)
  }

#' @title
#' Function contain code test
#' 
#' @description
#'    Test if function code contains a given text string.
#' @param expected
#'   Pattern to test for in function code.
#' @param label
#'   Expectation label used by \code{expect_function_code()}
#'   
#' @export
function_code <- 
  function (expected, label = NULL) 
  {
    function(actual) {
      self <- list()
      self$body <- as.character(body(actual))
      expectation(any(grepl(x = self$body, pattern = expected)),
                  failure_msg = paste0("'", expected, "' not found in function body."),
                  success_msg = paste0("'", expected, "' in function body."))
    }
  }


#' @title
#' Expect tidy format (to be constructed)
#' 
#' @description
#' Test that the format used in a function is tidy (see formatR)
#' 
expect_tidy_code <- function(){}



#' @title
#' Internal function (taken from testthat)
#' 
#' @description
#' Internal function (taken from testthat)
#' 
#' @param name See \code{testthat:::find_expr()}.
#' @param env See \code{testthat:::find_expr()}.
#' 
find_expr <- function(name, env = parent.frame()){
  subs <- do.call("substitute", list(as.name(name), env))
  paste0(deparse(subs, width.cutoff = 500), collapse = "\n")
}