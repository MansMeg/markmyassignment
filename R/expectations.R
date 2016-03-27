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
#' @keywords internal
#' 
#' @export
expect_function_self_contained <- function(object, info = NULL, label = NULL) {
  lab <- testthat:::make_label(object, label)

  global_vars <- codetools::findGlobals(object, merge = F)$variables

  testthat:::expect(
    identical(length(global_vars) == 0, TRUE), # TRUE/FALSE
    sprintf("%s contain global variable(s): %s.", lab, paste(global_vars, collapse = " ")), # Text
    info = info
  )
  invisible(object)
}

#' @keywords internal
#' @export
expect_self_contained <- function(object, info = NULL, label = NULL){
  .Deprecated("expect_function_self_contained")
  expect_function_self_contained(object, info, label)
}


#' @title
#' Expect that a given package is used
#' 
#' @description
#'   Tests that the following packages is used.
#' 
#' @param object
#'   Package to check for.
#' @param info 
#'   Extra information to be included in the message (useful when writing tests in loops).
#' 
#' @keywords internal
#' 
#' @export
expect_attached_package <- function(object, info = NULL){
  testthat:::expect(
    any(grepl(object, search())), # TRUE/FALSE
    sprintf("%s is not used.", object), # Text
    info = info
  )
  invisible(object)
}

#' @export
expect_package <- function(object, info = NULL, label = NULL){
  .Deprecated("expect_attached_package")
  expect_loaded_package(object, info)
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
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
#' 
find_expr <- function(name, env = parent.frame()){
  subs <- do.call("substitute", list(as.name(name), env))
  paste0(deparse(subs, width.cutoff = 500), collapse = "\n")
}