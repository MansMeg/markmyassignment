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
  # 1. Capture object and label
  act <- quasi_label(rlang::enquo(object))
  
  # 2. Call expect()
  act$global_vars <- codetools::findGlobals(object, merge = F)$variables
  expect(
    length(global_vars) == 0,
    sprintf("%s contain global variable(s): %s. %s %s", act$lab, paste(act$global_vars, collapse = " "), info, label)
  )
  
  # 3. Invisibly return the value
  invisible(act$val)
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
  checkmate::assert_string(object)
  
  # 1. Capture object and label
  act <- quasi_label(rlang::enquo(object))
  
  # 2. Call expect()
  expect(
    any(grepl(object, search())),
    sprintf("%s is not used. %s", act$lab, info)
  )
  
  # 3. Invisibly return the value
  invisible(act$val)
}

#' @title
#' Expect function arguments
#' 
#' @description
#'  Test that an function object has a function with given arguments.
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
expect_function_arguments <- function(object, expected, info = NULL, label = NULL, expected.label = NULL) {
  checkmate::assert_character(expected)
  
  # 1. Capture object and label
  act <- quasi_label(rlang::enquo(object))
  
  # 2. Call expect()
  act$function_arguments <- names(formals(object))
  act$missing_arguments <- !act$function_arguments %in% expected
  act$extra_arguments <- !expected %in% act$function_arguments
  
  expect(
    !(any(act$missing_arguments) | any(act$extra_arguments)),
    sprintf("%s contain arguments: %s, not %s. %s %s % info", 
            act$lab, 
            paste(function_arguments, collapse = " "), 
            paste(expected, collapse = " "), 
            info,
            label,
            expected.label)
  )
  
  # 3. Invisibly return the value
  invisible(act$val)
}


#' @title
#' Expect function contain code
#' 
#' @description
#'  Test that a given code string exists in function
#' 
#' @param object
#'   Function to check for mandatory code
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
    checkmate::assert_string(expected)
    
    # 1. Capture object and label
    act <- quasi_label(rlang::enquo(object))
    
    # 2. Call expect()
    act$body <- as.character(body(object))

    expect(
      any(grepl(x = body, pattern = expected)),
      sprintf("'%s' not found in the body of %s", 
              expected, 
              act$lab)
    )
    
    # 3. Invisibly return the value
    invisible(act$val)    
    
  }
