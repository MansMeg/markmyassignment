#' @title
#' Expect that the tested function is self-contained
#' 
#' @description
#'   Tests if a fuction is self-contained (i.e. do not use any global variables).
#' 
#' @param object 
#'   Function to test if it is self-contained.
#' @param label Additional information.
#' @param info Deprecated.
#' 
#' @keywords internal
#' 
#' @export
expect_function_self_contained <- function(object, info = NULL, label = NULL) {
  
  if(!is.null(label)) .Deprecated(msg = "argument label is deprecated with testthat 2.0")
  
  # 1. Capture object and label
  act <- quasi_label(rlang::enquo(object))
  
  # 2. Call expect()
  act$global_vars <- codetools::findGlobals(object, merge = F)$variables
  msg <- sprintf("%s contain global variable(s): %s.", act$lab, paste(act$global_vars, collapse = " "))
  expect(length(act$global_vars) == 0, msg, info)
  
  # 3. Invisibly return the value
  invisible(act$val)
}


#' @title
#' Expect that a given package is used
#' 
#' @description
#'   Tests that the following packages is used.
#' 
#' @param pkg
#'   Package to check for.
#' @param info Deprecated.
#' 
#' @keywords internal
#' 
#' @export
expect_attached_package <- function(pkg, info = NULL){
  checkmate::assert_string(pkg)

  # 2. Call expect()
  msg <- sprintf("Package '%s' is not used (attached).", pkg)
  expect(any(grepl(pkg, search())), msg)
  
  # 3. Invisibly return the value
  invisible(NULL)
}

#' @title
#' Expect that a forbidden package is not used/attached
#' 
#' @description
#'   Tests that the following packages is not used.
#' 
#' @param pkg Package to check for.
#' 
#' @keywords internal
#' 
#' @export
expect_no_attached_forbidden_package <- function(pkg){
  checkmate::assert_string(pkg)

  # 2. Call expect()
  msg <- sprintf("Package '%s' is forbidden.", pkg)
  expect(!any(grepl(pkg, search())), msg)
  
  # 3. Invisibly return the value
  invisible(NULL)
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
#' @param label Additional information.
#' @param info  Deprecated.
#' @param expected.label Deprecated.
#' 
#' @keywords internal
#' 
#' @export
expect_function_arguments <- function(object, expected, info = NULL, label = NULL, expected.label = NULL) {
  checkmate::assert_character(expected, null.ok = TRUE)
  
  if(!is.null(label)) .Deprecated(msg = "argument label is deprecated with testthat 2.0")
  if(!is.null(expected.label)) .Deprecated(msg = "argument expected.label is deprecated with testthat 2.0")
  
  # 1. Capture object and label
  act <- quasi_label(rlang::enquo(object))
  
  # 2. Call expect()
  act$function_arguments <- names(formals(object))
  act$missing_arguments <- !act$function_arguments %in% expected
  act$extra_arguments <- !expected %in% act$function_arguments
  
  msg <- sprintf("%s should contain arguments %s, not %s.", 
                 act$lab, 
                 paste(expected, collapse = " "),                 
                 paste(act$function_arguments, collapse = " "))
  expect(!(any(act$missing_arguments) | any(act$extra_arguments)), msg, info)
    
  
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
#' @param label Additional information.
#' @param info Deprecated.
#' @param expected.label Deprecated.
#' 
#' @keywords internal
#' 
#' @export
expect_function_code <- 
  function(object, expected, info = NULL, label = NULL, expected.label = NULL) 
  {
    checkmate::assert_string(expected)
    if(!is.null(label)) .Deprecated(msg = "argument label is deprecated with testthat 2.0")
    if(!is.null(expected.label)) .Deprecated(msg = "argument expected.label is deprecated with testthat 2.0")
    
    # 1. Capture object and label
    act <- quasi_label(rlang::enquo(object))
    
    # 2. Call expect()
    act$body <- as.character(body(object))

    expect(
      any(grepl(x = act$body, pattern = expected)),
      sprintf("'%s' not found in the body of %s", 
              expected, 
              act$lab),
      info
    )
    
    # 3. Invisibly return the value
    invisible(act$val)    
    
}

#' @title
#' Expect no forbidden function code
#' 
#' @description
#'  Test that a given code string does not exists in function.
#' 
#' @param object
#'   Function to check for mandatory code
#' @param forbidden
#'   Code string that are forbidden to use.
#' 
#' @keywords internal
#' 
#' @export
expect_no_forbidden_function_code <- 
  function(object, forbidden) 
  {
    checkmate::assert_string(forbidden)

    # 1. Capture object and label
    act <- quasi_label(rlang::enquo(object))
    
    # 2. Call expect()
    act$body <- as.character(body(object))
    
    expect(
      !any(grepl(x = act$body, pattern = forbidden)),
      sprintf("Forbidden code '%s' is found in the body of %s", 
              forbidden, 
              act$lab)
    )
    
    # 3. Invisibly return the value
    invisible(act$val)    
    
}




#' @title Depricated function: expect_self_contained
#' 
#' @description Function has been depricated and will be removed. Please use \code{\link{expect_function_self_contained}} instead.
#' 
#' @keywords internal
#' @export
expect_self_contained <- function(object, info = NULL, label = NULL){
  .Deprecated("expect_function_self_contained")
  expect_function_self_contained(object, info, label)
}


#' @title Depricated function: expect_package
#' 
#' @description Function has been depricated and will be removed. Please use \code{\link{expect_attached_package}} instead.
#' 
#' @keywords internal
#' @export
expect_package <- function(object, info = NULL, label = NULL){
  .Deprecated("expect_attached_package")
  expect_attached_package(object, info)
}