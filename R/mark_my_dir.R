#' @title
#' Mark assignments in a directory
#' 
#' @description
#' Marks assignments in a directory. Stores the results.
#' 
#' @param directory
#'   Directory with assignments files.
#' @param tasks
#'   Which task should be corrected (if more than one). Default is all. 
#'   To see the different task, see \code{\link{show_tasks}}.
#' @param assignment_path
#'   Assignment file to set before marking the assignment (url or local path).
#' @param force_get_tests
#'   Argument is deprecated, use \code{set_assignment()} instead.
#' @param quiet
#'   Should test be run without output?
#' @param ... further arguments sent to \code{test_dir()}.
#'   
#' @keywords internal
#'   
#' @export
mark_my_dir <- function(directory, tasks = NULL, assignment_path = NULL, force_get_tests = FALSE, quiet = FALSE, ...){
  checkmate::assert_directory_exists(directory)
  checkmate::assert_character(tasks, null.ok = TRUE)
  checkmate::assert_string(assignment_path, null.ok = TRUE)
  checkmate::assert_flag(force_get_tests)
  
  if(force_get_tests){
    .Deprecated("set_assignment()", old = "force_get_tests")
  }
  
  if(!is.null(assignment_path)) {
    old_warn_opt <- options(warn = 2)
    set_assgn_result <- try(suppressMessages(set_assignment(assignment_path)), silent = TRUE)
    options(warn = old_warn_opt$warn)
    if(methods::is(set_assgn_result, "try-error"))
      stop(set_assgn_result[1])
  }
  
  file_names <- dir(directory, pattern = "\\.[Rr]")
  if(length(file_names) == 0) stop("No files to mark.")
  
  files_to_mark <- paste0(directory, "/", file_names)
  res_mark <- vector(mode = "list", length = length(files_to_mark))
  names(res_mark) <- file_names
  
  for(i in seq_along(files_to_mark)){
    res_mark_temp <- try(
      mark_my_file(tasks = tasks, 
                   mark_file = files_to_mark[i],
                   assignment_path = assignment_path,
                   force_get_tests = FALSE, 
                   quiet = quiet, ...), silent=TRUE)

    if(inherits(res_mark_temp, "try-error")) {
      if(!quiet) message(file_names[i], " could not be marked.")
      res_mark[[i]] <- as.character(res_mark_temp[1])
    } else {
      res_mark[[i]] <- res_mark_temp
      if(!quiet) message(paste(file_names[i], "was marked."))
    }
  }
  return(res_mark)
}

