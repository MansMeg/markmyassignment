#' @title
#' Mark assignment in global environment
#' 
#' @description
#' Mark assignment in global environment.
#' 
#' @param tasks
#'   Which task should be corrected (if more than one). Default is all. 
#'   To see the different task, see \code{\link{show_tasks}}.
#' @param mark_file
#'   Argument is deprecated, use mark_my_file instead.
#' @param force_get_tests
#'   Argument is deprecated, use \code{set_assignment()} instead.
#' @param quiet
#'   Should test be run without output?
#' @param ... further arguments sent to \code{test_dir()}.
#' 
#' @examples
#' assignment_path <- 
#'   file.path(system.file(package = "markmyassignment"), "extdata", "example_assignment01.yml")
#' set_assignment(assignment_path)
#' source(file.path(system.file(package = "markmyassignment"), "extdata", "example_lab_file.R"))
#' mark_my_assignment()
#' 
#' @export
mark_my_assignment <- function(tasks = NULL, mark_file = NULL, force_get_tests = FALSE, quiet = FALSE, ...){
  checkmate::assert_character(tasks, null.ok = TRUE)
  if(!is.null(mark_file)) checkmate::assert_file_exists(mark_file)
  checkmate::assert_flag(force_get_tests)
  checkmate::assert_flag(quiet)
  
  if(!is.null(mark_file)){
    .Deprecated("mark_my_file", old = "mark_file")
  }
  if(force_get_tests){
    .Deprecated("set_assignment()", old = "force_get_tests")
  }
  
  # Run test suites 
  if(quiet){
    capture_output(test_results <- suppressMessages(run_test_suite(caller = "mark_my_assignment", tasks, mark_file, quiet, ...)))
  } else {
    test_results <- run_test_suite(caller = "mark_my_assignment", tasks, mark_file, quiet, ...)
  }

  
  # Put together file for multiple checks
  test_results_df <- as.data.frame(test_results) 
  if(!any(test_results_df$error) & sum(test_results_df$failed) == 0 & is.null(tasks) & !quiet) cheer()

  return(invisible(test_results))
}


#' @title
#'   Run test suite
#' 
#' @description
#'   Runs test on the tasks. Always run mandatory tests.
#' 
#' @param caller
#'   Either "mark_my_assignment" or "mark_my_file"
#' @param tasks
#'   Which task should be tested
#' @param mark_file
#'   Run tests on a R-file. Default is NULL means global environment.
#' @param quiet
#'   Should the output be supressed (only returning test results)
#' @param ... further arguments sent to \code{test_dir()}.
#'
#' @return
#'   test_suite results
#'   
#' @keywords internal
#'   
run_test_suite <- function(caller, tasks = NULL, mark_file = NULL, quiet = FALSE, ...){
  checkmate::assert_choice(caller, c("mark_my_assignment", "mark_my_file"))
  checkmate::assert_character(tasks, null.ok = TRUE)
  if(!is.null(mark_file)) checkmate::assert_file_exists(mark_file)
  checkmate::assert_flag(quiet)
  
  # mark_my_tasks_dir <- markmyassignment:::mark_my_tasks_dir
  # test_directory <- "C:/Users/appveyor/AppData/Local/Temp/"
  test_directory <- mark_my_tasks_dir()
  
  if(caller == "mark_my_assignment" & is.null(mark_file)){
    mark_my_env <- new.env(parent = .GlobalEnv)
  } else {
    mark_my_env <- new.env(parent = parent.env(env = .GlobalEnv))
  }
    
  if(!is.null(mark_file)){
    mark_file <- delete_circular_calls(mark_file)
    tf_path <- tempfile(pattern = "mark_file", fileext = ".txt")
    writeLines(text = mark_file, con = tf_path)
    source(file = tf_path, local = mark_my_env)
    unlink(x = tf_path)
  } 

  # tasks <- "task2"
  # translate_tasks_name_to_task_files <- markmyassignment:::translate_tasks_name_to_task_files
  tasks_fn <- translate_tasks_name_to_task_files(tasks)
  if(!is.null(tasks)) {
    tasks_filter <- paste(c("mandatory", paste(gsub("test-|\\.R", "", tasks_fn), collapse="|")), collapse="|")
  } else {
    tasks_filter <- NULL
  }
  
  # Source in before code
  # mark_my_run_code_dir <- markmyassignment:::mark_my_run_code_dir
  run_code_paths <- dir(mark_my_run_code_dir())
  before_paths <- file.path(mark_my_run_code_dir(), run_code_paths[grepl("before",run_code_paths)])
  for(i in seq_along(before_paths)) source(file = before_paths[i], local = mark_my_env)
    
  test_res <- test_dir(path = test_directory, 
                       filter = tasks_filter, 
                       env = mark_my_env,
                       ...)
  
  # Source in after code
  after_paths <- file.path(mark_my_run_code_dir(), run_code_paths[grepl("after",run_code_paths)])
  for(i in seq_along(after_paths)) source(file = after_paths[i], local = mark_my_env)
  
  test_res
}

#' @title
#'  Cheer when all tasks pass
#'  
#' @description
#' Cheer when all tasks pass
#' 
#' @keywords internal
#' 
cheer <- function() {
  cat(sample(x = c("Yay! All done!",
                   "Good work!",
                   "You're a coding rockstar!",
                   "Keep up the good work!",
                   "Everything's correct!"), 1))
}

#' @title
#'  Checks and deletes circular calls 
#'  
#' @description
#'  Checks and deletes circular calls 
#'  
#' @param mark_file File to check
#'  
#' @return
#'  Character vector of the possibly changed mark file
#'  
#' @keywords internal
#' 
delete_circular_calls <- function(mark_file){
  checkmate::assert_file_exists(mark_file)
  # mark_file <- "inst/extdata/example_lab_file_circular.R"
  txt_in <- txt_out <- as.character(parse(mark_file))
  forbidden <- forbidden_functions()
  forbidden_string <- paste(forbidden, "()", sep = "")
  regex <- paste("(^|;| )", forbidden, "\\(.*\\)", sep = "")
  msg_statement <- msg_forbidden <- character(0)
  remove_row <- logical(length(txt_in))
  forbidden_funs <- logical(length(regex))
  
  for(i in seq_along(txt_in)){
    for(j in seq_along(regex)){
      forbidden_funs[j] <- grepl(pattern = regex[j], txt_out[i])
    }
    if(any(forbidden_funs)){
      remove_row[i] <- TRUE
      msg_statement <- c(msg_statement, txt_in[i])
      msg_string <- paste(forbidden_string[forbidden_funs], collapse = ", ")
      msg_forbidden <- c(msg_forbidden, msg_string)
    }
  }

  warn_msg <- paste0("\n> ", msg_statement, "\ndue to: ", msg_forbidden, "\n")
    
  if(any(remove_row)){
    warning("The following statement(s) were ignored/removed when running mark_my_file():\n",
            warn_msg)
    txt_out <- txt_in[!remove_row]
  }
  return(txt_out)
}

#' Forbidden functions for \code{mark_my_file()}
#' 
#' @description 
#' Funcations that are removed prior to running \code{mark_my_file()}. 
#' The purpose is to avoid reinstalling packages, doing system calls or similar when checking a file.
#'  
#' @keywords internal
forbidden_functions <- function(){
  ns <- ls(name = "package:markmyassignment")
  forbidden <- c("install.packages", "utils::install.packages",
  "devtools::install_github", "install_github", "data", "system")
  c(forbidden, ns, paste0("markmyassignment::", ns))
}



#' Get task file name from task names
#' 
#' @param tasks task names in assignment
#' @keywords internal
translate_tasks_name_to_task_files <- function(tasks){
  df <- assignment_paths_and_files()
  tasks_in_assignment <- as.character(unique(df$name[df$class == "tasks"]))
  checkmate::assert_subset(tasks, tasks_in_assignment)
  as.character(df$test_file[df$name %in% tasks])
}
