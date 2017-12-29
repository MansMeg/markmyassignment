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
#'   Force download of test files before marking of assignments. Default is FALSE.
#' @param quiet
#'   Should test be run without output?
#' @param ... further arguments sent to \code{test_dir()}.
#' 
#' @examples
#' assignment_path <- 
#'  paste0(system.file(package = "markmyassignment"), "/extdata/example_assignment01.yml")
#' set_assignment(assignment_path)
#' source(paste0(system.file(package = "markmyassignment"), "/extdata/example_lab_file.R"))
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
  check_existance_tasks(tasks = tasks)
  
  return(invisible(test_results))
}

#' @title
#' Mark assignments in a directory
#' 
#' @description
#' Marks assignments in a directory. Stores the results.
#' 
#' @param directory
#'   Directory with assignments files.
#' @param lab_file
#'   Assignment file to set before marking the assignment (url or local path).
#' @param tasks
#'   Which task should be corrected (if more than one). 
#'   Default is all. To see the different task, see \code{\link{show_tasks}}.
#' @param force_get_tests
#'   Force download of test files before marking of assignments. Default is FALSE.
#'   
#' @keywords internal
#'   
#' @export
mark_my_dir <- function(directory, lab_file, tasks = NULL, force_get_tests = FALSE){
  checkmate::assert_directory_exists(directory)
  checkmate::assert_file_exists(lab_file)
  checkmate::assert_character(tasks, null.ok = TRUE)
  checkmate::assert_flag(force_get_tests)
  
  file_names <- dir(directory, pattern = "\\.[Rr]")
  if(length(file_names) == 0) stop("No files to mark.")
  
  files_to_mark <- paste0(directory, "/", file_names)
  res_mark <- vector(mode = "list", length = length(files_to_mark))
  names(res_mark) <- file_names
  
  for(i in seq_along(files_to_mark)){
    res_mark_temp <- try(
      mark_my_file(tasks = tasks, 
                   mark_file = files_to_mark[i],
                   assignment_path = lab_file,
                   force_get_tests = force_get_tests, 
                   quiet = TRUE), silent=TRUE)
    force_get_tests <- FALSE
    if(class(res_mark_temp) == "try-error") {
      message(res_mark_temp[1])
      message(file_names[i], " could not be marked.")
      res_mark[[i]] <- as.character(res_mark_temp[1])
    } else {
      res_mark[[i]] <- res_mark_temp
      print(paste(file_names[i], "was marked."))
    }
  }
  return(res_mark)
}



#' @title
#' Get test files
#' 
#' @description
#' Downloads the test files for the current assignment and save them to 
#' temp directory.
#' 
#' @param tasks
#'   Which task should be downloaded. Default is "all".
#' @param force_get_tests
#'   Force download/get test (ignore cached tests).
#' 
#' @keywords internal
#' 
get_tests <- function(tasks = NULL, force_get_tests = FALSE){
  assignment <- read_assignment_yml()
  dir.create(path = mark_my_test_dir(), recursive = TRUE, showWarnings = FALSE)
  
  tasks_to_get <- names(assignment$tasks)
  if(!is.null(tasks)) tasks_to_get <- tasks_to_get[tasks_to_get %in% tasks]
  if(!force_get_tests) tasks_to_get <- tasks_to_get[!tasks_to_get %in% cached_tasks()]
  
  for(task in tasks_to_get) {
    for(i in seq_along(assignment$tasks[[task]]$url)){
      dest <- paste0(mark_my_test_dir(), "/test-", task, "-", i, ".R")
      path <- path_type(assignment$tasks[[task]]$url[i]) 
      get_file(path = path, dest = dest)
    }
  }
  
  if(force_get_tests | !"00mandatory" %in% cached_tasks()){
    for(i in seq_along(assignment$mandatory$url)){
      dest <- paste0(mark_my_test_dir(), "/test-00mandatory-", i, ".R")
      path <- path_type(assignment$mandatory$url[i]) 
      get_file(path = path, dest = dest)
    }
  }
  return(invisible(TRUE))
}

#' @title
#' Cached tasks
#' 
#' @description
#'   Checks which assignments that are cached (ie already downloaded to temp dir).
#' 
#' @return
#'   character vector with cached assignments.
#' 
#' @keywords internal
#' 
cached_tasks <- function(){    
  files <- dir(mark_my_test_dir())
  unique(unlist(lapply(strsplit(files, split = "-"), FUN=function(X) X[2])))
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
  if(!is.null(tasks)) tasks_filter <- paste(c("mandatory", paste(gsub("test-|\\.R", "", tasks_fn), collapse="|")), collapse="|")
  
  # Source in before code
  run_code_paths <- dir(mark_my_run_code_dir())
  before_paths <- paste0(mark_my_run_code_dir(), "/", run_code_paths[grepl("before",run_code_paths)])
  for(i in seq_along(before_paths)) source(file = before_paths[i], local = mark_my_env)
    
  test_res <- test_dir(path = test_directory, 
                       filter = tasks_filter, 
                       env = mark_my_env,
                       ...)
  
  # Source in after code
  after_paths <- paste0(mark_my_run_code_dir(), "/", run_code_paths[grepl("after",run_code_paths)])
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
  txt_in <- txt_out <- as.character(parse(mark_file))
  forbidden <- c(
    "mark_my_assignment", "mark_my_dir", "set_assignment", "mark_my_file",
    "install.packages", "utils::install.packages",
    "devtools::install_github", "install_github", "data", "system")
  regex <- paste("(^|;| )", forbidden, "\\(.*\\)", sep = "")
  for(pattern in regex){
    txt_out <- gsub(pattern = pattern, replacement = "", x = txt_out)
  }
  if(!identical(txt_in, txt_out))
    message("The following statements were ignored when running mark_my_file:\n",
            paste(txt_in[txt_in != txt_out], collapse = "\n"))
  
  return(txt_out)
}


#' Get task file name from task names
#' 
#' @param tasks task names in assignment
#' 
translate_tasks_name_to_task_files <- function(tasks){
  df <- assignment_paths_and_files()
  tasks_in_assignment <- as.character(unique(df$name[df$class == "tasks"]))
  checkmate::assert_subset(tasks, tasks_in_assignment)
  as.character(df$test_file[df$name %in% tasks])
}
