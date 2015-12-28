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
#' @param reporter to use. Default is the 'summary' or specified in assignment yml file.
#' 
#' @examples
#' assignment_path <- 
#'  paste0(system.file(package = "markmyassignment"), "/extdata/example_assignment01.yml")
#' set_assignment(assignment_path)
#' source(paste0(system.file(package = "markmyassignment"), "/extdata/example_lab_file.R"))
#' mark_my_assignment()
#' 
#' @export
mark_my_assignment <- function(tasks = NULL, mark_file = NULL, force_get_tests = FALSE, quiet = FALSE, reporter = NULL){
  assert_function_arguments_in_API(
    tasks = tasks, mark_file = mark_file, force_get_tests = force_get_tests,
    quiet = quiet, reporter = reporter)
  if(!is.null(mark_file)){
    .Deprecated("mark_my_file", old = "mark_file")
  }
  get_tests(tasks = tasks, force_get_tests = force_get_tests)
  if(is.null(reporter)) reporter <- get_mark_my_reporter()
  test_results <- run_test_suite("mark_my_assignment", tasks, mark_file, quiet, reporter = reporter)
  if(!any(test_results$error) & sum(test_results$failed) == 0 & is.null(tasks) & !quiet) cheer()
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
#' @param tasks
#'   Which task should be corrected (if more than one). 
#'   Default is all. To see the different task, see \code{\link{show_tasks}}.
#' @param force_get_tests
#'   Force download of test files before marking of assignments. Default is FALSE.
#'   
#' @export
mark_my_dir <- function(directory, tasks = NULL, force_get_tests = FALSE){
  file_names <- dir(directory, pattern = "\\.[Rr]")
  files_to_mark <- paste0(directory, "/", file_names)
  if(length(ls(.GlobalEnv)) > 0) stop("Clean global environment before running tests on file.", call. = FALSE)
  if(length(files_to_mark) == 0) stop("No files to mark.")
  for(i in seq_along(files_to_mark)){ #i <- 1
    res_mark_temp <- try(
      mark_my_assignment(tasks = tasks, 
                         mark_file = files_to_mark[i], 
                         force_get_tests = force_get_tests, 
                         quiet = TRUE), silent=TRUE)
    force_get_tests <- FALSE
    if(class(res_mark_temp) == "try-error") {
      res_mark_temp[1]
      message(file_names[i], " could not be marked.")
    } else if (!exists(x = "res_mark")){
      res_mark_temp$marked_file <- file_names[i]
      res_mark <- res_mark_temp
    } else {
      res_mark_temp$marked_file <- file_names[i]
      res_mark <- rbind(res_mark, res_mark_temp)
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
#' @param reporter
#'   Reporter to use. Standard is student.
#'
#' @return
#'   test_suite results
#'   
run_test_suite <- function(caller, tasks = NULL, mark_file = NULL, quiet = FALSE, reporter = "summary"){
  
  test_directory <- mark_my_test_dir()
  
  if(caller == "mark_my_assignment" & is.null(mark_file))
    mark_my_env <- new.env(parent = .GlobalEnv)
  else
    mark_my_env <- new.env(parent = parent.env(env = .GlobalEnv))
  
  if(!is.null(mark_file)){
    stop_if_circular_calls(mark_file)
    source(file = mark_file, local = mark_my_env)
  } 
  
  if(quiet) reporter <- "silent"
  
  if(is.null(tasks)) tasks <- "all" else tasks <- paste(c("00mandatory", paste(tasks, collapse="|")), collapse="|")
  
  if(tasks == "all") tasks <- NULL
  test_res <- test_dir(path = test_directory, 
                       filter = tasks, 
                       reporter = reporter, env = mark_my_env)
  test_res
}


#' @title
#'  Functions to create directories
#'  
#' @description
#'  Functions to create directories
#'  
#' @name directories
#' 
mark_my_base_dir <- function() paste0(tempdir(), "/markmyassignment")

#' @rdname directories
#' @param no assignment number
mark_my_assignment_dir <- function(no = 1) paste0(mark_my_base_dir(), "/assignment", no)

#' @rdname directories
#' @param ... to send to \code{\link{mark_my_assignment_dir}}
mark_my_test_dir <- function(...) paste0(mark_my_assignment_dir(...), "/tests")



#' @title
#'  Cheer when all tasks pass
#'  
#' @description
#' Cheer when all tasks pass
cheer <- function() {
  cat(sample(x = c("Yay! All done!",
                   "Good work!",
                   "You're a coding rockstar!",
                   "Keep up the good work!",
                   "Everything's correct!"), 1))
}

#' @title
#'  Get reporter from yml file
#'  
#' @description
#'  Get reporter from yml file
#'  
#'  Default reporter is 'summary'. 
#'  
get_mark_my_reporter <-function(){
  assign_yml <- read_assignment_yml()
  if("reporter" %in% names(assign_yml)){
    reporter <- assign_yml$reporter
  } else {
    reporter <- "summary"
  }
  reporter
}

#' @title
#'  Checks and stop if there are circular calls 
#'  
#' @description
#'  Checks and stop if there are circular calls 
#'  
#' @param mark_file File to check
#'  
stop_if_circular_calls <- function(mark_file){
  forbidden <- c("mark_my_assignment", "mark_my_dir", "set_assignment")
  forbidden_exist_in_code <- 
    lapply(forbidden, FUN = grepl,  x = as.character(parse(mark_file)))
  if(any(unlist(forbidden_exist_in_code))){
    res <- unlist(lapply(forbidden_exist_in_code, any))
    error <- unique(forbidden[res])
    stop(paste0("Please remove circular calls (", paste(error, collapse = "(), "),"()) from file."), call. = FALSE)    
  }
}
