#' @title
#' Mark assignment in global environment
#' 
#' @details
#' Mark assignment in global environment.
#' 
#' @param tasks
#'   Which task should be corrected (if more than one). Default is all. To see the different task, see show_task().
#' @param mark_file
#'   Run tests on a R-file. Default is NULL means global environment.
#' @param force_get_tests
#'   Force download of test files before marking of assignments. Default is FALSE.
#' @param quiet
#'   Should test be run without output?
#' 
#' @examples
#'   mark_my_assignment()
#'   mark_my_assignment()
#' 
#' @export
mark_my_assignment <- function(tasks = NULL, mark_file = NULL, force_get_tests = FALSE, quiet = FALSE){
  get_tests(tasks = tasks, force = force_get_tests)
  test_results <- run_test_suite(tasks, mark_file, quiet)
  if(sum(test_results$failed) == 0 & is.null(tasks)) cheer()
  return(invisible(test_results))
}



#' @title
#' Mark assignments in a directory
#' 
#' @details
#' Marks assignments in a directory. Stores the results.
#' 
#' @param directory
#'   Directory with assignments files.
#' @param force_get_tests
#'   Force download of test files before marking of assignments. Default is FALSE.
#' 
#' @examples
#'   directory <- paste0(tempdir(), "/gitmsge2926d41ba09.txt")
#'   directory <- tempdir()
#'   writeLines("test", tempfile(fileext = ".R"))
#'   
#' @export
mark_my_dir <- function(directory, tasks = NULL, force_get_tests = FALSE){
  files_to_mark <- dir(directory, pattern = "\\.[Rr]")
  if(length(files_to_mark) == 0) stop("No files to mark.")
  for(single_file in files_to_mark){
    res_mark <- 
      mark_my_assignment(tasks = tasks, 
                         mark_file = single_file, 
                         force_get_tests = force_get_tests, 
                         quiet = TRUE)
    force_get_tests <- FALSE
  }
  return(res_mark)
}



#' @title
#' Get test files
#' 
#' @details
#' Downloads the test files for the current assignment and save them to 
#' temp directory.
#' 
#' @param tasks
#'  Which task should be downloaded. Default is "all".
#' 
#' @examples
#'  get_tests(c("uppgift2", "uppgift5"), force_get_tests=TRUE, verbose=TRUE)
#'  get_tests(force_get_tests=TRUE, verbose=TRUE)
#'  get_tests(verbose=TRUE)
get_tests <- function(tasks = NULL, force_get_tests = FALSE, verbose = FALSE){
  assignment <- read_assignment_yml()
  dir.create(path = mark_my_test_dir(), recursive = TRUE, showWarnings = FALSE)
  
  tasks_to_get <- names(assignment$tasks)
  if(!is.null(tasks)) tasks_to_get <- tasks_to_get[tasks_to_get %in% tasks]
  if(!force_get_tests) tasks_to_get <- tasks_to_get[!tasks_to_get %in% cached_tasks()]
    
  for(task in tasks_to_get) {
    for(i in seq_along(assignment$tasks[[task]]$url)){
      dest <- paste0(mark_my_test_dir(), "/test-", task, "-", i, ".R")
      path <- path_type(assignment$tasks[[task]]$url[i]) 
      if(verbose) message("Downloading : ", path)
      get_file(path = path, dest = dest)
    }
  }
    
  if(force_get_tests | !"00mandatory" %in% cached_tasks()){
    for(i in seq_along(assignment$mandatory$url)){
      dest <- paste0(mark_my_test_dir(), "/test-00mandatory-", i, ".R")
      path <- path_type(assignment$mandatory$url[i]) 
      if(verbose) message("Downloading : ", path)
      get_file(path = path, dest = dest)
    }
  }
  return(invisible(TRUE))
}

#' @title
#' Cached tasks.
#' 
#' @details
#'   Checks which assignments that are cached (ie already downloaded to temp dir).
#' 
#' @return
#'   character vector with cached assignments.
#' 
#' @examples
#'  cached_tasks()
#' 
cached_tasks <- function(){    
  files <- dir(mark_my_test_dir())
  unique(unlist(lapply(strsplit(files, split = "-"), FUN=function(X) X[2])))
}


#' @title
#'   Run test suite.
#' 
#' @details
#'   Runs test on the tasks. Always run mandatory tests.
#' 
#' @param tasks
#'   Which task should be tested
#' @param mark_file
#'   Run tests on a R-file. Default is NULL means global environment.
#' @param quiet
#'   Should the output be supressed (only returning test results)
#'
#' 
#' @return
#'   test_suite results
#'   
#' @examples
#'   run_test_suite(tasks = "uppgift1")
#'   run_test_suite(tasks = c("uppgift1", "uppgift3"))
#'   run_test_suite(tasks = c("uppgift1", "uppgift3"), quiet = TRUE)
#'
run_test_suite <- function(tasks = NULL, mark_file = NULL, quiet = FALSE){
  
  test_directory <- mark_my_test_dir()
  
  if(!is.null(mark_file)){
    if(length(ls(.GlobalEnv)) > 0) stop("Clean global environment before running tests on file.")
    mark_my_env <- new.env()
    source(file = mark_file, local = mark_my_env)
  } else {
    mark_my_env <- test_env()
  }
  
  if(quiet) report <- "silent" else report <- "summary"
  
  if(is.null(tasks)) tasks <- "all" else tasks <- c("00mandatory", tasks)

  for(no in seq_along(tasks)){
    if(tasks[no] == "all") tasks <- NULL
    test_res_temp <- test_dir(path = test_directory, 
                              filter = tasks[no], 
                              reporter = report, env = mark_my_env)
    if(no == 1) {
      test_res <- test_res_temp
    } else {
      test_res <- rbind(test_res, test_res_temp)
    }
  }
  test_res
}


#' @title
#'  Functions to create directories
#'  
#' @name directories
#' 
mark_my_base_dir <- function() paste0(tempdir(), "/markmyassignment")

#' @rdname directories
mark_my_assignment_dir <- function(no = 1) paste0(mark_my_base_dir(), "/assignment", no)

#' @rdname directories
mark_my_test_dir <- function(...) paste0(mark_my_assignment_dir(...), "/tests")



#' @title
#'  Cheer when all tasks pass
#'  
cheer <- function() {
  cat(sample(x = c("Yay! All done!",
               "Good work!",
               "You're a coding rockstar!",
               "Keep up the good work!",
               "Everything's correct!"), 1))
}