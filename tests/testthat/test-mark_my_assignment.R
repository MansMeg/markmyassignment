
context("mark_my_assignment")

test_that(desc="mark_my_assignment()",{
  suppressMessages(set_assignment(paste0(system.file(package = "markmyassignment"), "/extdata/example_assignment01.yml")))
  
  if(length(ls(name = .GlobalEnv)) == 0){
    expect_is(mark_my_assignment(mark_file = paste0(system.file(package = "markmyassignment"), "/extdata/example_lab_file.R"), quiet = TRUE), "testthat_results")  
    expect_error(mark_my_assignment(mark_file = paste0(system.file(package = "markmyassignment"), "/extdata/example_lab_file_circular.R"), quiet = TRUE))
  }
  
  source(paste0(system.file(package = "markmyassignment"), "/extdata/example_lab_file.R"))
  
  expect_is(capture.output(mark_my_assignment()), "character")
  expect_equal(capture.output(mark_my_assignment())[1], "Marking assignment...")
  expect_is(mark_my_assignment(quiet = TRUE), "testthat_results")
  expect_true(sum(grepl(x = capture.output(mark_my_assignment(tasks = "task1")), pattern = "Marking assignment..."))==1)
  expect_is(mark_my_assignment(tasks = "task1", quiet = TRUE), "testthat_results")
  expect_equal(length(mark_my_assignment(tasks = "task1", quiet = TRUE)), 2)
  expect_is(mark_my_assignment(tasks = c("task1", "task2"), quiet = TRUE), "testthat_results")
  expect_is(mark_my_assignment(force_get_tests = TRUE, quiet = TRUE), "testthat_results")
  expect_warning(mark_my_assignment(mark_file = paste0(system.file(package = "markmyassignment"), "/extdata/example_lab_file.R"), quiet = TRUE))
  
  
})


test_that(desc="Assertions on arguments in mark_my_assignment()",{
  
  suppressMessages(set_assignment(paste0(system.file(package = "markmyassignment"), "/extdata/example_assignment01.yml")))
  source(paste0(system.file(package = "markmyassignment"), "/extdata/example_lab_file.R"))
  
  expect_warning(mark_my_assignment(tasks = "no such task", quiet = TRUE))
  expect_warning(mark_my_assignment(tasks = c("task1", "no such task", "task2"), quiet = TRUE))
  expect_error(mark_my_assignment(tasks = task2, quiet = TRUE))
  expect_error(mark_my_assignment(mark_file = "~/no such directory/no such file.R"))
  expect_error(mark_my_assignment(quiet = "TRUE"))
  expect_error(mark_my_assignment(force_get_tests = "TRUE"))
  expect_error(mark_my_assignment(reporter = StudentReporter))
  
})


test_that(desc="mark_my_dir()",{
  suppressMessages(set_assignment(paste0(system.file(package = "markmyassignment"), "/extdata/example_assignment01.yml")))
  if(length(ls(name = .GlobalEnv)) == 0){
    expect_is(mark_my_dir(paste0(system.file(package = "markmyassignment"), "/extdata/example_dir")), class = "data.frame")
    expect_equal(nrow(mark_my_dir(paste0(system.file(package = "markmyassignment"), "/extdata/example_dir"))), 8)
  }
})
