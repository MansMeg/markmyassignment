
context("mark_my_assignment")

test_that(desc="mark_my_assignment()",{
  suppressMessages(set_assignment(paste0(system.file(package = "markmyassignment"), "/extdata/example_assignment01.yml")))
  
  if(length(ls(name = .GlobalEnv)) == 0){
    expect_is(mark_my_assignment(mark_file = paste0(system.file(package = "markmyassignment"), "/extdata/example_lab_file.R"), quiet = TRUE), "data.frame")  
  }
  
  source(paste0(system.file(package = "markmyassignment"), "/extdata/example_lab_file.R"))
  
  expect_is(capture.output(mark_my_assignment()), "character")
  expect_equal(capture.output(mark_my_assignment())[1], "Mandatory tests : ..")
  expect_is(mark_my_assignment(quiet = TRUE), "data.frame")
  expect_is(mark_my_assignment(tasks = "task1", quiet = TRUE), "data.frame")
  expect_equal(nrow(mark_my_assignment(tasks = "task1", quiet = TRUE)), 2)
  expect_is(mark_my_assignment(tasks = c("task1", "task2"), quiet = TRUE), "data.frame")
  expect_is(mark_my_assignment(force_get_tests = TRUE, quiet = TRUE), "data.frame")
  expect_error(mark_my_assignment(mark_file = paste0(system.file(package = "markmyassignment"), "/extdata/example_lab_file.R"), quiet = TRUE))

})

test_that(desc="mark_my_dir()",{
  suppressMessages(set_assignment(paste0(system.file(package = "markmyassignment"), "/extdata/example_assignment01.yml")))
  if(length(ls(name = .GlobalEnv)) == 0){
    expect_is(mark_my_dir(paste0(system.file(package = "markmyassignment"), "/extdata/example_dir")), class = "data.frame")
    expect_equal(nrow(mark_my_dir(paste0(system.file(package = "markmyassignment"), "/extdata/example_dir"))), 8)
  }
})
