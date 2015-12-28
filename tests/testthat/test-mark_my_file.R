
context("mark_my_file")

test_that(desc="mark_my_file()",{
  
  source_file <- paste0(system.file(package = "markmyassignment"), "/extdata/example_lab_file.R")
  assignment_file <- paste0(system.file(package = "markmyassignment"), "/extdata/example_assignment01.yml")
  
  expect_is(mark_my_file(mark_file = source_file, lab_file = assignment_file, quiet = TRUE), "testthat_results")
  expect_error(mark_my_file(mark_file = paste0(system.file(package = "markmyassignment"), "/extdata/example_lab_file_circular.R"), lab_file = assignment_file, quiet = TRUE))
  expect_is(capture.output(mark_my_file(mark_file = source_file, lab_file = assignment_file)), "character")
  expect_equal(capture.output(mark_my_file(mark_file = source_file, lab_file = assignment_file))[1], "Marking assignment...")
  expect_true(sum(grepl(x = capture.output(mark_my_file(tasks = "task1", mark_file = source_file, lab_file = assignment_file)), pattern = "Marking assignment..."))==1)
  expect_is(mark_my_file(tasks = "task1", mark_file = source_file, lab_file = assignment_file, quiet = TRUE), "testthat_results")
  expect_equal(length(mark_my_file(tasks = "task1", mark_file = source_file, lab_file = assignment_file, quiet = TRUE)), 2)
  expect_is(mark_my_file(tasks = c("task1", "task2"), mark_file = source_file, lab_file = assignment_file, quiet = TRUE), "testthat_results")
  expect_is(mark_my_file(mark_file = source_file, lab_file = assignment_file, force_get_tests = TRUE, quiet = TRUE), "testthat_results")
  
})



test_that(desc="Assertions on arguments in mark_my_file()",{
  
  source_file <- paste0(system.file(package = "markmyassignment"), "/extdata/example_lab_file.R")
  assignment_file <- paste0(system.file(package = "markmyassignment"), "/extdata/example_assignment01.yml")
  
  expect_warning(mark_my_file(tasks = "no such task", mark_file = source_file, lab_file = assignment_file, quiet = TRUE))
  expect_error(mark_my_file(mark_file = source_file, lab_file = assignment_file, tasks = task2, quiet = TRUE))
  expect_error(mark_my_file(mark_file = source_file, lab_file = "~/no such directory/no such file.yml", quiet = TRUE))
  expect_error(mark_my_file(quiet = "TRUE", mark_file = source_file, lab_file = assignment_file))
  expect_error(mark_my_file(force_get_tests = "TRUE", mark_file = source_file, lab_file = assignment_file))
  expect_error(mark_my_file(reporter = StudentReporter, mark_file = source_file, lab_file = assignment_file))
  
})
