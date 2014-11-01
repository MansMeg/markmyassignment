
context("set_assignment")

test_that(desc="set_assignment()",{
  correct_url <- "https://raw.githubusercontent.com/MansMeg/markmyassignment/master/inst/extdata/example_assignment.yml"
  wrong_url1 <- "https://raw.githubusercontent.com/MansMeg/markmyassignment/master/inst/extdata/example_lab_file.R"
  wrong_url2 <- "https://raw.githubusercontent.com/MansMeg/markmyassignment/master/inst/extdata/file_that_do_not_exist.R"
  super_wrong_path <- "XXX"
  correct_local <- paste0(system.file(package = "markmyassignment"), "/extdata/example_assignment.yml")
  wrong_local1 <- paste0(system.file(package = "markmyassignment"), "/extdata/example_lab_file.R")
  wrong_local2 <- paste0(system.file(package = "markmyassignment"), "/file_that_do_not_exist.R")
  
  expect_is(suppressMessages(set_assignment(correct_url)), "character")
  expect_is(suppressMessages(set_assignment(correct_local)), "character")
  expect_error(set_assignment(wrong_url1))
  expect_error(set_assignment(wrong_url2))
  expect_error(set_assignment(wrong_local1))
  expect_error(set_assignment(wrong_local2))
  expect_error(set_assignment(super_wrong_path))
})

test_that(desc="show_tasks()",{
  correct_local <- paste0(system.file(package = "markmyassignment"), "/extdata/example_assignment.yml")
  suppressMessages(set_assignment(correct_local))
  expect_equal(show_tasks(), c("task1","task2"))
})
