### Assignment : task1 ###

context("task1")

test_that("Marking of task 1", {
  expect_true(exists("task1"))
  expect_object("task1")
  expect_is(task1, "numeric")
  expect_equal(task1, c(pi, exp(1)))
})
