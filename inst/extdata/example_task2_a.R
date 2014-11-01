### Assignment : task2 ###

test_that("Marking task2", {
  expect_object("task2")
  expect_is(task2, "function")
  expect_function_arguments(task2, c("vector2"))
})
