### Assignment : task2 ###

context("task2")

test_that("Mark even more on task2", {
  expect_is(task2(5:10), "integer")
  expect_equal(length(task2(5:10)), 1)
  expect_equal(task2(vector=5:10), 15)
  expect_equal(task2(vector=c(8,1,1,1,0,5)), 13)
})
