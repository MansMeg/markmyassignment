### Assignment structure ###

context("Mandatory tests")

test_that("Mandatory tests", {
  expect_object("namn")
  expect_package_not_used("cheating_package")
})

