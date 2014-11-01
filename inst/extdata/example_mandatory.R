### Assignment structure ###

context("Mandatory tests")

test_that("Mandatory tests", {
  expect_object("my_name")
  expect_package_not_used("cheating_package")
})

