### Assignment structure ###

context("Mandatory tests")

test_that("Mandatory tests", {
  expect_true(exists("my_name"), "Variable my_name is missing")
  expect_package_not_used("cheating_package", info = "package 'cheating_package' is not allowed")
})

