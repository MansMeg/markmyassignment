
context("expectations")

test_that(desc="expect_self_contained()",{
  h <- "1"
  f <- function() h
  g <- function() h <- 1; h
  res <- is_self_contained()(f)
  expect_that(res$passed, is_false())
  expect_self_contained(g)  
})


test_that(desc="expect_package()",{
  expect_package("base")
  res <- use_package()("fake_package_name")
  expect_that(res$passed, is_false())
})


test_that(desc="expect_function_arguments()",{
  f <- function(x) x^2
  expect_function_arguments(f, "x")
  res <- has_function_arguments(c("x", "y"))(f)
  expect_that(res$passed, is_false())
  res <- has_function_arguments(c("X"))(f)
  expect_that(res$passed, is_false())
})


test_that(desc="expect_function_code()",{
  expect_function_code(object = base::mean, expected = "UseMethod")
  res <- function_code("markmyassignment")(base::mean)
  expect_that(res$passed, is_false())
})

