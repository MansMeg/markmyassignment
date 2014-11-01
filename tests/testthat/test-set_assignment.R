
context("set_assignment")

test_that(desc="set_assignment()",{
  correct_url <- "https://raw.githubusercontent.com/MansMeg/KursRprgm/master/Labs/Test/d1.yml"
  wrong_url1 <- "https://raw.githubusercontent.com/MansMeg/KursRprgm/master/Labs/Test/uppgift1Test.R"
  wrong_url2 <- "https://raw.githubusercontent.com/MansMeg/KursRprgm/master/Labs/Test/blahblah"
  super_wrong_path <- "XXX"
  expect_is(suppressMessages(set_assignment(correct_url)), "character")
  expect_error(set_assignment(wrong_url1))
  expect_error(set_assignment(wrong_url2))
  expect_error(set_assignment(super_wrong_path))
})

test_that(desc="show_tasks()",{
  correct_url <- "https://raw.githubusercontent.com/MansMeg/KursRprgm/master/Labs/Test/d1.yml"
  suppressMessages(set_assignment(correct_url))
  expect_equal(show_tasks(), c("uppgift1","uppgift2","uppgift3","uppgift4","uppgift5"))
})
