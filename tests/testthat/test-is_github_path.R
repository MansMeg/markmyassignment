
context("is_github_path")

test_that(desc="expect_function_self_contained()",{
  expect_true(is_github_path("https://api.github.com/repos/octokit/octokit.rb/contents/README.md"))
  expect_true(is_github_path("https://github.com/octokit/octokit.rb/blob/master/README.md"))
  expect_true(is_github_path("https://raw.githubusercontent.com/octokit/octokit.rb/master/README.md"))
  expect_false(is_github_path("https://google/octokit/octokit.rb/blob/github/README.md"))

  expect_names(names(is_github_path("https://api.github.com/repos/octokit/octokit.rb/contents/README.md")), identical.to = "api")
  expect_names(names(is_github_path("https://github.com/octokit/octokit.rb/blob/master/README.md")), identical.to = "http")
  expect_names(names(is_github_path("https://raw.githubusercontent.com/octokit/octokit.rb/master/README.md")), identical.to = "raw")
  expect_null(names(is_github_path("https://google/octokit/octokit.rb/blob/github/README.md")))
})
