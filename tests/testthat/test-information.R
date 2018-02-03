context("Information functions")

test_that("Test information functions errors", {
  
  expect_error(ghc_repos_get_github_information(prefix = "test"), "organization required")
  expect_error(ghc_repos_get_github_information(org = "test"), "assignment prefix required")
  expect_error(ghc_repos_get_github_information("test", "test"), "access token required")
  
})
