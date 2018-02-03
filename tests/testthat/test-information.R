context("Information functions")

test_that("Test information functions errors", {
  
  expect_error(ghc_repos_get_github_information(), "organization required")
  expect_error(ghc_repos_get_github_information("organization"), "assignment prefix required")
  expect_error(ghc_repos_get_github_information("organization", "prefix"), "access token required")
  
})
