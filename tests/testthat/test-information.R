context("Information functions")

test_that("Test information functions errors", {
  
  expect_error(ghc_find_assignment_repositories(), "organization required")
  expect_error(ghc_find_assignment_repositories("organization"), "assignment prefix required")
  expect_error(ghc_find_assignment_repositories("organization", "prefix"), "access token required")
  
})
