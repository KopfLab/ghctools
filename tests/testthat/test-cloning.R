context("Cloning functions")

test_that("Test cloning functions errors", {
  
  # cloning 
  expect_error(ghc_clone_repositories(), "repos data frame required")
  expect_error(ghc_clone_repositories(data_frame(x = 1)), "missing column")
  expect_error(ghc_clone_repositories(data_frame(url = "url")), "missing column")
  expect_error(ghc_clone_repositories(data_frame(repository = "repo")), "missing column")
  
  expect_error(ghc_clone_assignment_repositories(), "organization required")
  expect_error(ghc_clone_assignment_repositories("organization"), "assignment prefix required")

})
