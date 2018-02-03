context("Command functions")

test_that("Test command functions errors", {
  
  expect_error(ghc_find_local_repositories(), "prefix required")
  expect_message(ghc_find_local_repositories("test", folder = system.file(package = "ghctools")),
                 "found 0 git repositories")
  
  expect_error(ghc_run_git_command(command = "test"), "prefix required")
  expect_error(ghc_run_git_command("test"), "command required")
  
  expect_error(ghc_git_status(), "prefix required")
  expect_error(ghc_git_pull(), "prefix required")
  expect_error(ghc_git_push(), "prefix required")
  expect_error(ghc_git_commit(message = "test"), "prefix required")
  expect_error(ghc_git_commit(prefix = "test"), "message required")
  expect_error(ghc_git_add(filepath = "test"), "prefix required")
  expect_error(ghc_git_add(prefix = "test"), "file path is required")
  expect_error(ghc_git_remove(filepath = "test"), "prefix required")
  expect_error(ghc_git_remove(prefix = "test"), "file path is required")
  expect_error(ghc_git_create_branch(branch = "test"), "prefix required")
  expect_error(ghc_git_create_branch(prefix = "test"), "branch name required")
  expect_error(ghc_git_delete_branch(branch = "test"), "prefix required")
  expect_error(ghc_git_delete_branch(prefix = "test"), "branch name required")
  expect_error(ghc_git_switch_branch(), "prefix required")
  expect_error(ghc_git_discard_changes(), "prefix required")
  
})
