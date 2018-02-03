context("Command functions")

test_that("Test command functions errors", {
  
  # relevant utils
  expect_error(find_repositories(), "prefix required")
  expect_message(find_repositories("test", folder = system.file(package = "ghctools")),
                 "found 0 git repositories")
  
  # general command
  expect_error(ghc_repos_run_git_command(command = "test"), "prefix required")
  expect_error(ghc_repos_run_git_command("test"), "command required")
  
  # cloning 
  expect_error(ghc_clone_repositories(), "repos data frame required")
  expect_error(ghc_clone_repositories(data_frame(x = 1)), "missing column")
  expect_error(ghc_clone_repositories(data_frame(url = "url")), "missing column")
  expect_error(ghc_clone_repositories(data_frame(repository = "repo")), "missing column")
  expect_error(ghc_repos_git_clone(prefix = "test"), "organization required")
  expect_error(ghc_repos_git_clone(org = "test"), "prefix required")
  
  # other standard commands
  expect_error(ghc_repos_git_status(), "prefix required")
  expect_error(ghc_repos_git_pull(), "prefix required")
  expect_error(ghc_repos_git_push(), "prefix required")
  expect_error(ghc_repos_git_commit(message = "test"), "prefix required")
  expect_error(ghc_repos_git_commit(prefix = "test"), "message required")
  expect_error(ghc_repos_git_add(filepath = "test"), "prefix required")
  expect_error(ghc_repos_git_add(prefix = "test"), "file path is required")
  expect_error(ghc_repos_git_remove(filepath = "test"), "prefix required")
  expect_error(ghc_repos_git_remove(prefix = "test"), "file path is required")
  expect_error(ghc_repos_git_create_branch(branch = "test"), "prefix required")
  expect_error(ghc_repos_git_create_branch(prefix = "test"), "branch name required")
  expect_error(ghc_repos_git_delete_branch(branch = "test"), "prefix required")
  expect_error(ghc_repos_git_delete_branch(prefix = "test"), "branch name required")
  expect_error(ghc_repos_git_switch_branch(), "prefix required")
  expect_error(ghc_repos_git_discard_changes(), "prefix required")
  
})


test_that("Test command function returns", {
  
  # general command
  expect_message(retval <- ghc_repos_run_git_command("DNE", "git status"), 
                 "executing .*git.* for 0 repositories")
  expect_equal(retval, "DNE")
  
  # cloning -- cannot test with out token
  
  # other standard commands
  expect_message(retval <- ghc_repos_git_status("DNE"), 
                 "executing .*git status.* for 0 repositories")
  expect_equal(retval, "DNE")
  
  expect_message(retval <- ghc_repos_git_pull("DNE"), 
                 "executing .*git pull.* for 0 repositories")
  expect_equal(retval, "DNE")
  
  expect_message(retval <- ghc_repos_git_push("DNE"), 
                 "executing .*git push.* for 0 repositories")
  expect_equal(retval, "DNE")
  
  expect_message(retval <- ghc_repos_git_commit("DNE", message = "test"), 
                 "executing .*git commit.* for 0 repositories")
  expect_equal(retval, "DNE")
  
  expect_message(retval <- ghc_repos_git_add("DNE", filepath = "test"), 
                 "executing .*git add.* for 0 repositories")
  expect_equal(retval, "DNE")
  
  expect_message(retval <- ghc_repos_git_remove("DNE", filepath = "test"), 
                 "executing .*git rm.* for 0 repositories")
  expect_equal(retval, "DNE")
  
  expect_message(retval <- ghc_repos_git_create_branch("DNE", branch = "test"), 
                 "executing .*git checkout.* for 0 repositories")
  expect_equal(retval, "DNE")
  
  expect_message(retval <- ghc_repos_git_delete_branch("DNE", branch = "test"), 
                 "executing .*git branch.* for 0 repositories")
  expect_equal(retval, "DNE")
 
  expect_message(retval <- ghc_repos_git_switch_branch("DNE", branch = "test"), 
                 "executing .*git checkout test.* for 0 repositories")
  expect_equal(retval, "DNE")
  
  expect_message(retval <- ghc_repos_git_discard_changes("DNE"), 
                 "executing .*git checkout -- \\..* for 0 repositories")
  expect_equal(retval, "DNE")
  
})
