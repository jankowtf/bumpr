context("bump-1")
test_that("bump", {

  ## Class instance //    
  ns <- classr::createInstance(cl = "Bumpr.Git.S3", 
    obj = list(
      version = character(),
      git_repos = "origin",
      user_email = character(),
      user_name = character(),
      home = Sys.getenv("HOME")
    )
  )
  ns$user_email <- "janko.thyson@rappster.de"
  ns$user_name <- "Janko Thyson"  
  
  bump(what = ns)
  
  }
)