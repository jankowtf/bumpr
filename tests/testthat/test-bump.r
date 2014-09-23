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
  
  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/bumpr.test")
  } else {
    wd_0 <- setwd("tests/testthat/data/bumpr.test")
  }
  getwd()
  bump(what = ns)
  
  }
)
