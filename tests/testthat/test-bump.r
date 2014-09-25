context("bump-1")
test_that("bump", {

  skip("interactive only")
  
  ## Class instance //    
  ns Bumpr.GitVersion.S3reateInstance(cl = "Bumpr.Git.S3", 
    obj = list(
      version = character(),
      git_repos = "origin",
      user_email = character(),
      user_name = character(),
      home = Sys.getenv("HOME")
    )
  )
#   ns$user_email <- "janko.thyson@rappster.de"
#   ns$user_name <- "Janko Thyson"  
  
  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/bumpr.test")
  } else {
    wd_0 <- setwd("tests/testthat/data/bumpr.test")
  }
  
  bump(what = ns)
  
  setwd(wd_0)
  on.exit(setwd(wd_0))
  
  }
)

context("bump-Bumpr.RPackageVersion.S3")
test_that("bump", {

  skip("interactive only")

  ## Class instance //    
  ns <- classr::createInstance(cl = "Bumpr.RPackageVersion.S3", 
    obj = list(
      version = character()
    )
  )
  
  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/bumpr.test")
  } else {
    wd_0 <- setwd("tests/testthat/data/bumpr.test")
  }

  bump(what = ns)
  
  setwd(wd_0)
  on.exit(setwd(wd_0))
  
  }
)
