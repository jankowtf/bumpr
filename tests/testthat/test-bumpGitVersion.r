context("bumpGitVersion-A")
test_that("bumpGitVersion", {

  skip("interactive only")
  
  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/bumpr.test")
  } else {
    wd_0 <- setwd("tests/testthat/data/bumpr.test")
  }
  
  bumpGitVersion()
  
  setwd(wd_0)
  
})
