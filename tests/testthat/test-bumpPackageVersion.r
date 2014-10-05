context("bumpPackageVersion-1")
test_that("bumpPackageVersion", {

  skip("interactive only")
  
  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/bumpr.test")
  } else {
    wd_0 <- setwd("tests/testthat/data/bumpr.test")
  }
  
  bumpPackageVersion()
  
  setwd(wd_0)
  
})
