context("bump-GitVersion.S3")
test_that("bump-GitVersion.S3", {

  skip("interactive only")
  
  ## Class instance //    
  ns <- bumpr::GitVersion.S3()  
  
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

context("bump-RPackageVersion.S3")
test_that("bump-RPackageVersion.S3", {

  skip("interactive only")

  ## Class instance //    
  ns <- bumpr::RPackageVersion.S3()
  
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
