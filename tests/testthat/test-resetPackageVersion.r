context("resetPackageVersion-A")
test_that("resetPackageVersion", {

  skip("interactive only")
  
  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/bumpr.test")
  } else {
    wd_0 <- setwd("tests/testthat/data/bumpr.test")
  }
  
  as.list(read.dcf("DESCRIPTION")[1,])$Version
  resetPackageVersion(vsn = "0.0.40", desc_fields = list(Date = "2014-01-01"))
  
  setwd(wd_0)
  
})
