context("bumpGitVersion-A")
test_that("bumpGitVersion", {

  skip("interactive only")
  
  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/bumpr.test")
  } else {
    wd_0 <- setwd("tests/testthat/data/bumpr.test")
  }
  
  ## DEBUG //
  if (FALSE) {
    .ns = .ns <- bumpr::GitVersion.S3()
    project = devtools::as.package(".")$package
    temp_credentials = FALSE
    
    what = .ns
    ## From and to //
    if (!file.exists("DESCRIPTION")) {
      stop("Not a valid R package project. Consult 'devtools' package")
    }    
    from <- unname(read.dcf("DESCRIPTION", field = "Version")[1,1])
    if (is.na(from)) {
      stop("Invalid version in DESCRIPTION")
    }
    to <- as.character(what$version)
    
    taken = character()
    sys_state = bumpr::SystemState.S3()
    desc_fields = list("Date" = NULL)
  }
  
  bumpGitVersion()
  
  on.exit(setwd(wd_0))
  
})
