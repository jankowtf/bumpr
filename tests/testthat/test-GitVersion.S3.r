context("GitVersion.S3_A")
test_that("GitVersion.S3", {

  expect_is(GitVersion.S3(), "GitVersion.S3")
  expect_is(GitVersion.S3(TRUE), "GitVersion.S3")
  expect_is(GitVersion.S3(version = "0.1.1"), "GitVersion.S3")
  
  }
)

