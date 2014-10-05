context("RPackageVersion.S3_A")
test_that("RPackageVersion.S3", {

  expect_is(RPackageVersion.S3(), "RPackageVersion.S3")
  expect_is(RPackageVersion.S3(TRUE), "RPackageVersion.S3")
  expect_is(RPackageVersion.S3(version = "0.1.1"), "RPackageVersion.S3")
  
})

