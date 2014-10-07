context("SystemState.S3_A")
test_that("SystemState.S3", {

  expect_is(SystemState.S3(), "SystemState.S3")
  expect_is(SystemState.S3(TRUE), "SystemState.S3")
  expect_is(res <- SystemState.S3(ask_authentication = FALSE), "SystemState.S3")
  expect_false(res$ask_authentication)
  
})

