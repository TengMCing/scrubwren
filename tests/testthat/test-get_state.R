test_that("get_state returns internal environment", {
  # This test is independent of Python but still skip on CRAN
  testthat::skip_on_cran()
  st <- get_state()
  expect_true(is.environment(st))
  expect_true("py_builtins" %in% names(st) || is.null(names(st)))
})
