test_that("py_init initializes Python or reports existing", {
  skip_if_no_python()
  expect_silent(py_init(quiet = TRUE))
  expect_true(reticulate::py_available())
})
