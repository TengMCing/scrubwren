test_that("py_convert_on/off toggle conversion for Python modules", {
  skip_if_no_python()

  np <- reticulate::import("numpy", convert = FALSE)

  # Turn conversion on: results should not be Python objects
  py_convert_on(np)
  arr_on <- np$array(c(1, 2, 3))
  expect_false(reticulate::is_py_object(arr_on))

  # Turn conversion off: results should be Python objects
  py_convert_off(np)
  arr_off <- np$array(c(1, 2, 3))
  expect_true(reticulate::is_py_object(arr_off))
})
