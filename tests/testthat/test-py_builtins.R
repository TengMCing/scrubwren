test_that("py_builtins exposes Python built-ins", {
  skip_if_no_python()

  # Access Python's built-in `len`
  lst <- reticulate::r_to_py(list(1, 2, 3))
  expect_equal(reticulate::py_to_r(py_builtins$len(lst)), 3L)

  # Use Python's `isinstance`
  expect_true(reticulate::py_to_r(py_builtins$isinstance(lst, py_builtins$list)))
})
