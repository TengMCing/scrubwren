test_that("py_is_iterable and py_is_iterator reflect Python protocols", {
  skip_if_no_python()
  np <- reticulate::import("numpy", convert = FALSE)

  # A Python list is iterable but not an iterator
  lst <- reticulate::r_to_py(list(1, 2, 3))
  expect_true(py_is_iterable(lst))
  expect_false(py_is_iterator(lst))

  # An iterator is both iterable and an iterator
  it <- py_builtins$iter(lst)
  expect_true(py_is_iterable(it))
  expect_true(py_is_iterator(it))
})
