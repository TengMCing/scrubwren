test_that("py_iterable_wrapper applies function lazily and iterates correctly", {
  skip_if_no_python()
  
  # A simple Python list [1,2,3]
  py_list <- py_builtins$list(list(1L, 2L, 3L))
  
  # Wrap it with a square function
  wrapper <- py_iterable_wrapper(py_list, function(x) x * x)
  
  # It should be a Python iterator
  expect_true(!py_is_iterator(wrapper))
  
  # Length should match original list
  expect_equal(reticulate::py_len(wrapper), 3L)
  
  # Pull values lazily
  wrapper_iter <- reticulate::as_iterator(wrapper)
  out1 <- reticulate::iter_next(wrapper_iter)
  out2 <- reticulate::iter_next(wrapper_iter)
  out3 <- reticulate::iter_next(wrapper_iter)
  
  expect_equal(reticulate::py_to_r(out1), 1L)
  expect_equal(reticulate::py_to_r(out2), 4L)
  expect_equal(reticulate::py_to_r(out3), 9L)
  
  # After exhausting, next should be NULL
  expect_null(reticulate::iter_next(wrapper_iter))
})

test_that("py_iterable_wrapper works when obj is already an iterator", {
  skip_if_no_python()
  
  py_list <- py_builtins$list(list(10L, 20L))
  
  # Convert to iterator manually
  it <- reticulate::as_iterator(py_list)
  
  wrapper <- py_iterable_wrapper(it, function(x) x + 1)
  
  expect_true(!py_is_iterator(wrapper))
  
  wrapper_iter <- reticulate::as_iterator(wrapper)
  expect_equal(reticulate::iter_next(wrapper_iter) |> reticulate::py_to_r(), 11)
  expect_equal(reticulate::iter_next(wrapper_iter) |> reticulate::py_to_r(), 21)
  expect_null(reticulate::iter_next(wrapper_iter) |> reticulate::py_to_r())
})
