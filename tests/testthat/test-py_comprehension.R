test_that("py_comprehension basic and formats follow examples", {
  skip_if_no_python()

  # Simple comprehension: squares of 1:3
  res_list_py <- py_comprehension(
    i ~ reticulate::r_to_py(1:3),
    i^2
  )
  expect_true(reticulate::is_py_object(res_list_py))
  expect_identical(reticulate::py_to_r(res_list_py), c((1:3)^2))

  # Nested loops with conditions (small toy)
  test <- reticulate::r_to_py(list(list(1, 2, 3), list(1, 2), list(1)))
  res_cond <- py_comprehension(
    c(
      x ~ test | py_builtins$len(x) > 1,
      z ~ x | z > 1
    ),
    {
      a <- z + 1
      a
    }
  )
  # x sequences with length > 1: [1,2,3], [1,2]; z > 1 yields [2,3,2] + 1
  expect_identical(reticulate::py_to_r(res_cond), c(3, 4, 3))

  # Nested comprehension
  res_nested <- py_comprehension(
    i ~ reticulate::r_to_py(1:3),
    py_comprehension(j ~ reticulate::r_to_py(1:3) | j >= i, j)
  )
  expect_true(reticulate::is_py_object(res_nested))
  expect_identical(reticulate::py_to_r(res_nested), list(1:3, 2:3, 3L))

  # Return results as a regular R list
  res_r_list <- py_comprehension(
    i ~ reticulate::r_to_py(1:3),
    i^2,
    format = list()
  )
  expect_type(res_r_list, "list")
  expect_identical(reticulate::py_to_r(reticulate::r_to_py(res_r_list)), 
                   c((1:3)^2))

  # Return results as a Python tuple
  res_tuple <- py_comprehension(
    i ~ reticulate::r_to_py(1:3),
    i^2,
    format = py_builtins$tuple()
  )
  expect_true(inherits(res_tuple, "python.builtin.tuple"))
  expect_identical(reticulate::py_to_r(res_tuple), as.list((1:3)^2))

  # Return results as a Python set (order not guaranteed)
  res_set <- py_comprehension(
    i ~ reticulate::r_to_py(c(1, 2, 2, 3)),
    i^2,
    format = py_builtins$set()
  )
  expect_true(inherits(res_set, "python.builtin.set"))
  expect_identical(reticulate::py_to_r(py_builtins$list(res_set)), 
                   c(1, 4, 9))

  # Return results as a Python dict
  res_dict <- py_comprehension(
    i ~ reticulate::r_to_py(1:3),
    list(i, i^2),
    format = py_builtins$dict()
  )
  expect_true(inherits(res_dict, "python.builtin.dict"))
  # Check entries
  expect_identical(reticulate::py_to_r(res_dict[[1]]), 1)
  expect_identical(reticulate::py_to_r(res_dict[[2]]), 4)
  expect_identical(reticulate::py_to_r(res_dict[[3]]), 9)
})
