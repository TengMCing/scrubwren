test_that("py_tuple_unpack unpacks simple and nested structures", {
  skip_if_no_python()

  # Simple unpacking with R list
  a <- b <- NULL
  py_tuple_unpack(c(a, b), list(1, 2))
  expect_identical(a, 1)
  expect_identical(b, 2)

  # Nested unpacking
  a <- b <- d <- NULL
  py_tuple_unpack(c(a, c(b, d)), list(1, list(2, 3)))
  expect_identical(a, 1)
  expect_identical(b, 2)
  expect_identical(d, 3)

  # Works with Python tuples/lists
  tup <- reticulate::tuple(list(10, list(20, 30)))
  x <- y <- z <- NULL
  py_tuple_unpack(c(x, c(y, z)), tup)
  expect_identical(reticulate::py_to_r(x), 10)
  expect_identical(reticulate::py_to_r(y), 20)
  expect_identical(reticulate::py_to_r(z), 30)
})
