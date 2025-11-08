test_that("py_for loops over Python objects with unpacking", {
  skip_if_no_python()

  pairs <- reticulate::tuple(list(list(1, "a"), list(2, "b")), convert = TRUE)
  out <- character()
  py_for(c(i, j) ~ pairs, {
    out <- c(out, paste0(i, "-", j))
  })
  expect_identical(out, c("1-a", "2-b"))
})

test_that("py_for supports next and break like examples", {
  skip_if_no_python()
  seen <- integer()
  py_for(x ~ reticulate::r_to_py(list(1, 2, 3)), {
    if (reticulate::py_to_r(x) == 2) next  # skip printing 2
    if (reticulate::py_to_r(x) == 3) break # exit before printing 3
    seen <- c(seen, reticulate::py_to_r(x))
  })
  expect_identical(seen, 1)
})
