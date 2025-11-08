skip_if_no_python <- function() {
  testthat::skip_on_cran()
  # Try to initialize Python quietly; ignore errors
  try(py_init(quiet = TRUE), silent = TRUE)
  if (!reticulate::py_available()) testthat::skip("Python not available for reticulate")
}
