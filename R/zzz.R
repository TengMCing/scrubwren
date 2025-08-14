py_builtins <- NULL

.onLoad <- function(libname, pkgname) {
  py_builtins <<- reticulate::import_builtins(convert = FALSE)
}