py_builtins <- NULL
.scrubwren_state <- new.env()

.onLoad <- function(libname, pkgname) {
  py_builtins <<- reticulate::import_builtins(convert = FALSE)
}