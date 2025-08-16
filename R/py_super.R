#' @export
py_super <- function() {
  eval.parent(quote(py_builtins$super(`__class__`, self)))
}

#' @export
py_super_init <- function(...) {
  eval.parent(substitute(py_builtins$super(`__class__`, self)$`__init__`(...)))
}