#' @export
py_class <- function(classname, ..., inherit = NULL) {
  expr <- bquote(reticulate::PyClass(classname = .(substitute(classname)), 
                                     defs = .(substitute(list(...))), 
                                     inherit = .(substitute(inherit))))
  eval.parent(expr)
}
