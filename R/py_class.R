#' @export
py_class <- function(classname, ..., inherit = NULL) {
  expr <- substitute(reticulate::PyClass(classname = classname, 
                                         defs = list(...), 
                                         inherit = inherit))
  eval.parent(expr)
}
