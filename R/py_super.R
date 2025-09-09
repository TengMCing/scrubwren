#' Call Python's `super()` from R
#'
#' These functions provide access to Python's built-in `super()` mechanism
#' from within R, making it easier to work with Python class inheritance
#' when defining classes via [reticulate::PyClass()] or [py_class()].
#'
#' - `py_super()` returns a proxy to the superclass of the current object,
#'   equivalent to Python's `super()`. It can be used in two forms:
#'     - `py_super()` (no arguments): behaves like `super()` with implicit
#'       resolution provided by `reticulate`.
#'     - `py_super(type, object_or_type)` (two arguments): advanced usage
#'       equivalent to `super(type, object_or_type)` in Python, e.g.
#'       `py_super(A, self)` or `py_super(__class__, self)`.
#' - `py_super_init()` directly calls the superclass initializer,
#'   equivalent to `super().__init__(...)`. Unlike `py_super()`, it always
#'   refers to the next class in the MRO and does not accept type arguments.
#'
#' While `reticulate` internally injects a zero-argument `super()` reference 
#' into class methods, this re-export makes the functionality explicit and 
#' convenient for user code.
#'
#' @param type Optional. The class or `__class__` symbol, for advanced
#'   two-argument `super()` usage. Ignored if missing.
#' @param object_or_type Optional. Typically `self`, for advanced
#'   two-argument `super()` usage. Ignored if missing.
#' @param ... Arguments to pass to the superclass initializer (for
#'   `py_super_init()` only).
#'
#' @return 
#' - `py_super()` returns a Python object proxy to the superclass.  
#' - `py_super_init()` returns `NULL` invisibly 
#'   (or [reticulate::py_none()] if `convert = FALSE`), called for side effects.  
#'
#' @examples
#' \dontrun{
#' Employee <- py_class(
#'   "Employee",
#'   `__init__` = function(self, name, id) {
#'     self$name <- name
#'     self$id <- id
#'     return(py_builtins$None)
#'   }
#' )
#'
#' Salary <- py_class(
#'   "Salary", inherit = Employee,
#'   `__init__` = function(self, name, id, salary) {
#'     # Option A: use implicit super
#'     py_super()$`__init__`(name, id)
#'
#'     # Option B: use advanced two-argument super
#'     # py_super(Salary, self)$`__init__`(name, id)
#'
#'     # Option C: use py_super_init()
#'     # py_super_init(name, id)
#'
#'     self$salary <- salary
#'     return(py_builtins$None)
#'   }
#' )
#' }
#'
#' @name py_super
NULL



#' @rdname py_super
#' @export
py_super <- function(type, object_or_type) {
  if (missing(type) && missing(object_or_type)) {
    eval.parent(quote(super()))
  } else {
    eval.parent(bquote(py_builtins$super(.(type), .(object_or_type))))
  }
}

#' @rdname py_super
#' @export
py_super_init <- function(...) {
  eval.parent(substitute(super()$`__init__`(...)))
}
