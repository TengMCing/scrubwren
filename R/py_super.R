#' Call Python's `super()` from R
#'
#' These functions provide access to Python's built-in `super()` mechanism
#' from within R, making it easier to work with Python class inheritance
#' when defining classes via [reticulate::PyClass()] or [py_class()].
#'
#' - `py_super()` returns a proxy to the superclass of the current object,
#'   equivalent to Python's `super()`.
#' - `py_super_init()` directly calls the superclass initializer,
#'   equivalent to `super().__init__(...)`.
#'
#' While `reticulate` internally injects a `super()` reference into class
#' methods, this re-export makes the functionality explicit and convenient
#' for user code.
#'
#' @param ... Arguments to pass to the superclass initializer (for
#'   `py_super_init()` only).
#'
#' @return 
#' - `py_super()` returns a Python object proxy to the superclass.  
#' - `py_super_init()` returns `NULL` invisibly 
#' (or [reticulate::py_none()] if `convert = FALSE`), called for side effects.  
#'
#' @examples
#' \dontrun{
#' Employee <- py_class(
#'   "Employee",
#'   `__init__` = function(self, name, id) {
#'     self$name <- name
#'     self$id <- id
#'     return(py_builtins$None)
#'   },
#'   get_email = function(self) {
#'     paste0(self$name, "_", self$id, "@company.com")
#'   }
#' )
#'
#' Salary <- py_class(
#'   "Salary", inherit = Employee,
#'   `__init__` = function(self, name, id, salary) {
#'     # Option A: use py_super()
#'     py_super()$`__init__`(name, id)
#'
#'     # Option B: use py_super_init()
#'     # py_super_init(name, id)
#'
#'     self$salary <- salary
#'     return(py_builtins$None)
#'   },
#'   get_summary = function(self) {
#'     list(ID = self$id,
#'          Name = self$name,
#'          Email = self$get_email(),
#'          Salary = self$salary)
#'   }
#' )
#'
#' mike <- Salary("Mike", "1234", 1000)
#' mike$get_summary()
#' }
#'
#' @name py_super
NULL


#' @rdname py_super
#' @export
py_super <- function() {
  eval.parent(quote(super()))
}

#' @rdname py_super
#' @export
py_super_init <- function(...) {
  eval.parent(substitute(super()$`__init__`(...)))
}
