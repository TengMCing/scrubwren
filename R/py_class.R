#' Create a Python class
#' 
#' This function works like [reticulate::PyClass()], but the `defs` argument 
#' can now be provided as individual arguments (`...`) instead of a list.
#' 
#' @param classname Character. Name of the class.
#' @param ... Functions, attributes, or other definitions for the class.
#' @param inherit List. A list of Python class objects representing the 
#' parent classes.
#' 
#' @examples
#' 
#' \dontrun{
#' Employee <- py_class("Employee", 
#'                      `__init__` = function(self, name, id) {
#'                        self$name <- name
#'                        self$id <- id
#'                        return(invisible(NULL))
#'                      },
#'                      get_email = function(self) {
#'                        paste0(self$name, "_", self$id, "@company.com")
#'                      })
#' Mike <- Employee("Mike", "1234")
#' Mike$get_email()
#' }
#' 
#' 
#' @export
py_class <- function(classname, ..., inherit = NULL) {
  expr <- substitute(reticulate::PyClass(classname = classname, 
                                         defs = list(...), 
                                         inherit = inherit))
  eval.parent(expr)
}



