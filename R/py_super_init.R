#' Call Python's `super().__init__()` from R
#' 
#' This function mimics Python's built-in `super().__init__()` method, 
#' calling the initializer of the superclass for the current object.
#' It is equivalent to ``super()$`__init__`(...)``.
#'
#' @param ... Arguments to pass to the superclass initializer.
#' @return No return; called for side effect.
#' @examples
#' \dontrun{
#' Employee <- py_class("Employee",
#'                      `__init__` = function(self, name, id) {
#'                        self$name <- name
#'                        self$id <- id
#'                        return(py_builtins$None)
#'                      },
#'                      get_email = function(self) {
#'                        paste0(self$name, "_", self$id, "@company.com")
#'                      })
#' 
#' Salary <- py_class("Salary", inherit = Employee,
#'                    `__init__` = function(self, name, id, salary) {
#'                      py_super_init(name, id)
#'                      self$salary <- salary
#'                      return(py_builtins$None)
#'                    },
#'                    get_salary_summary = function(self) {
#'                      list(ID = self$id,
#'                           Name = self$name,
#'                           Email = self$get_email(),
#'                           Salary = self$salary)
#'                    })
#' 
#' mike_salary <- Salary("Mike", "1234", 1000)
#' mike_salary$get_salary_summary()
#' mike_salary$get_email()
#' }
#' 
#' @export
py_super_init <- function(...) {
  eval.parent(substitute(super()$`__init__`(...)))
}
