#' Create a Python class
#' 
#' This function works like [reticulate::PyClass()], but the `defs` argument 
#' can now be provided as individual arguments (`...`) instead of a list.
#' 
#' The original [reticulate::PyClass()] does not allow disabling the automatic 
#' conversion of Python objects to R. This function modifies its function body 
#' to enable that functionality.
#' 
#' @param classname Character. Name of the class.
#' @param ... Functions, attributes, or other definitions for the class.
#' @param inherit List. A list of Python class objects representing the 
#' parent classes.
#' @param convert Boolean. Whether to automatically convert Python objects to R.
#' @return A Python class object.
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
#' Mike$get_email() |> class()
#' }
#' 
#' 
#' @export
py_class <- function(classname, ..., inherit = NULL, convert = FALSE) {
  
  new_pyclass <- reticulate::PyClass
  txt <- deparse(body(new_pyclass))
  
  # directly inject the literal value of `convert` (TRUE/FALSE) into the text
  txt <- gsub("assign\\(\"convert\", TRUE",
              sprintf("assign(\"convert\", %s", if (convert) "TRUE" else "FALSE"),
              txt)
  
  txt <- gsub("builtins <- import_builtins(convert = TRUE)",
              sprintf("builtins <- import_builtins(convert = %s)",
                      if (convert) "TRUE" else "FALSE"),
              txt, fixed = TRUE)
  
  body(new_pyclass) <- parse(text = txt)
  
  new_pyclass(classname = classname, 
              defs = list(...), 
              inherit = inherit)
}



