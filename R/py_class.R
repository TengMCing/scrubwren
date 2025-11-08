# Adapted from reticulate v1.44.0 code
inject_super <- function(fun) {
  e <- new.env(parent = environment(fun))
  
  e$super <- function() {
    self <- get("self", envir = parent.frame(), inherits = FALSE)
    class_ <- get("__class__", envir = e, inherits = FALSE)
    py_builtins$super(class_, self)
  }
  
  environment(fun) <- e
  
  fun
}

#' Create a Python class (adapted from reticulate v1.44.0)
#'
#' This function creates a Python class object, similarly to
#' [reticulate::PyClass()], but with two key differences:
#'
#' 1. The class definitions (`defs`) can be supplied directly as named
#'    arguments via `...`, rather than as a list.
#' 2. The implementation is adapted from the internal class-construction
#'    mechanism used in **reticulate v1.44.0**, with modifications that allow
#'    you to control whether Python objects are automatically converted to R
#'    (`convert = TRUE/FALSE`).
#'
#' Each function supplied in `...` is wrapped to support Python's `super()`
#' and to optionally disable automatic conversion of Python arguments,
#' closely following the behaviour of reticulate’s internals.
#'
#' @param classname Character. Name of the class to define.
#' @param ... Functions, attributes, or other class definitions.
#' @param inherit A Python class object or list of Python class objects
#'   representing base classes. If omitted, the class will not inherit from
#'   any Python base classes.
#' @param convert Logical. Whether to automatically convert Python objects
#'   to R when calling methods on the class. Defaults to `FALSE` to allow
#'   low-level interop.
#'
#' @return A Python class object, constructed using Python's `type()`.
#'
#' @examples
#'
#' \dontrun{
#' Employee <- py_class(
#'   "Employee",
#'   `__init__` = function(self, name, id) {
#'     self$name <- name
#'     self$id <- id
#'     invisible(NULL)
#'   },
#'   get_email = function(self) {
#'     paste0(self$name, "_", self$id, "@company.com")
#'   }
#' )
#'
#' Mike <- Employee("Mike", "1234")
#' Mike$get_email()
#' Mike$get_email() |> class()
#' }
#'
#' @export
py_class <- function(classname, ..., inherit = NULL, convert = FALSE) {
  
  defs <- list(...)
  
  if (reticulate::is_py_object(inherit)) inherit <- list(inherit)
  
  if (length(inherit) == 0) {
    bases <- reticulate::tuple()
  } else if (is.list(inherit)) {
    bases <- do.call(reticulate::tuple, inherit)
  } else if (is.character(inherit)) {
    bases <- do.call(tuple, as.list(inherit))
  } else {
    cli::cli_abort("Unexpected `inherit` argument!")
  }
  
  defs <- lapply(defs, function(x) {
    if (!is.function(x)) return(x)
    
    f <- inject_super(x)
    
    x <- function(...) {
      args <- list(...)
      assign("convert", convert, envir = as.environment(args[[1]]))
      if (convert) {
        return(do.call(f, append(args[1], lapply(args[-1], py_to_r))))
      } else {
        return(do.call(f, args))
      }
    }
    
    attr(x, "__env__") <- environment(f)
    return(x)
  })
  
  if (convert) {
    builtins <- reticulate::import_builtins(convert = TRUE)
    type <- builtins$type(classname, bases, do.call(reticulate::dict, defs))
  } else {
    type <- py_builtins$type(classname, bases, do.call(reticulate::dict, defs))  
  }
  
  lapply(defs, function(x) {
    envir <- attr(x, "__env__")
    if (!is.environment(envir))
      return()
    
    envir$`__class__` <- type
  })
  
  return(type)
}
