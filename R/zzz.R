.scrubwren_state <- new.env()

py_builtins_bindings <- function(x) {
  if (missing(x)) {
    # Delay the import of builtins to give the user the opportunity to 
    # declare which python interpreter to use.
    if (is.null(.scrubwren_state$py_builtins)) {
      
      potential_py <- reticulate::py_discover_config()
      
      if (is.null(potential_py)) {
        cli::cli_alert_danger("No Python interpreter could be detected! Please install Python and set the interpreter using `reticulate::use_python()`.")
      } else {
        cli::cli_alert_info("Importing `py_builtins` from {.field Python {reticulate::py_config()$version}} at {.file {reticulate::py_config()$python}}.")
        cli::cli_alert_info("You can re-import `py_builtins` by calling `reimport_py_builtins()` after setting the interpreter with `reticulate::use_python()`.")
        .scrubwren_state$py_builtins <- reticulate::import_builtins(convert = FALSE) 
      }
    }
  }
  
  # The binding is locked, so we can not assign value to it.
  
  return(.scrubwren_state$py_builtins)
}

#' @export
reimport_py_builtins <- function(convert = FALSE) {
  .scrubwren_state$py_builtins <- reticulate::import_builtins(convert = convert) 
}

#' Python's built-in functions.
#' 
#' These built-in functions are imported when the package loads. 
#' By default, `reticulate::py_discover_config()` is used to determine the Python version. 
#' To use a different Python interpreter, first set it with `reticulate::use_python()`, 
#' then re-import the built-in functions by calling `reimport_py_builtins()`.
#' 
#' @usage py_builtins
#' @name py_builtins
#' @export
NULL

.onLoad <- function(libname, pkgname) {
  makeActiveBinding("py_builtins",
                    py_builtins_bindings,
                    topenv())
}