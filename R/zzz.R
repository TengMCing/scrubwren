.scrubwren_state <- new.env()
.scrubwren_state$py_builtins <- NULL

py_builtins_bindings <- function(x) {

  # Delay the import of builtins to give user the opportunity to 
  # declare which python interpreter to use.
  if (is.null(.scrubwren_state$py_builtins)) {
    
    msg_option <- getOption("scrubwren.show_py_builtins_message", default = TRUE)
    
    if (reticulate::py_available()) {
      if (msg_option) cli::cli_alert_info("Importing `py_builtins` from {.field Python {reticulate::py_config()$version}} at {.file {reticulate::py_config()$python}}.")
      .scrubwren_state$py_builtins <- reticulate::import_builtins(convert = FALSE) 
    } else {
      if (msg_option) cli::cli_alert_danger("Cannot import `py_builtins` because Python is not ready! You can force initialization of Python with `reticulate::py_config().`")
    }
  }
    
  # The binding is locked, so we can not assign value to it.
  
  return(.scrubwren_state$py_builtins)
}


#' Python's built-in functions
#' 
#' These built-in functions are imported when `py_builtins` is first evaluated 
#' and Python is already initialized. 
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