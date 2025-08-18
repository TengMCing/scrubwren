
# .scrubwren_state --------------------------------------------------------

.scrubwren_state <- new.env()
.scrubwren_state$py_builtins <- NULL

#' Access the internal `scrubwren` state
#'
#' Returns the hidden internal state object used by the `scrubwren` package
#' for managing temporary values (e.g., during tuple unpacking or other helper
#' operations).
#'
#' @return An environment containing internal package state.
#'
#' @details
#' This function exposes the `.scrubwren_state` environment.  
#' It is primarily intended for debugging or advanced use, and the
#' structure of the state object is not guaranteed to remain stable
#' across package versions.
#'
#' @examples
#' \dontrun{
#' st <- get_scrubwren_state()
#' ls(st)             # List objects in the internal state
#' st$last_tuple_unpack_value  # Access the last unpacked tuple value
#' }
#'
#' @export
get_scrubwren_state <- function() {
  return(.scrubwren_state)
}


# py_builtins -------------------------------------------------------------

py_builtins_bindings <- function(x) {

  # Delay the import of builtins to give user the opportunity to 
  # declare which python interpreter to use.
  if (is.null(.scrubwren_state$py_builtins)) {
    
    msg_option <- getOption("scrubwren.show_py_builtins_message", default = TRUE)
    
    if (reticulate::py_available()) {
      if (msg_option && interactive()) cli::cli_alert_info("Importing `py_builtins` from {.field Python {reticulate::py_config()$version}} at {.file {reticulate::py_config()$python}}.")
      .scrubwren_state$py_builtins <- reticulate::import_builtins(convert = FALSE) 
    } else {
      if (msg_option && interactive()) cli::cli_alert_danger("Cannot import `py_builtins` because Python is not ready! You can force initialization of Python with `reticulate::py_config().`")
    }
  }
    
  # The binding is locked, so we can not assign value to it.
  
  return(.scrubwren_state$py_builtins)
}


#' Access Python's built-in functions
#'
#' `py_builtins` provides access to Python's standard built-in functions
#' (e.g., `len`, `iter`, `isinstance`, etc.).
#'
#' @details
#' The object is a **locked active binding** and cannot be assigned to.
#' It is lazily imported on first access; if Python is not ready, the import fails.
#' In interactive sessions, an informational message is printed (can be suppressed
#' via `options(scrubwren.show_py_builtins_message = FALSE)`).  
#' All built-ins are Python objects; convert to R objects with [reticulate::py_to_r()].

#'
#' @examples
#' \dontrun{
#' # Suppress informational messages
#' options(scrubwren.show_py_builtins_message = FALSE)
#' 
#' # Access Python's built-in `len`
#' lst <- reticulate::r_to_py(list(1, 2, 3))
#' py_builtins$len(lst)  # returns 3
#'
#' # Use Python's `isinstance`
#' py_builtins$isinstance(lst, py_builtins$list)
#'
#' }
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