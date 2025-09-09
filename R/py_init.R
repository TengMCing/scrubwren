#' Explicitly initialize a Python session
#'
#' This function explicitly initializes a Python session in the current R
#' session. A single R session can only be bound to one Python session.
#' Once initialized, the Python configuration is fixed for the duration
#' of the R session.
#'
#' If Python has already been initialized, a warning is issued and the
#' existing configuration is returned. Otherwise, the specified Python
#' executable is used to start a new session.
#'
#' @param python_path Character. Path to the Python
#'   executable to use. Defaults to the `python` element returned by
#'   [reticulate::py_discover_config()].
#' @param quiet Boolean. Whether to suppress message.
#'
#' @return An invisible Python configuration object, as returned by
#'   [reticulate::py_config()].
#'
#' @examples
#' \dontrun{
#' # Initialize Python using the default discovery
#' py_init()
#' }
#'
#' @export
py_init <- function(python_path = reticulate::py_discover_config()$python,
                    quiet = FALSE) {
  if (reticulate::py_available()) {
    config <- reticulate::py_config()
    if (!quiet) cli::cli_alert_warning("{.field Python {config$version}} from {.file {python_path}} is already initialized!")
  } else {
    reticulate::use_python(python_path)
    config <- reticulate::py_config()
    if (!quiet) cli::cli_alert_info("Initialized {.field Python {config$version}} from {.file {python_path}}.")
  }
  return(invisible(config))
}
