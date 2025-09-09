#' Toggle automatic Python-to-R conversion
#'
#' These functions control whether objects from Python are automatically
#' converted to their R equivalents when accessed through `reticulate`.
#'
#' - `py_convert_on()` enables automatic conversion for a given Python object.
#' - `py_convert_off()` disables automatic conversion for a given Python object.
#'
#' The `obj` must be a Python object proxy (typically a module, class instance,
#' or similar R environment created by `reticulate`).  
#'
#' @param obj Environment. A Python object proxy as returned by `reticulate`. Must be an R
#' environment representing a Python object.  
#'
#' @return Invisibly returns `env` with its `convert` flag updated.  
#'
#' @examples
#' \dontrun{
#' # Assume `np` is a Python module (NumPy)
#' np <- reticulate::import("numpy", convert = FALSE)
#'
#' # Turn conversion on
#' py_convert_on(np)
#' np$array(c(1,2,3)) |> class()  # returns R array
#'
#' # Turn conversion off
#' py_convert_off(np)
#' np$array(c(1,2,3)) |> class()  # returns Python object
#' }
#'
#' @name py_convert_toggle
NULL

#' @rdname py_convert_toggle
#' @export
py_convert_on <- function(obj) {
  if (!reticulate::is_py_object(obj)) cli::cli_abort("Argument `obj` is not a Python object!")
  assign("convert", TRUE, obj)
  return(invisible(obj))
}

#' @rdname py_convert_toggle
#' @export
py_convert_off <- function(obj) {
  if (!reticulate::is_py_object(obj)) cli::cli_abort("Argument `obj` is not a Python object!")
  assign("convert", FALSE, obj)
  return(invisible(obj))
}
