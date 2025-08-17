#' @export
py_convert_on <- function(env) {
  if (!reticulate::is_py_object(env)) cli::cli_abort("{substitute(env)} is not an Python object!")
  assign("convert", TRUE, env)
}

#' @export
py_convert_off <- function(env) {
  if (!reticulate::is_py_object(env)) cli::cli_abort("{substitute(env)} is not an Python object!")
  assign("convert", FALSE, env)
}