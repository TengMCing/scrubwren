#' @export
py_is_iterable <- function(obj) {
  reticulate::py_config()
  if (!reticulate::is_py_object(obj)) cli::cli_abort("{substitute(obj)} is not a Python object!")
  result <- tryCatch(py_builtins$iter(obj),
                     python.builtin.TypeError = function(e) quote(TypeError))
  if (identical(result, quote(TypeError))) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

#' @export
py_is_iterator <- function(obj) {
  if (!reticulate::is_py_object(obj)) cli::cli_abort("{substitute(obj)} is not a Python object!")
  collections <- reticulate::import(collections, convert = FALSE)
  result <- py_buiitins$isinstance(obj, collections$abc$Iterator) 
  return(reticulate::py_to_r(result))
}

# @param envir where to run the for loop.
#' @export
py_for <- function(loop_spec, body, envir = parent.frame()) {
  if (!is.call(loop_spec) || loop_spec[[1]] != as.symbol("~"))
    cli::cli_abort("The loop specification needs to be a formula!")
  
  var_sym <- loop_spec[[2]]
  iter_sym <- loop_spec[[3]]
  iter <- eval(iter_sym, envir = envir)
  
  # If `iter` only implements the __iter__ method but not the __next__ method,
  # we need to convert it to a iterator.
  if (!py_is_iterator(iter)) {
    iter <- reticulate::as_iterator(iter)  
  }
  
  while (TRUE) {
    item <- reticulate::iter_next(iter, completed = quote(StopIteration))
    if (identical(item, quote(StopIteration))) break
    py_tuple_unpack(var_sym, item, envir = envir, quote_vars = FALSE)
    eval(substitute(body), envir = envir)
  }
}
