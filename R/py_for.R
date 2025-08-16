#' @param envir where to run the for loop.
#' @export
py_for <- function(loop_spec, body, envir = parent.frame()) {
  if (!is.call(loop_spec) || loop_spec[[1]] != as.symbol("~"))
    cli::cli_abort("The loop specification needs to be a formula!")
  
  var_sym <- loop_spec[[2]]
  iter_sym <- loop_spec[[3]]
  iter <- eval(iter_sym, envir = envir)
  
  # If `iter` only implements the __iter__ method but not the __next__ method,
  # we need to convert it to a iterator.
  if (!reticulate::py_has_attr(iter, "__next__")) {
    iter <- py_builtins$iter(iter)  
  }
  
  while (TRUE) {
    item <- reticulate::iter_next(iter, completed = quote(StopIteration))
    if (identical(item, quote(StopIteration))) break
    py_tuple_unpack(var_sym, item, envir = envir, quote_vars = FALSE)
    eval(substitute(body), envir = envir)
  }
}
