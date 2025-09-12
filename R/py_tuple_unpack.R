#' Unpack a Python tuple (or sequence) into R variables
#'
#' This function unpacks elements of a Python tuple (or any sequence-like
#' object) into R variables, similar to tuple unpacking in Python
#' (`a, b = (1, 2)`).  
#'
#' If `vars` is a vector of names created with `c(...)`, each element of
#' `value` is assigned to the corresponding variable in the calling
#' environment. Nested unpacking is supported recursively.
#'
#' @param vars A symbol or a call like `c(a, b, c)` representing the
#'   variables to assign into.
#' @param value A Python object (tuple, list, etc.) or an R vector/list to
#'   unpack.
#' @param envir Environment. Environment in which to assign the unpacked values. 
#' Defaults to the calling environment.
#' @param quote_vars Boolean. If `TRUE`, `vars` is captured unevaluated
#'   (recommended when writing `c(a, b, c)` directly).
#'
#' @return Invisibly returns `NULL`. Called for side effects.
#'
#' @details
#' If the number of elements in `value` exceeds the number of variables
#' provided, a warning is issued but unpacking proceeds for the available
#' variables.
#'
#' @examples
#' \dontrun{
#' # Simple unpacking
#' py_tuple_unpack(c(a, b), list(1, 2))
#' a  # 1
#' b  # 2
#'
#' # With nested unpacking
#' py_tuple_unpack(c(a, c(b, d)), list(1, list(2, 3)))
#' a  # 1
#' b  # 2
#' d  # 3
#'
#' # Works with Python tuples/lists
#' tup <- reticulate::tuple(list(10, list(20, 30)))
#' py_tuple_unpack(c(x, c(y, z)), tup)
#' x  # 10
#' y  # 20
#' z  # 30
#' }
#'
#' @export
py_tuple_unpack <- function(vars, value, envir = parent.frame(), quote_vars = TRUE) {
  if (quote_vars) {
    vars <- substitute(vars)
  }
  
  if (!is.call(vars) || vars[[1]] != as.symbol("c")) {
    .state$last_tuple_unpack_value <- value
    expr <- call("<-", vars, quote(get_state()$last_tuple_unpack_value))
    eval(expr, envir = envir)
    return(invisible(NULL))
  }
  
  if (reticulate::is_py_object(value)) {
    if (reticulate::py_len(value) > length(vars) - 1) 
      cli::cli_warn("Unpacked values remain: the variable structure {.var {deparse(vars)}} is shorter than expected [{reticulate::py_len(value)}]!")
  } else {
    if (length(value) > length(vars) - 1) 
      cli::cli_warn("Unpacked values remain: the variable structure {.var {deparse(vars)}} is shorter than expected [{length(value)}]!")
  }
  
  for (i in 2:length(vars)) {
    if (reticulate::is_py_object(value)) {
      py_tuple_unpack(vars[[i]], value[[i - 2]], envir, FALSE)
    } else {
      py_tuple_unpack(vars[[i]], value[[i - 1]], envir, FALSE)
    }
  }
}
