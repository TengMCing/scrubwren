
#' @param envir where to assign the value to vars.
#' @export
py_tuple_unpack <- function(vars, value, envir = parent.frame(), quote_vars = TRUE) {
  if (quote_vars) {
    vars <- substitute(vars)
  }
  
  if (!is.call(vars) || vars[[1]] != as.symbol("c")) {
    .scrubwren_state$last_tuple_unpack_value <- value
    expr <- bquote(.(vars) <- .scrubwren_state$last_tuple_unpack_value)
    eval(expr, envir = envir)
    return(invisible(NULL))
  }
  
  if (reticulate::is_py_object(value)) {
    if (reticulate::py_to_r(py_builtins$len(value)) > length(vars) - 1) 
      cli::cli_warn("Unpacked values remain: the variable structure {.var {deparse(vars)}} is shorter than expected [{reticulate::py_to_r(py_builtins$len(value))}]!")
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
