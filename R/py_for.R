
# py_iterable_check -------------------------------------------------------

#' Test whether a Python object is iterable or an iterator
#'
#' These functions check the iteration protocol of a Python object
#' when accessed from R through `reticulate`.
#'
#' - `py_is_iterable()` returns `TRUE` if the object can return an iterator
#'   via Python's `iter()` function, otherwise `FALSE`.
#' - `py_is_iterator()` returns `TRUE` if the object itself is an iterator,
#'   i.e. an instance of `collections.abc.Iterator`.
#'
#' @param obj A Python object proxy.
#'
#' @return A Boolean scalar (`TRUE` or `FALSE`).
#'
#' @seealso Python's [iterator protocol](https://docs.python.org/3/library/stdtypes.html#typeiter)
#'
#' @examples
#' \dontrun{
#' np <- reticulate::import("numpy", convert = FALSE)
#'
#' # A Python list is iterable but not an iterator
#' lst <- reticulate::r_to_py(list(1, 2, 3))
#' py_is_iterable(lst)   # TRUE
#' py_is_iterator(lst)   # FALSE
#'
#' # An iterator (e.g., from iter()) is both iterable and an iterator
#' it <- py_builtins$iter(lst)
#' py_is_iterable(it)    # TRUE
#' py_is_iterator(it)    # TRUE
#' }
#'
#' @name py_iterable_check
NULL

#' @rdname py_iterable_check
#' @export
py_is_iterable <- function(obj) {
  reticulate::py_init(quiet = TRUE)
  if (!reticulate::is_py_object(obj)) cli::cli_abort("{substitute(obj)} is not a Python object!")
  result <- tryCatch(py_builtins$iter(obj),
                     python.builtin.TypeError = function(e) quote(TypeError))
  if (identical(result, quote(TypeError))) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

#' @rdname py_iterable_check
#' @export
py_is_iterator <- function(obj) {
  if (!reticulate::is_py_object(obj)) cli::cli_abort("{substitute(obj)} is not a Python object!")
  collections <- reticulate::import("collections", convert = FALSE)
  result <- py_builtins$isinstance(obj, collections$abc$Iterator) 
  return(reticulate::py_to_r(result))
}


# py_for ------------------------------------------------------------------

#' Python-style `for` loops in R
#'
#' Executes a Python-style `for` loop in R, iterating over a Python iterable or
#' iterator. This provides a convenient syntax for looping with destructuring
#' (tuple unpacking) similar to Python's `for` statement.
#'
#' @param loop_spec A two-sided formula of the form `vars ~ iterable`, where
#'   `vars` specifies one or more loop variables (e.g. `x` or `c(i, j)`), and
#'   `iterable` is a Python iterable or iterator.
#' @param body An R expression to evaluate on each iteration.
#' @param envir The environment in which to run the loop and evaluate the body.
#'   Defaults to the calling environment.
#'
#' @return Invisibly returns `NULL`. Called for side effects.
#'
#' @details
#' - If `iterable` implements only `__iter__` but not `__next__`, it is
#'   automatically converted into an iterator.  
#' - Loop variables support tuple unpacking via [py_tuple_unpack()].  
#' - The loop tracks whether the user calls `break` or `next` inside the loop:
#'   * `break` exits the loop early, skipping any remaining iterations.
#'   * `next` skips to the next iteration without stopping the loop entirely.
#'   * If neither is called, the loop proceeds normally.  
#'
#' @examples
#' \dontrun{
#' 
#' # Basic loop over a Python list with loop control
#' py_for(x ~ reticulate::r_to_py(list(1, 2, 3)), {
#'   if (reticulate::py_to_r(x) == 2) next  # skip printing 2
#'   if (reticulate::py_to_r(x) == 3) break # exit before printing 3
#'   print(x)
#' })
#' 
#' # Loop over a Python list
#' py_for(x ~ reticulate::r_to_py(list(1, 2, 3)), {
#'   print(x)
#' })
#'
#' # Loop with tuple unpacking
#' pairs <- reticulate::tuple(list(list(1, "a"), list(2, "b")), convert = TRUE)
#' py_for(c(i, j) ~ pairs, {
#'   cat("i =", i, " j =", j, "\n")
#' })
#'
#' # Loop over a NumPy array
#' np <- reticulate::import("numpy", convert = FALSE)
#' arr <- np$array(c(10, 20, 30))
#' py_for(val ~ arr, {
#'   print(val)
#' })
#' }
#'
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
    
    body_expr <- bquote({
      
      # Track for loop control
      assign("for_loop_control", "null", envir = scrubwren::get_scrubwren_state())
      
      while (TRUE) {
        
        # "user break" means the body has not been evaluated, 
        # if this is the final state, it means the user `break` from the loop body.
        # "no break" means the body has been evaluated,
        # if this is the final state, it means the user may or may not call `next` but no `break`.
        if (scrubwren::get_scrubwren_state()$for_loop_control == "null") {
          assign("for_loop_control", "user break", envir = scrubwren::get_scrubwren_state())
        } else {
          assign("for_loop_control", "no break", envir = scrubwren::get_scrubwren_state())
          break 
        }

        .(substitute(body))
      }
    }) 
    
    eval(body_expr, envir = envir)
    
    if (.scrubwren_state$for_loop_control == "user break") break
  }
}

