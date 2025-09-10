
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
  if (!reticulate::is_py_object(obj)) cli::cli_abort("Arugument `obj` is not a Python object!")
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
  if (!reticulate::is_py_object(obj)) cli::cli_abort("Arugument `obj` is not a Python object!")
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
#' @param env The environment in which to run the loop and evaluate the body.
#'   Defaults to the calling environment.
#' @param enable_loop_control Boolean. if `TRUE`, the loop monitors user calls
#'   to `break` or `next` at the top level of the `py_for()` body. These statements
#'   then control the loop as expected. If `FALSE`, any `break` or `next`
#'   at the top level of the body will trigger an error.  
#'   Note that `break` and `next` inside nested loops within the body are not
#'   governed by this setting; their behavior depends on the configuration of
#'   the nested loops themselves.
#'
#' @return Invisibly returns `NULL`. Called for side effects.
#'
#' @details
#' - If `iterable` implements only `__iter__` but not `__next__`, it is
#'   automatically converted into an iterator.  
#' - Loop variables support tuple unpacking via [py_tuple_unpack()].  
#' - If `enable_loop_control = TRUE`, the loop tracks whether the user 
#'   calls `break` or `next` inside the loop:
#'   * `break` exits the loop early, skipping any remaining iterations.
#'   * `next` skips to the next iteration without stopping the loop entirely.
#'   * If neither is called, the loop proceeds normally.  
#' 
#' ## Performance warnings
#' Looping over Python objects in R can be **inefficient**. In each iteration, 
#' `reticulate` must pass handles between R and Python, often performing 
#' implicit or explicit object conversions and copies.  
#' If the `body` of your loop is lightweight and you need to iterate over a 
#' large Python object, consider defining a Python function via 
#' [reticulate::py_run_string()] or [py_builtins]`$exec()` and calling it directly.  
#' You can also use `r.var` (where `var` is any R variable name) to access or 
#' assign R objects directly from Python, which may help avoid unnecessary data transfer.  
#' Native Python tools are **significantly faster** in such cases!
#' 
#' For example, instead of doing:
#' ```r
#' y <- py_builtins$int(0L)
#' py_for(x ~ reticulate::r_to_py(1:10000), y <- y + x^2)
#' ```
#' it is better to do:
#' ```r
#' function_def <- "
#' def cumsum_square(n):
#'     return sum([x ** 2 for x in range(1, n + 1)])
#' "
#' my_func <- reticulate::py_run_string(function_def, local = TRUE, convert = FALSE)
#' y <- my_func$cumsum_square(100000L)
#' ```
#' 
#' Note that we need to set `local = TRUE`, so that the returned dictionary
#' is not within the main module. The main module created by `reticulate` 
#' automatically converts Python objects to R objects, unless we disable this 
#' behavior for the entire module. Doing so, however, could interfere
#' with `reticulate`'s internals. Defining the function in a private dictionary
#' with `convert = FALSE` allows us to keep objects as native Python types, 
#' which is important when working with large integers that could otherwise 
#' overflow when converted back to R.
#'
#' @examples
#' \dontrun{
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
#' 
#' # Basic loop over a Python list with loop control
#' py_for(x ~ reticulate::r_to_py(list(1, 2, 3)), {
#'   if (reticulate::py_to_r(x) == 2) next  # skip printing 2
#'   if (reticulate::py_to_r(x) == 3) break # exit before printing 3
#'   print(x)
#' })
#' 
#' # Loop with loop control disabled
#' py_for(x ~ reticulate::r_to_py(list(1, 2, 3)), {
#' 
#'   # Local loop `break` is allowed
#'   for (i in 1:10) break  
#'   
#'   # Local `py_for` loop `break` is allowed
#'   py_for(i ~ reticulate::r_to_py(list(1, 2, 3)), break)
#'   
#'   # Using break or next here will trigger an error
#'   next 
#'   print(x)
#' }, enable_loop_control = FALSE)
#' 
#' # Loop with inner loop control disabled
#' py_for(x ~ reticulate::r_to_py(list(1, 2, 3)), {
#' 
#'   # Nested `py_for` with loop control disabled
#'   # Using `break` here will trigger an error
#'   py_for(i ~ reticulate::r_to_py(list(1, 2, 3)), 
#'          break, 
#'          enable_loop_control = FALSE)
#'          
#'   print(x)
#' })
#' 
#' }
#'
#' @export
py_for <- function(loop_spec, body, env = parent.frame(), enable_loop_control = TRUE) {
  if (!is.call(loop_spec) || loop_spec[[1]] != as.symbol("~"))
    cli::cli_abort("The loop specification needs to be a formula!")
  
  var_sym <- loop_spec[[2]]
  iter_sym <- loop_spec[[3]]
  
  iter <- eval(iter_sym, envir = env)
  
  # If `iter` only implements the __iter__ method but not the __next__ method,
  # we need to convert it to a iterator.
  if (!py_is_iterator(iter)) {
    iter <- reticulate::as_iterator(iter)  
  }
  
  while (TRUE) {
    item <- reticulate::iter_next(iter, completed = quote(StopIteration))
    if (identical(item, quote(StopIteration))) break
    py_tuple_unpack(var_sym, item, envir = env, quote_vars = FALSE)
    
    if (enable_loop_control) {
      body_expr <- bquote({
        
        # Track for loop control
        assign("loop_control_break", "null", envir = scrubwren::get_scrubwren_state())
        
        while (TRUE) {
          
          # "user break" means the body has not been evaluated, 
          # if this is the final state, it means the user `break` from the loop body.
          # "no break" means the body has been evaluated,
          # if this is the final state, it means the user may or may not call `next` but no `break`.
          if (scrubwren::get_scrubwren_state()$loop_control_break == "null") {
            assign("loop_control_break", "user break", envir = scrubwren::get_scrubwren_state())
          } else {
            assign("loop_control_break", "no break", envir = scrubwren::get_scrubwren_state())
            break 
          }
          
          .(substitute(body))
        }
      }) 
    } else {
      body_expr <- substitute(body)
    }
      
    eval(body_expr, envir = env)
    
    if (enable_loop_control && .scrubwren_state$loop_control_break == "user break") break
  }
}



# py_comprehension --------------------------------------------------------

#' Python-Style Comprehension in R
#'
#' Evaluate Python-like comprehensions in R. Supports multiple nested loops
#' with optional conditions, returning results in a chosen Python collection 
#' type (`list`, `tuple`, `set`, or `dict`) or as a regular R list. 
#' Conceptually, it works like [lapply()] for Python objects, but 
#' this function offers additional return types and more flexible 
#' control over iteration.
#' 
#' @details
#' 
#' ## Dictionary return type
#' When `format = `[py_builtins]`$dict()`, each evaluation of `body` must 
#' be a **key-value pair**. Valid pair formats include:
#' * A Python tuple of length 2
#' * A Python list of length 2
#' * A regular R list of length 2  
#' 
#' The first element is used as the key and the second as the value.  
#' This allows creating dictionaries in Python style directly from R 
#' comprehensions.
#' 
#' ## Side effects
#' Like [lapply()], this function evaluates `body` in a local scope, so 
#' assignments normally do not affect the caller environment. To produce side 
#' effects, use [<<-], [assign()] with a suitable environment, or modify an 
#' environment variable directly (see [environment()]).
#' 
#' ## Performance warnings
#' Looping over Python objects in R can be **inefficient**. In each iteration, 
#' `reticulate` must pass handles between R and Python, often performing 
#' implicit or explicit object conversions and copies.  
#' If the `body` of your loop is lightweight and you need to iterate over a 
#' large Python object, consider defining a Python function via 
#' [reticulate::py_run_string()] or [py_builtins]`$exec()` and calling it directly.  
#' You can also use `r.var` (where `var` is any R variable name) to access or 
#' assign R objects directly from Python, which may help avoid unnecessary data transfer.  
#' Native Python tools are **significantly faster** in such cases!
#' 
#' For example, instead of doing:
#' ```r
#' y <- py_comprehension(x ~ reticulate::r_to_py(1:100000), x^2)
#' ```
#' it is better to do:
#' ```r
#' function_def <- "
#' def list_square(n):
#'     return [x ** 2 for x in range(1, n + 1)]
#' "
#' my_func <- reticulate::py_run_string(function_def, local = TRUE, convert = FALSE)
#' y <- my_func$list_square(100000L)
#' ```
#' 
#' Note that we need to set `local = TRUE`, so that the returned dictionary
#' is not within the main module. The main module created by `reticulate` 
#' automatically converts Python objects to R objects, unless we disable this 
#' behavior for the entire module. Doing so, however, could interfere
#' with `reticulate`'s internals. Defining the function in a private dictionary
#' with `convert = FALSE` allows us to keep objects as native Python types, 
#' which is important when working with large integers that could otherwise 
#' overflow when converted back to R.
#'
#' @param loop_spec_list Formula/List. A single formula or a 
#' list of formulas specifying the loops. Each formula must have the form 
#' `var ~ iterable` or `var ~ iterable | condition`, where `var` is the 
#' loop variable and `iterable` is a Python iterable.  
#' When multiple formulas are provided, each additional formula defines a 
#' deeper loop, with the last formula representing the innermost loop.  
#' The `condition` will be wrapped using [py_builtins]`$bool()`, so it can be 
#' either an R boolean or a Python value compatible with Python’s 
#' [truth-testing procedure](https://docs.python.org/3/library/stdtypes.html#truth).
#' @param body Expression. An R expression to evaluate inside the innermost loop. 
#' Its result is collected into the comprehension output.
#' @param env Environment. The parent environment for evaluation; defaults to 
#' the caller's environment. The comprehension is evaluated 
#' in a **new environment** created on top of this provided `env`, so that 
#' variables created or modified inside `body` do not affect the outer 
#' environment unless global modification is explicitly used.
#' @param format A Python collection type or a regular R list to store the results. 
#'   Defaults to a Python list ([py_builtins]`$list()`).
#'
#' @return A Python collection or R list containing the results of the comprehension.
#'
#' @examples
#' \dontrun{
#' # Simple Python-style comprehension
#' py_comprehension(
#'   i ~ reticulate::r_to_py(1:3),
#'   i^2
#' )
#'
#' # Nested loops with conditions
#' test <- reticulate::r_to_py(list(list(1, 2, 3), list(1, 2), list(1)))
#' py_comprehension(
#'   c(
#'     x ~ test | py_builtins$len(x) > 1,
#'     z ~ x | z > 1
#'   ),
#'   {
#'     a <- z + 1
#'     a
#'   }
#' )
#'
#' # Return results as a regular R list
#' py_comprehension(
#'   i ~ reticulate::r_to_py(1:3),
#'   i^2,
#'   format = list()
#' )
#'
#' # Return results as a Python tuple
#' py_comprehension(
#'   i ~ reticulate::r_to_py(1:3),
#'   i^2,
#'   format = py_builtins$tuple()
#' )
#'
#' # Return results as a Python set
#' py_comprehension(
#'   i ~ reticulate::r_to_py(c(1, 2, 2, 3)),
#'   i^2,
#'   format = py_builtins$set()
#' )
#' 
#' # Return results as a Python dict
#' py_comprehension(
#'   i ~ reticulate::r_to_py(1:3),
#'   list(i, i^2),
#'   format = py_builtins$dict()
#' )
#' }
#'
#' @export
py_comprehension <- function(loop_spec_list, body, env = parent.frame(), format = py_builtins$list()) {
  
  # Convert `loop_spect_list` into a list if only a single formula is provided
  if (!is.list(loop_spec_list)) {
    loop_spec_list <- list(loop_spec_list)
  }
  
  # Setup comprehension container
  comprehension_init(format)
  .scrubwren_state$comprehension_add <- comprehension_add
  
  # The final expression is a nested `py_for()` loop
  accumulated_body <- substitute({scrubwren::get_scrubwren_state()$comprehension_add(body)})
  
  # Innermost loop needs to be added to the `accumulated_body` first
  for (loop_spec in rev(loop_spec_list)) {
    if (!is.call(loop_spec) || loop_spec[[1]] != as.symbol("~"))
      cli::cli_abort("The loop specification needs to be a formula!")
    
    var_sym <- loop_spec[[2]]
    iter_sym <- loop_spec[[3]]
    iter_condition <- TRUE
    
    # Extract the iteration condition
    if (is.call(iter_sym) && iter_sym[[1]] == as.symbol("|")) {
      iter_condition <- iter_sym[[3]]
      iter_sym <- iter_sym[[2]]
    }
    
    new_loop_spec <- call("~", var_sym, iter_sym)
    
    accumulated_body <- bquote(py_for(.(new_loop_spec), {
      
      # Accept both R boolean and Python object as the iteration condition
      if (reticulate::py_to_r(py_builtins$bool(.(iter_condition)))) .(accumulated_body)  
    }, enable_loop_control = FALSE))
  }
  
  # Record the accumulated for debug purpose
  .scrubwren_state$comprehension_accumulated_body <- accumulated_body
  
  # Evaluate the nested for loop in a new environment to avoid unnecessary side-effect
  eval(accumulated_body, envir = new.env(parent = env))
  
  # Finalize the result
  comprehension_finalize()
  
  return(.scrubwren_state$comprehension_result)
}

comprehension_init <- function(format) {
  
  # Use the most efficient and convenient container
  if (reticulate::is_py_object(format)) {
    if (reticulate::py_to_r(py_builtins$isinstance(format, py_builtins$list))) {
      .scrubwren_state$comprehension_format <- "py_list"
      .scrubwren_state$comprehension_result <- py_builtins$list()
    } else if (reticulate::py_to_r(py_builtins$isinstance(format, py_builtins$tuple))) {
      .scrubwren_state$comprehension_format <- "py_tuple"
      .scrubwren_state$comprehension_result <- py_builtins$list()
    } else if (reticulate::py_to_r(py_builtins$isinstance(format, py_builtins$set))) {
      .scrubwren_state$comprehension_format <- "py_set"
      .scrubwren_state$comprehension_result <- py_builtins$set()
    } else if (reticulate::py_to_r(py_builtins$isinstance(format, py_builtins$dict))) {
      .scrubwren_state$comprehension_format <- "py_dict"
      .scrubwren_state$comprehension_result <- py_builtins$dict()
    }
  } else if (is.list(format)) {
    
    # Pre-allocate space to avoid frequent memory allocation 
    .scrubwren_state$comprehension_format <- "r_list"
    .scrubwren_state$comprehension_result <- vector("list", 1000L)
    .scrubwren_state$comprehension_result_len <- 0L
    .scrubwren_state$comprehension_result_capacity <- 1000L
  } else {
    cli::cli_abort("Argument`format` must be a Python list, tuple, set, dict, or an R list!")
  }
}

comprehension_add <- function(x) {
  
  format <- .scrubwren_state$comprehension_format
  
  if (format == "r_list") {
    
    capacity <- .scrubwren_state$comprehension_result_capacity 
    .scrubwren_state$comprehension_result_len <- .scrubwren_state$comprehension_result_len + 1L
    
    # Check the container capacity
    if (capacity < .scrubwren_state$comprehension_result_len) {
      
      # Exponential growth at the beginning, then switch to linear growth
      if (capacity < 1e5) {
        capacity <- capacity * 2L
      } else {
        capacity <- capacity + 1e5L
      }
      
      # Set and record new capacity, NULL will be padded
      length(.scrubwren_state$comprehension_result) <- capacity
      .scrubwren_state$comprehension_result_capacity <- capacity
    }
    
    # Record the object
    .scrubwren_state$comprehension_result[[.scrubwren_state$comprehension_result_len]] <- x
    
  } else if (format %in% c("py_list", "py_tuple")) {
    .scrubwren_state$comprehension_result$append(x)
  } else if (format == "py_set") {
    .scrubwren_state$comprehension_result$add(x)
  } else if (format == "py_dict") {
    
    # Handle R object and Python object differently
    # So user can provide R list or Python tuple/list as 
    if (reticulate::is_py_object(x)) {
      .scrubwren_state$comprehension_result[x[0]] <- x[1]
    } else {
      .scrubwren_state$comprehension_result[x[[1]]] <- x[[2]]
    }
  }
}

comprehension_finalize <- function() {
  format <- .scrubwren_state$comprehension_format
  if (format == "r_list") {
    length(.scrubwren_state$comprehension_result) <- .scrubwren_state$comprehension_result_len
  } else {
    if (format == "py_tuple") {
      .scrubwren_state$comprehension_result <- py_builtins$tuple(.scrubwren_state$comprehension_result)
    }
  }
}