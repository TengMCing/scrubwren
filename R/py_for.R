
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
#'
#' @return Invisibly returns `NULL`. Called for side effects.
#'
#' @details
#' - If `iterable` implements only `__iter__` but not `__next__`, it is
#'   automatically converted into an iterator.  
#' - Loop variables support tuple unpacking via [py_tuple_unpack()].  
#' - The loop tracks whether the user 
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
#' # Nested loop
#' py_for(x ~ reticulate::r_to_py(list(list(1, 2, 3), list(4, 5, 6))), {
#'   py_for(y ~ x, {
#'     print(y)
#'   })
#'   print("inner list finished")
#' })
#' 
#' 
#' }
#'
#' @export
py_for <- function(loop_spec, body, env = parent.frame()) {
  if (!is.call(loop_spec) || loop_spec[[1]] != as.symbol("~"))
    cli::cli_abort("The loop specification needs to be a formula!")
  
  var_sym <- loop_spec[[2]]
  iter_sym <- loop_spec[[3]]
    
  body_expr <- quote({
    
    while(TRUE) {

      if (!scrubwren::get_state()$for_next_item()) {
        break
      }
      
      scrubwren::get_state()$for_unpack(var_sym)

      body
    }
  })
  
  body_expr[[2]][[3]][[3]][[2]] <- var_sym
  body_expr[[2]][[3]][[4]] <- substitute(body)
  
  .state$for_body <- body_expr
  .state$for_next_item <- for_next_item
  .state$for_unpack <- for_unpack

  # Use a loop number to track inner loop
  if (is.null(.state$loop)) .state$loop <- 0L
  
  # Build a stack
  if (is.null(.state$loop_iter)) .state$loop_iter <- list()
  if (is.null(.state$loop_item)) .state$loop_item <- list()
  if (is.null(.state$loop_env)) .state$loop_env <- list()
  .state$loop <- .state$loop + 1L
  
  # Get the iterator
  iter <- eval(iter_sym, envir = env)
  
  # If `iter` only implements the __iter__ method but not the __next__ method,
  # we need to convert it to a iterator.
  # Record the iterator
  if (!py_is_iterator(iter)) {
    .state$loop_iter[[.state$loop]] <- reticulate::as_iterator(iter)  
  } else {
    .state$loop_iter[[.state$loop]] <- iter
  }
  
  # Record the iterator environment
  .state$loop_env[[.state$loop]] <- env
  
  eval(body_expr, envir = env)
  
  # Pop the stack
  .state$loop_iter[[.state$loop]] <- NULL
  .state$loop_item[[.state$loop]] <- NULL
  .state$loop_env[[.state$loop]] <- NULL
  .state$loop <- .state$loop - 1L  
  
  
  return(invisible(NULL))
}

# Get next item and check if the loop should continue
for_next_item <- function() {
  
  loop <- .state$loop
  
  .state$loop_item[[loop]] <- reticulate::iter_next(.state$loop_iter[[loop]], 
                                                    completed = quote(StopIteration))
  
  if (identical(.state$loop_item[[loop]], quote(StopIteration))) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

# Unpack for loop variables
for_unpack <- function(var_sym) {
  loop <- .state$loop
  py_tuple_unpack(substitute(var_sym), 
                  .state$loop_item[[loop]], 
                  envir = .state$loop_env[[loop]],
                  quote_vars = FALSE)
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
#' # Nested comprehension
#' py_comprehension(i ~ reticulate::r_to_py(1:5),
#'   py_comprehension(j ~ reticulate::r_to_py(1:5) | j >= i, j)
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
  
  # New a local environment
  eval_env <- new.env(parent = env)
  
  # Build a stack
  if (is.null(.state$comp)) .state$comp <- 0L
  .state$comp <- .state$comp + 1L
  
  # Setup comprehension container
  comp_init(format, substitute(body), eval_env)
  
  .state$comp_add <- comp_add
  
  # The final expression is a nested `py_for()` loop
  agg_body <- quote({scrubwren::get_state()$comp_add()})
  
  # Innermost loop needs to be added to the `agg_body` first
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
    
    agg_body <- bquote(py_for(.(new_loop_spec), {
      
      # Accept both R boolean and Python object as the iteration condition
      if (reticulate::py_to_r(py_builtins$bool(.(iter_condition)))) .(agg_body)  
    }))
  }
  
  # Record the accumulated for debug purpose
  .state$comp_agg_body <- agg_body
  
  # Evaluate the nested for loop in a new environment to avoid unnecessary side-effect
  eval(agg_body, envir = eval_env)
  
  # Finalize the result
  result <- comp_finalize()
  
  # Clean up the stack
  .state$comp <- .state$comp - 1L
  
  return(result)
}

comp_init <- function(format, body, eval_env) {
  
  number <- .state$comp
  if (number == 1) {
    .state$comp_format <- list()
    .state$comp_result <- list()
    .state$comp_result_len <- list()
    .state$comp_result_capacity <- list()
    .state$comp_func <- list()
  }
  
  # Define a function to call the body
  .state$comp_func[[number]] <- function() {}
  body(.state$comp_func[[number]]) <- body
  environment(.state$comp_func[[number]]) <- eval_env
  
  # Use the most efficient and convenient container
  if (reticulate::is_py_object(format)) {
    if (reticulate::py_to_r(py_builtins$isinstance(format, py_builtins$list))) {
      .state$comp_format[[number]] <- "py_list"
      .state$comp_result[[number]] <- py_builtins$list()
    } else if (reticulate::py_to_r(py_builtins$isinstance(format, py_builtins$tuple))) {
      .state$comp_format[[number]] <- "py_tuple"
      .state$comp_result[[number]] <- py_builtins$list()
    } else if (reticulate::py_to_r(py_builtins$isinstance(format, py_builtins$set))) {
      .state$comp_format[[number]] <- "py_set"
      .state$comp_result[[number]] <- py_builtins$set()
    } else if (reticulate::py_to_r(py_builtins$isinstance(format, py_builtins$dict))) {
      .state$comp_format[[number]] <- "py_dict"
      .state$comp_result[[number]] <- py_builtins$dict()
    }
  } else if (is.list(format)) {
    
    # Pre-allocate space to avoid frequent memory allocation 
    .state$comp_format[[number]] <- "r_list"
    .state$comp_result[[number]] <- vector("list", 1000L)
    .state$comp_result_len[[number]] <- 0L
    .state$comp_result_capacity[[number]] <- 1000L
  } else {
    cli::cli_abort("Argument`format` must be a Python list, tuple, set, dict, or an R list!")
  }
}

comp_add <- function() {
  
  
  number <- .state$comp
  format <- .state$comp_format[[number]]
  
  x <- .state$comp_func[[number]]()
  
  if (format == "r_list") {
    
    capacity <- .state$comp_result_capacity[[number]] 
    .state$comp_result_len[[number]] <- .state$comp_result_len[[number]] + 1L
    
    # Check the container capacity
    if (capacity < .state$comp_result_len[[number]]) {
      
      # Exponential growth at the beginning, then switch to linear growth
      if (capacity < 1e5) {
        capacity <- capacity * 2L
      } else {
        capacity <- capacity + 1e5L
      }
      
      # Set and record new capacity, NULL will be padded
      length(.state$comp_result[[number]]) <- capacity
      .state$comp_result_capacity[[number]] <- capacity
    }
    
    # Record the object
    .state$comp_result[[.state$comp_result_len[[number]]]][[number]] <- x
    
  } else if (format %in% c("py_list", "py_tuple")) {
    .state$comp_result[[number]]$append(x)
  } else if (format == "py_set") {
    .state$comp_result[[number]]$add(x)
  } else if (format == "py_dict") {
    
    # Handle R object and Python object differently
    # So user can provide R list or Python tuple/list
    if (reticulate::is_py_object(x)) {
      .state$comp_result[[number]][x[0]] <- x[1]
    } else {
      .state$comp_result[[number]][x[[1]]] <- x[[2]]
    }
  }
}

comp_finalize <- function() {
  
  number <- .state$comp
  format <- .state$comp_format[[number]]
  
  if (format == "r_list") {
    length(.state$comp_result[[number]]) <- .state$comp_result_len[[number]]
  } else {
    if (format == "py_tuple") {
      .state$comp_result[[number]] <- py_builtins$tuple(.state$comp_result[[number]])
    }
  }
  
  result <- .state$comp_result[[number]]
  
  .state$comp_format[[number]] <- NULL
  .state$comp_result[[number]] <- NULL
  .state$comp_result_len[[number]] <- NULL
  .state$comp_result_capacity[[number]] <- NULL
  .state$comp_func[[number]] <- NULL
  
  return(result)
}