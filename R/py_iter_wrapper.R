#' Wrap a Python iterable with a custom function
#'
#' This function wraps a Python iterable or iterator so that each
#' element is transformed by a specified function when iterated over. The returned
#' object behaves like a Python iterator, applying the transformation lazily on each element.
#'
#' @param obj A Python iterable or iterator object. If it is
#'   iterable but not an iterator, it will be converted to an iterator internally.
#' @param func An R or Python function to apply to each element of the iterable.
#' @param convert Boolean. Whether to automatically convert each value to R object.
#'
#' @return A Python iterator object whose elements are transformed by `func`. Each
#'   call to `next()` returns `func(element)`.
#'
#' @details
#' - If `obj` is iterable but not already an iterator, it is converted to a Python iterator
#'   using `reticulate::as_iterator()`.
#' - The returned object implements the Python iterator protocol (`__iter__` and `__len__`).
#' - The function `func` is applied lazily: the transformation occurs only when elements are
#'   retrieved from the iterator.
#'
#' @examples
#' \dontrun{
#' my_list <- py_builtins$list(1:5)
#' wrapped_iter <- py_iterable_wrapper(my_list, function(x) x^2) |>
#'   reticulate::as_iterator()
#' reticulate::iter_next(wrapped_iter)  # Returns 1
#' reticulate::iter_next(wrapped_iter)  # Returns 4
#' }
#'
#' @export
py_iterable_wrapper <- function(obj, func, convert = FALSE) {
  if (!py_is_iterable(obj)) cli::cli_abort("Arugument `obj` is not iterable!")
  
  IterWrapper <- reticulate::py_run_string("
class IterWrapper: 
    def __init__(self, obj, func): 
        self.obj = obj
        self.func = func 
      
    def __len__(self): 
        return len(self.obj) 
      
    def __iter__(self): 
        for x in self.obj: 
            yield (self.func(x))",
                      convert = convert,
                      local = TRUE)$IterWrapper
  
  return(IterWrapper(obj, func))                         
}