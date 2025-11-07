#' Wrap a Python iterable with a custom function
#'
#' This function wraps a Python iterable or iterator so that each
#' element is transformed by a specified function when iterated over. The returned
#' object behaves like a Python iterator, applying the transformation lazily on each element.
#'
#' @param obj A Python iterable or iterator object. If it is
#'   iterable but not an iterator, it will be converted to an iterator internally.
#' @param func An R or Python function to apply to each element of the iterable.
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
#' wrapped_iter <- py_iter_wrapper(my_list, function(x) x^2)
#' reticulate::iter_next(wrapped_iter)  # Returns 1
#' reticulate::iter_next(wrapped_iter)  # Returns 4
#' }
#'
#' @export
py_iter_wrapper <- function(obj, func) {
  if (py_is_iterable(obj) && !py_is_iterator(obj)) {
    obj_iter <- reticulate::as_iterator(obj)
  }
  
  IterWrapper <- reticulate::py_run_string("
class IterWrapper: 
    def __init__(self, obj_iter, func): 
        self.obj_iter = obj_iter 
        self.func = func 
      
    def __len__(self): 
        return len(self.obj_iter) 
      
    def __iter__(self): 
        for x in self.obj_iter: 
            yield (self.func(x))",
                      convert = FALSE,
                      local = TRUE)$IterWrapper
  
  return(reticulate::as_iterator(IterWrapper(obj_iter, func)))                         
}