# bw_image_to_df <- function(obj, format = "HW", channel_name = "L") {
#   if (!reticulate::is_py_object(obj)) cli::cli_abort("Argument `obj` is not a Python object!")
#   
#   np <- reticulate::import("numpy", convert = FALSE)
#   if (!reticulate::py_to_r(py_builtins$isinstance(obj, np$ndarray))) {
#     obj <- np$array(obj)
#   }
#   
#   if (reticulate::py_to_r(obj$ndim) != 2) cli::cli_abort("Argument `obj` must be a 2-dimensional object, but it has {obj$ndim} dimensions!")
#   
#   if (!toupper(format) %in% c("HW", "WH")) cli::cli_abort('Argument `format` must be "HW" or "WH"!')
#   
#   if (toupper(format == "WH")) obj <- np$transpose(obj)
#   
#   reticulate::py_to_r(obj)
# }
# 
# xx <- reticulate::np_array(matrix(1:10, ncol = 2))
# # H: 5
# # W: 2
# xx$shape
# 
# 
# bw_image_to_df(xx)
# 
# 
# py_image_to_df <- function(obj, format = "N?HWC", channel_names = c("R", "G", "B")) {
#   
# }
# 
# 
# py_preview_image <- function(obj, format = "N?HWC") {
#   if (reticulate::is_py_object(ob)) cli::cli_abort("format ")
#   
#   np <- reticulate::import("numpy", convert = FALSE)
#   if (!reticulate::py_to_r(py_builtins$isinstance(obj, np$ndarray))) {
#     obj <- np$array(obj)
#   }
# 
# }