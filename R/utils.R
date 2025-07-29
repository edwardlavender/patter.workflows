# For ggmaps, extract graphical parameters from layers
get_geom_parameter <- function(geom_list, args, default) {
  parameter <- sapply(args, function(arg) {
    if (rlang::has_name(geom_list, arg)) {
      geom_list$arg
    }
  })[1]
  if (is.null(parameter)) {
    parameter <- default
  }
  parameter
}