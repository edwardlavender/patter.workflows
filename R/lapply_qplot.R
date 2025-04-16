#' @title Quick plots
#' @description These functions quickly plot maps, reading map layers from memory or from file. 
#' @param .iteration The iteration [`data.table`] (see `constructor` functions). If `.iteration` contains a `file_coord` or `file_ud` column, map layers (coordinates or utilisation distributions) are read from file; otherwise, they are extracted from `.datasets`. 
#' @param .datasets A named `list` of datasets, with one element for each `iteration`, required if `iteration$file_output` is not specified:
#' * For [`lapply_qplot_coord()`], `.datasets` should contain `coord` and (optional) `coordinates` elements (see [`get_dataset_coord()`]). The extracted coordinates should comprise a [`data.table`] of `x` and `y` coordinates for this function. 
#' * For [`lapply_qplot_map()`], `.datasets` should contain a `ud` element  (see [`get_dataset_ud()`]).
#' @param .n_plot An `integer` that defines the number of plots. 
#' @param .map For [`lapply_qplot_coord()`], `.map` is a spatial layer, plotted via [`terra::plot()`], onto which coordinates are added. 
#' @param .n_coord For [`lapply_qplot_coord()`], `.n_coord` is an `integer` that defines the maximum number of coordinate pairs to plot. 
#' @details
#' This functions produce quick multi-panel maps for with `.n_plot` panels.
#' * If `.iteration$file_output` is specified, map layers are read into memory;
#' * Otherwise, map layers are extracted from `.datasets`;
#' * [`lapply_qplot_coord()`] plots background maps with coordinates ontop;
#' * [`lapply_qplot_ud()`] plots utilisation distributions;
#' * Map titles are defined by the row index in `.iteration`;
#' 
#' @author Edward Lavender
#' @name lapply_qplot
NULL

#' @rdname lapply_qplot
#' @export

lapply_qplot_coord <- function(.iteration,
                               .datasets = list(), 
                               .n_plot = 4L, 
                               .n_coord = 1e4L,
                               .map, ...) {
  
  # Define iteration index, if required
  iteration   <- copy(.iteration)
  if (!rlang::has_name(iteration, "index")) {
    index <- NULL
    iteration[, index := 1:.N]
  }
  
  # (optional) Focus on iteration rows with files that exist
  if (rlang::has_name(iteration, "file_coord")) {
    iteration  <- iteration[file.exists(iteration$file_coord), ]
    .n_plot <- min(c(.n_plot, nrow(iteration)))
    if (.n_plot == 0) {
      warn("No `iteration$file_coord` files exist.")
      return(nothing())
    }
  }
  iteration <- iteration[1:.n_plot, ]
  
  # Set up plotting window 
  pp <- par(mfrow = patter:::par_mf(.n_plot))
  on.exit(par(pp, no.readonly = TRUE), add = TRUE)
  
  # Make plots
  lapply(split(iteration, seq_len(nrow(iteration))), function(.sim) {
      coord <- get_dataset_coord(.sim, .datasets)
      if (nrow(coord) > .n_coord) {
        coord <- coord[sample.int(.n_coord, replace = FALSE), ]
      }
      terra::plot(.map, main = .sim$index, ...)
      points(coord$x, coord$y, pch = ".", col = "red")
      nothing()
  })
  
  nothing()

}

#' @rdname lapply_qplot
#' @export

lapply_qplot_ud <- function(.iteration,
                             .datasets = list(), 
                             .n_plot = 4L, ...) {
  
  # Define iteration index, if required
  iteration <- copy(.iteration)
  if (!rlang::has_name(iteration, "index")) {
    index <- NULL
    iteration[, index := 1:.N]
  }
  
  # (optional) Focus on iteration rows with files that exist
  if (rlang::has_name(iteration, "file_ud")) {
    iteration  <- iteration[file.exists(iteration$file_ud), ]
    .n_plot <- min(c(.n_plot, nrow(iteration)))
    if (.n_plot == 0) {
      warn("No `iteration$file_ud` files exist.")
      return(nothing())
    }
  }
  iteration <- iteration[1:.n_plot, ]
  
  # Set up plotting window 
  pp <- par(mfrow = patter:::par_mf(.n_plot))
  on.exit(par(pp, no.readonly = TRUE), add = TRUE)
  
  # Make plots
  lapply(split(iteration, seq_len(nrow(iteration))), function(.sim) {
    ud <- get_dataset_ud(.sim, .datasets)
    terra::plot(ud, main = .sim$index, ...)
    nothing()
  })
  
  nothing()
  
}