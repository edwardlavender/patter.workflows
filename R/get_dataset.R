#' @title Estimate coordinates: get `algorithm` inputs
#' @description These functions get `algorithm` inputs (i.e., parameters or datasets).
#' @param sim,datasets Arguments inherited from `constructor` in [`proj.lapply::workflow()`].
#' @param parameter A `character` that defines the name of a column in `sim`.
#' @details These functions simply extract required inputs from `sim` or `datasets` with appropriate checks.
#' 
#' `get_dataset_*()` functions extract a `dataset` from `datasets`. 
#' 
#' The following functions are designed to support `estimate_coord` constructors:
#'    * [`get_dataset_map()`] extracts `datasets$map`
#'    * [`get_dataset_detections()`] extracts `datasets$detections_by_unit[[sim$unit_id]]`
#'    * [`get_dataset_moorings()`] extracts `datasets$moorings`
#'    * [`get_dataset_t.layer()`] extracts `datasets$t.layer`
#'    
#' The following functions are designed to support `estimate_map` constructors:
#' 
#'    * [`get_dataset_coord()`] gets coordinates from file, via `qs::qread(sim$file_coord)`, or memory, via `datasets$coord[[sim$index]]$output`. A `datasets$coordinates()` function can be supplied to extract the coordinates, in the format required by the `estimate_map` function, for estimation. 
#'    
#' `get_parameter()` functions extract a parameter value via `sim[[parameter]]`;
#' 
#' These functions are used in `constructor` functions. 
#' 
#' They are available to users for use in custom `constructor` functions.
#' @author Edward Lavender
#' @name get_input
NULL

#' @rdname get_input
#' @export

get_dataset_map <- function(sim, datasets) {
  # TO DO
  # * Handle datasets on file with rlang::has_name(sim, "file_map")
  check_names(datasets, "map")
  datasets$map
}

#' @rdname get_input
#' @export

get_dataset_detections <- function(sim, datasets) {
  check_names(sim, "unit_id")
  check_names(datasets, "detections_by_unit")
  detections <- datasets$detections_by_unit[[sim$unit_id]]
  if (is.null(detections)) {
    abort("`datasets$detections_by_unit[[sim$unit_id]]` for sim$unit_id = {sim$unit_id} is NULL!", 
          .environ = environment())
  }
  detections
}

#' @rdname get_input
#' @export

get_dataset_moorings <- function(sim, datasets) {
  check_names(datasets, "moorings")
  datasets$moorings
}

#' @rdname get_input
#' @export

get_dataset_t.layer <- function(sim, datasets) {
  check_names(datasets, "t.layer")
  datasets$t.layer
}

#' @rdname get_input
#' @export

get_dataset_coord <- function(sim, datasets) {
  # Read dataset from file, if available
  if (rlang::has_name(sim, "file_coord")) {
    rlang::check_installed("qs")
    data <- qs::qread(sim$file_coord)
  } else {
    # Read dataset from memory 
    check_names(sim, "index")
    check_names(datasets, "coord")
    data <- datasets$coord[[sim$index]]$output
  }
  if (rlang::has_name(datasets, "coordinates")) {
    data <- datasets$coordinates(data)
  }
  data
}

#' @rdname get_input
#' @export

get_parameter <- function(sim, parameter) {
  check_names(sim, parameter)
  sim[[parameter]]
}
