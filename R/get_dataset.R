#' @title Estimate coordinates: get `algorithm` inputs
#' @description These functions get `algorithm` inputs (i.e., parameters or datasets).
#' @param sim,datasets Arguments inherited from `constructor` in [`proj.lapply::workflow()`].
#' @param parameter A `character` that defines the name of a column in `sim`.
#' @details These functions simply extract required inputs from `sim` or `datasets` with appropriate checks:
#' * `get_dataset_*()` functions extract a `dataset` **by position** via `datasets[[sim$unit_id]]`;
#' * `get_parameter()` functions extract a parameter value via `sim[[parameter]]`;
#' 
#' These functions are used in [`constructor`] functions. 
#' 
#' They are available to users for use in custom [`constructor`] functions.
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

get_parameter <- function(sim, parameter) {
  check_names(sim, parameter)
  sim[[parameter]]
}
