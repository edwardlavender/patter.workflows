#' @title Estimate coordinates: constructors
#' @description `constructor` functions construct a named `list` of arguments for an algorithm implementation. 
#' @param sim A [`data.table`] row, inherited from [`lapply_estimate_coord()`].
#' @param map  A [`data.table`] row, inherited from [`lapply_estimate_coord()`].
#' @param datasets A [`data.table`] row, inherited from [`lapply_estimate_coord()`].
#' @param verbose A `logical` variable, , inherited from [`lapply_estimate_coord()`].
#' @details
#' Default `constructors` are provided for the COA, RSP and AC particle algorithms. 
#' 
#' For [`constructor_coa()`]:
#' * `sim` should include the following columns:
#'      - `unit_id` 
#'      - `delta_t`
#'      
#' * `datasets` should include the following elements:
#'      - `detections_by_unit`
#'      - `moorings`
#'
#' For [`constructor_rsp()`]:
#' * `sim` should include the following columns:
#'      - `unit_id`
#'      - `er.ad`
#'      
#' * `datasets` should include the following elements:
#'      - `detections_by_unit`
#'      - `moorings`
#'      - `t.layer`;
#' 
#' For particle algorithms, constructors have to be defined manually, following the same function signature. 
#' 
#' @return `constructor_*()` functions return a named `list` of arguments for an algorithm (e.g., [`algorithm_coa()`]).
#' @author Edward Lavender
#' @name constructor

#' @rdname constructor
#' @export 

constructor_coa <- function(sim, map, datasets, verbose) {
  # Define datasets
  detections <- get_dataset_detections(sim, datasets)
  moorings   <- get_dataset_moorings(sim, datasets)
  # Define parameters 
  delta_t    <- get_parameter(sim, "delta_t")
  # Collect arguments 
  list(.map          = map,
       .detections   = detections, 
       .moorings     = moorings, 
       .delta_t      = delta_t, 
       .plot_weights = FALSE)
}

#' @rdname constructor
#' @export

constructor_rsp <- function(sim, map, datasets, verbose) {
  # Define datasets
  detections <- get_dataset_detections(sim, datasets) 
  moorings   <- get_dataset_moorings(sim, datasets)
  act        <- as_actel(map        = map, 
                         detections = detections, 
                         moorings   = moorings)
  t.layer    <- get_dataset_t.layer(sim, datasets)
  # Define parameters
  # * At the time of writing, only er.ad can be specified dynamically
  # * Other parameters take default values
  er.ad <- get_parameter(sim, "er.ad")
  # Collect arguments
  args <- list(input   = act,
               t.layer = t.layer,
               coord.x = "Longitude",
               coord.y = "Latitude",
               er.ad   = er.ad)
}


#' @title Estimate coordinates: get `constructor` inputs
#' @description These internal functions get `constructor` inputs (i.e., parameters or datasets).
#' @param sim,datasets Arguments inherited from `constructor` in [`estimate_coord()`].
#' @details These functions simply extract required inputs from `sim` or `datasets` with appropriate checks.
#' @author Edward Lavender
#' @name get_
NULL

#' @rdname get_
#' @keywords internal

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

#' @rdname get_
#' @keywords internal

get_dataset_moorings <- function(sim, datasets) {
  check_names(datasets, "moorings")
  datasets$moorings
}

#' @rdname get_
#' @keywords internal

get_dataset_t.layer <- function(sim, datasets) {
  check_names(datasets, "t.layer")
  datasets$t.layer
}

#' @rdname get_
#' @keywords internal

get_parameter <- function(sim, parameter) {
  check_names(sim, parameter)
  sim[[parameter]]
}