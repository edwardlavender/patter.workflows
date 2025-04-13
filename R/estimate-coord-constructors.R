#' @title Estimate coordinates: constructors
#' @description `constructor` functions construct a named `list` of arguments for an algorithm implementation. 
#' @param sim,datasets,verbose Arguments inherited from [`cl_lapply_workflow()`]:
#' * `sim` is a [`data.table`] row;
#' * Other arguments are as described for [`cl_lapply_workflow()`];
#' @details
#' The following `constructors` are built in to [`patter.workflows`]:
#' * [`constructor_coa()`] assembles a named list of arguments for [`algorithm_coa()`];
#' * [`constructor_rsp()`] assembles a named list of arguments for [`algorithm_rsp()`];
#' 
#' For [`constructor_coa()`]:
#' * `sim` should include the following columns:
#'      - `unit_id`: an integer that defines the individual/block ID;
#'      - `delta_t`: the time interval over which to calculate centres of activity (see [`algorithm_coa()`]);
#' * `datasets` should include the following elements:
#'      - `detections_by_unit`: a `list` of acoustic detection [`data.table`]s (see [`algorithm_coa()`]) for each `unit_id`;
#'      - `moorings`: a `moorings` data.table (see [`algorithm_coa()`]);
#'
#' For [`constructor_rsp()`]:
#' * `sim` should include the following columns:
#'      - `unit_id`: see above;
#'      - `er.ad`: the `er.ad` parameter (see [`algorithm_rsp()`]);
#' * `datasets` should include the following elements:
#'      - `detections_by_unit`: see above;
#'      - `moorings`: see above;
#'      - `t.layer`: the transition matrix (see [`algorithm_rsp()`])
#'      
#' Inbuilt constructors are implemented as follows:
#' * The dataset(s) for `sim$unit_id` (e.g., for a specific individual/time window) are extracted from `datasets` **by position** via `get_dataset_*()` functions (e.g., [`get_dataset_detections()`]);
#' * Supported parameters (e.g., `er.ad`) are extracted from `sim` via [`get_parameter()`];
#' * (Other parameters are left at default settings);
#' * A named `list` of arguments, including datasets and parameters, for the relevant function (e.g., [`algorithm_coa()`]) is returned;
#' 
#' For more control, define a custom `constructor_*()` function.
#' 
#' For [`algorithm_particle()`] constructors have to be defined manually, following the same function signature. 
#' 
#' @return `constructor_*()` functions return a named `list` of arguments for an algorithm (e.g., [`algorithm_coa()`]).
#' @author Edward Lavender
#' @name constructor

#' @rdname constructor
#' @export 

constructor_coa <- function(sim, datasets, verbose) {
  # Define datasets
  map        <- get_dataset_map(sim, datasets)
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

constructor_rsp <- function(sim, datasets, verbose) {
  # Define datasets
  map        <- get_dataset_map(sim, datasets)
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
