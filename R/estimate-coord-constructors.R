#' @title Estimate coordinates: constructors
#' @description `estimate_coord_*()` `constructor` functions construct a named `list` of arguments for an `estimate_coord_*()` function. 
#' @param sim,datasets,verbose Arguments inherited from [`cl_lapply_workflow()`]:
#' * `sim` is a [`data.table`] row;
#' * Other arguments are as described for [`cl_lapply_workflow()`];
#' @details
#' The following `constructors` are built in to [`patter.workflows`]:
#' * [`constructor_coa()`] assembles a named list of arguments for [`estimate_coord_coa()`];
#' * [`constructor_rsp()`] assembles a named list of arguments for [`estimate_coord_rsp()`];
#' 
#' For [`constructor_coa()`]:
#' * `sim` should include the following columns:
#'      - `unit_id`: an integer that defines the individual/block ID;
#'      - `delta_t`: the time interval over which to calculate centres of activity (see [`estimate_coord_coa()`]);
#' * `datasets` should include the following elements:
#'      - `detections_by_unit`: a `list` of acoustic detection [`data.table`]s (see [`estimate_coord_coa()`]) for each `unit_id`;
#'      - `moorings`: a `moorings` data.table (see [`estimate_coord_coa()`]);
#'
#' For [`constructor_rsp()`]:
#' * `sim` should include the following columns:
#'      - `unit_id`: see above;
#'      - `er.ad`: the `er.ad` parameter (see [`estimate_coord_rsp()`]);
#' * `datasets` should include the following elements:
#'      - `detections_by_unit`: see above;
#'      - `moorings`: see above;
#'      - `t.layer`: the transition matrix (see [`estimate_coord_rsp()`])
#'      
#' Inbuilt constructors are implemented as follows:
#' * The dataset(s) for `sim$unit_id` (e.g., for a specific individual/time window) are extracted from `datasets` **by position** via `get_dataset_*()` functions (e.g., [`get_dataset_detections()`]);
#' * Supported parameters (e.g., `er.ad`) are extracted from `sim` via [`get_parameter()`];
#' * (Other parameters are left at default settings);
#' * A named `list` of arguments, including datasets and parameters, for the relevant function (e.g., [`estimate_coord_coa()`]) is returned;
#' 
#' For more control, define a custom `constructor_*()` function.
#' 
#' For [`estimate_coord_particle()`] constructors have to be defined manually, following the same function signature. 
#' 
#' @return `constructor_*()` functions return a named `list` of arguments for an algorithm (e.g., [`estimate_coord_coa()`]).
#' @author Edward Lavender
#' @name estimate_coord-constructor

#' @rdname estimate_coord-constructor
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

#' @rdname estimate_coord-constructor
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
