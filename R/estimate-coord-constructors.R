#' @title Estimate coordinates: constructors
#' @description `estimate_coord_*()` `constructor` functions construct a named `list` of arguments for an `estimate_coord_*()` function. 
#' @param .sim,.datasets,.verbose,... Arguments inherited from [`cl_lapply_workflow()`]:
#' * `.sim` is a [`data.table`] row;
#' * `.datasets` is a named `list` of .datasets, generally used to pass iteration-specific arguments to a `constructor`;
#' * `...` are additional static arguments passed to `constructor` for an `algorithm` function;
#' * `.verbose` controls user outputs;
#' @details
#' The following `constructors` are built in to [`patter.workflows`]:
#' * [`constructor_coa()`] assembles a named list of arguments for [`estimate_coord_coa()`];
#' * [`constructor_rsp()`] assembles a named list of arguments for [`estimate_coord_rsp()`];
#' 
#' For [`constructor_coa()`]:
#' * `.sim` should include the following columns:
#'      - `unit_id`: an integer that defines the individual/block ID;
#'      - `delta_t`: the time interval over which to calculate centres of activity (see [`estimate_coord_coa()`]);
#' * `.datasets` should include the following elements:
#'      - `map`: A [`terra::SpatRaster`] (see [`patter::coa()`])
#'      - `detections_by_unit`: a `list` of acoustic detection [`data.table`]s (see [`estimate_coord_coa()`]) for each `unit_id`;
#'      - `moorings`: a `moorings` data.table (see [`estimate_coord_coa()`]);
#'
#' For [`constructor_rsp()`]:
#' * `.sim` should include the following columns:
#'      - `unit_id`: see above;
#'      - `er.ad`: the `er.ad` parameter (see [`estimate_coord_rsp()`]);
#' * `.datasets` should include the following elements:
#'      - `map` as above;
#'      - `detections_by_unit`: see above;
#'      - `moorings`: see above;
#'      
#' Inbuilt constructors are implemented as follows:
#' * The dataset(s) for `.sim$unit_id` (e.g., for a specific individual/time window) are extracted from `.datasets` via `get_dataset_*()` functions (e.g., [`get_dataset_detections()`]);
#' * Supported parameters (e.g., `er.ad`) are extracted from `.sim` via [`get_parameter()`];
#' * (Other parameters are left at default settings); 
#' * A named `list` of arguments, including .datasets and parameters, plus any arguments passed via `...`, for the relevant function (e.g., [`estimate_coord_coa()`]) is returned;
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

constructor_coa <- function(.sim, .datasets, .verbose, ...) {
  # Define .datasets
  map        <- get_dataset_map(.sim, .datasets)
  detections <- get_dataset_detections(.sim, .datasets)
  moorings   <- get_dataset_moorings(.sim, .datasets)
  # Define parameters 
  delta_t    <- get_parameter(.sim, "delta_t")
  # Collect arguments 
  args <- list(.map          = map,
               .detections   = detections, 
               .moorings     = moorings, 
               .delta_t      = delta_t, 
               .plot_weights = FALSE)
  list_merge(args, list(...))
}

#' @rdname estimate_coord-constructor
#' @export

constructor_rsp <- function(.sim, .datasets, .verbose, ...) {
  # Define .datasets
  map        <- get_dataset_map(.sim, .datasets)
  detections <- get_dataset_detections(.sim, .datasets) 
  moorings   <- get_dataset_moorings(.sim, .datasets)
  act        <- as_actel(.map        = map, 
                         .detections = detections, 
                         .moorings   = moorings)
  # Define parameters
  # * At the time of writing, only er.ad can be specified dynamically
  # * Other parameters take default values
  er.ad <- get_parameter(.sim, "er.ad")
  # Collect arguments
  args <- list(input   = act,
               coord.x = "Longitude",
               coord.y = "Latitude",
               er.ad   = er.ad)
  list_merge(args, list(...))
}
