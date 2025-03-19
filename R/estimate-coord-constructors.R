#' @title Estimate coordinates: constructors
#' @description `constructor` functions construct a named `list` of arguments for an algorith implementation. 
#' @param sim
#' @param map
#' @param datasets
#' @details
#' Default `constructors` are provided for the COA, RSP and AC particle algorithms. 
#' 
#' For [`constructor_coa()`]:
#' * `sim` should include a `delta_t` column
#' * `datasets` should include `detections_by_unit` and `moorings`
#'
#' For [`constructor_rsp()`]:
#' * `sim` should include an `er.ad` column
#' * `datasets` should include `detections_by_unit`, `moorings` and `t.layer`
#' 
#' For particle algorithms, constructors have to be defined manually. 
#' 
#' @author Edward Lavender
#' @name constructor

#' @rdname constructor
#' @keywords internal 

constructor_coa <- function(sim, map, datasets) {
  # Checks
  check_names(datasets, c("detections_by_unit", "moorings"))
  check_names(sim, "delta_t")
  # Define datasets
  detections <- datasets$detections_by_unit[[sim$unit_id]]
  moorings   <- datasets$moorings
  # Define parameters 
  delta_t    <- sim$delta_t
  # Collect arguments 
  list(.map          = map,
       .detections   = detections, 
       .moorings     = moorings, 
       .delta_t      = delta_t, 
       .plot_weights = FALSE)
}

#' @rdname constructor
#' @keywords internal 

constructor_rsp <- function(sim, map, datasets) {
  # Checks
  check_names(datasets, c("detections_by_unit", "moorings", "t.layer"))
  check_names(sim, "er.ad")
  # Define datasets
  detections <- datasets$detections_by_unit[[sim$unit_id]]
  moorings   <- datasets$moorings
  act        <- as_actel(map = map, 
                         detections = detections, 
                         moorings = moorings)
  t.layer    <- datasets$tm
  # Define parameters
  # * At the time of writing, only er.ad can be specified dynamically
  # * Other parameters take default values
  er.ad <- sim$er.ad
  # Collect arguments
  args <- list(input = act,
               t.layer = t.layer,
               coord.x = "Longitude", coord.y = "Latitude",
               er.ad = er.ad)
}