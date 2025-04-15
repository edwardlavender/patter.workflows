#' @title Estimate maps: algorithms
#' @description These functions map estimated coordinates. 
#' @param map A [terra::SpatRaster].
#' @param ... Arguments for algorithms defined in other packages:
#' * For [`estimate_map_pou()`], `...` is passed directly to [`patter::map_pou()`];
#' * For [`estimate_map_dens()`], `...` is passed to [`patter::map_dens()`];
#' * For [`estimate_map_dbbmm()`], `...` is passed directly to [`RSP::dynBBMM()`];
#' @details
#' * `estimate_map_pou(...)` is simply `map_pou(...)$ud`
#' * `estimate_map_dens(...)` is simply `map_dens(...)$ud`
#' * `estimate_map_dbbmm(...)` runs `RSP::dynBBMM(...)` and then:
#'    - Converts the UD to a [`terra::SpatRaster`]
#'    - Aggregates individual UD layers, if required
#'    - Projects and resamples the aggregated UD onto `map`
#'    - Masks the result by `map` and normalises the distribution;
#' @return
#' All functions return a [`terra::SpatRaster`].
#' @author Edward Lavender
#' @name estimate_map-algorithm
NULL

#' @rdname estimate_map-algorithm
#' @export

estimate_map_pou <- function(...) {
  map_pou(...)$ud
}

#' @rdname estimate_map-algorithm
#' @export

estimate_map_dens <- function(...) {
  map_dens(...)$ud
}

#' @rdname estimate_map-algorithm
#' @export

estimate_map_dbbmm <- function(map, ...) {
  rlang::check_installed("spatial.extensions")
  dbb <- RSP::dynBBMM(...)
  # Process UD
  dbb <- dbb$dbbmm[[1]]
  dbb <- terra::rast(dbb)
  if (terra::nlyr(dbb) > 1L) {
    dbb <- spatNormalise(terra::app(dbb, "sum"))
  }
  # Resample RSP onto map grid for consistency
  ud <- terra::project(dbb, terra::crs(map))
  ud <- terra::resample(ud, map)
  ud <- terra::mask(ud, map)
  spatNormalise(ud)
}

