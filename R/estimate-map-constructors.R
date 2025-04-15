#' @title Estimate map: constructors
#' @description `estimate_map_*()` `constructor` functions construct a named `list` of arguments for an `estimate_map_*()` function. 
#' @param sim,datasets,verbose Constructor arguments.
#' @details
#' All `constructor` functions have a common signature (see [`estimate_coord-constructor`]).
#' 
#' For [`constructor_map_pou()`]:
#' * `datasets` should include the following elements:
#'      - `map`
#'      - `coord`
#'
#' For [`constructor_map_dens()`]:
#' * `sim` should include:
#'      - `sigma`
#' * `datasets` should include the following elements:
#'      - `map`
#'      - `coord`
#' 
#' For [`constructor_map_dbbmm`]:
#' * `datasets` should include the following elements:
#'      - `map`
#'      - `coord`
#'      - `base.raster`
#'      - `UTM`
#' 
#' For full customisation, a custom `constructor_*()` function is required. 
#' 
#' @return `constructor_*()` functions return a named `list` of arguments for an algorithm (e.g., [`estimate_coord_coa()`]).
#' @author Edward Lavender
#' @name estimate_coord-constructor

#' @rdname estimate_coord-constructor
#' @export 

constructor_map_pou <- function(sim, datasets, verbose) {
  coord <- get_dataset_coord(sim, datasets)
  map   <- get_dataset_map(sim, datasets)
  list(.map = map, .coord = coord, .plot = FALSE, .verbose = verbose)
}

#' @rdname estimate_coord-constructor
#' @export 

constructor_map_dens <- function(sim, datasets, verbose) {
  coord <- get_dataset_coord(sim, datasets)
  map   <- get_dataset_map(sim, datasets)
  sigma <- get_parameter(sim, "sigma")
  list(.map        = map,
       .coord      = coord, 
       .discretise = TRUE, 
       .sigma      = sigma,
       .plot       = FALSE, 
       .fterra     = FALSE,
       .verbose    = verbose) 
}

#' @rdname estimate_coord-constructor
#' @export 

constructor_map_dbbmm <- function(sim, datasets, verbose) {
  check_names(datasets, c("map", "coord", "base.raster", "UTM"))
  coord <- get_dataset_coord(sim, datasets)
  map   <- get_dataset_map(sim, datasets)
  list(map = map,
       input = coord, 
       base.raster = datasets$base.raster, 
       UTM = datasets$UTM)
}