#' @title Estimate map: constructors
#' @description `estimate_map_*()` `constructor` functions construct a named `list` of arguments for an `estimate_map_*()` function. 
#' @param .sim,.datasets,.verbose,... Constructor arguments (see [`estimate_coord-constructor`]).
#' @details
#' All `constructor` functions have a common signature (see [`estimate_coord-constructor`]).
#' 
#' For `constructor_map_*()` functions, .datasets should include `map` and `coord` elements. A `dataset$coordinate()` element may also be required (see [`get_dataset_coord()`]).
#' 
#' For full customisation, a custom `constructor_*()` function is required. 
#' 
#' @return `constructor_*()` functions return a named `list` of arguments for an algorithm (e.g., [`estimate_coord_coa()`]).
#' @author Edward Lavender
#' @name estimate_map-constructor

#' @rdname estimate_map-constructor
#' @export 

constructor_map_pou <- function(.sim, .datasets, .verbose, ...) {
  map   <- get_dataset_map(.sim, .datasets)
  coord <- get_dataset_coord(.sim, .datasets)
  args <- list(.map = map, .coord = coord, .plot = FALSE, .verbose = .verbose)
  list_merge(args, list(...))
}

#' @rdname estimate_map-constructor
#' @export 

constructor_map_dens <- function(.sim, .datasets, .verbose, ...) {
  map   <- get_dataset_map(.sim, .datasets)
  coord <- get_dataset_coord(.sim, .datasets)
  args <- list(.map        = map,
               .coord      = coord, 
               .discretise = TRUE, 
               .plot       = FALSE, 
               .fterra     = FALSE,
               .verbose    = .verbose, ...)
  list_merge(args, list(...))
}

#' @rdname estimate_map-constructor
#' @export 

constructor_map_dbbmm <- function(.sim, .datasets, .verbose, ...) {
  map   <- get_dataset_map(.sim, .datasets)
  coord <- get_dataset_coord(.sim, .datasets)
  args <- list(map = map,
               input = coord)
  list_merge(args, list(...))
}