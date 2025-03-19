#' @title Estimate coordinates
#' @description These functions support coordinate estimation. 
#' @details
#' The call stack is as follows:
#' 1. `lapply_estimate_coord_*()` is the exported interface for iterative coordinate estimation using different algorithms;
#' 2. `lapply_estimate_coord()` is the core internal routine;
#' 3. `estimate_coord_*()` is a interface for coordinate estimation for a single iteration for different algorithms;
#' 4. `estimate_coord()` is the core internal routine, implementing the algorithms with error handling, call statistics etc.;
#' 5. `algorithm_*()` functions implement the algorithms;
#' @author Edward Lavender
#' @name estimate_coord

#' @rdname estimate_coord
#' @keywords internal

estimate_coord_coa <- function(sim, map, datasets, constructor) {
  estimate_coord(sim         = sim, 
                 map         = map, 
                 datasets    = datasets, 
                 constructor = constructor, 
                 algorithm   = algorithm_coa)
}

#' @rdname estimate_coord
#' @keywords internal

estimate_coord_rsp <- function(sim, map, datasets, constructor) {
  estimate_coord(sim         = sim, 
                 map         = map, 
                 datasets    = datasets, 
                 constructor = constructor, 
                 algorithm   = algorithm_rsp)
}

#' @rdname estimate_coord
#' @keywords internal

estimate_coord_patter <- function(sim, map, datasets, constructor) {
  estimate_coord(sim         = sim, 
                 map         = map, 
                 datasets    = datasets, 
                 constructor = constructor, 
                 algorithm   = algorithm_particle)
}