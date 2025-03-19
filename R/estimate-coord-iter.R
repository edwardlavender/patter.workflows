#' @title Estimate coordinates
#' @description These functions iteratively estimate coordinates using selected algorithms, such as the COA, RSP and particle algorithms (i.e., [`coa_algorithm()`], [`rsp_algorithm()`] and [`particle_algorithm()`]). 
#' @param iteration An iteration [`data.table`]. 
#' * Each row corresponds to one algorithm run and is passed to `constructor()`.
#' * Required columns depend on the `constructor()` function but generally include run-specific parameters (such as `delta_t` for the [`patter::coa()`]) and file paths. 
#' @param map A map that defines the study area, passed to `constructor()`. 
#' * For [`estimate_coord_coa()`] and [`estimate_coord_rsp()`] `map` is a [`SpatRaster`]. 
#' * For [`estimate_coord_patter()`], `map` can be a [`SpatRaster`] or a `character` file path. 
#' @param datasets (optional) A named `list` of datasets, passed to `constructor()`.
#' @param constructor A [`constructor`] `function` that constructs a named `list` of arguments for the algorithm routine (e.g., [`patter::coa()`]). 
#' * For [`algorithm_coa()`] and [`algorithm_rsp()`], default constructors are provided. 
#' * For [`algorithm_particle()`], a require a user-defined constructor is required.
#' 
#' For function requirements, see [`constructor`]. 
#' 
#' @param (optional) log.coffee,log.txt Log options. 
#' * `log.coffee` is a `character` that defines the directory in which to record the timing of 'coffee breaks', during which time the computer is given a rest (see [`coffee()`]). Use `NULL` to suppress coffee breaks. 
#' * If `verbose = TRUE`, `log.txt` is a `character` that defines a path to a `log.txt` file in which output user messages are recorded. 
#' @param verbose User output control options. 
#' * `verbose` is a `logical` variable that defines whether or not to output user messages to the console or `log.txt` if specified. 
#' 
#' @details
#' `lapply_estimate_coord_*()` wraps the internal function [`lapply_estimate_coord()`]. This function iterate over rows in `iteration` via [`cl_lapply()`] and applies one of the following internal routines:
#' * [`estimate_coord_coa()`]
#' * [`estimate_coord_rsp()`]
#' * [`estimate_coord_patter()`]
#' 
#' Internally, these routines wrap [`estimate_coord()`]. This function:
#' * Constructs a named `list` of arguments for an algorithm function, via a [`constructor`], given information in the `iteration` row, `map` and `datasets`;
#' * Passes the arguments to an algorithm function, such as:
#'    * [`algorithm_coa()`]
#'    * [`algorithm_rsp()`]
#'    * [`algorithm_particle()`]);
#' * Implements error handling;
#' * Records estimated coordinates and call statistics (such as computation time);
#' * Optionally writes outputs to disk or returns them in memory 
#' 
#' @author Edward Lavender
#' @name lapply_estimate_coord

#' @rdname lapply_estimate_coord
#' @export

lapply_estimate_coord_coa <- function(iteration, 
                                      map,
                                      datasets, 
                                      constructor = constructor_coa,
                                      log.coffee = NULL,
                                      log.txt = NULL, 
                                      verbose = TRUE) {
  
  lapply_estimate_coord(iteration   = iteration, 
                        map         = map, 
                        datasets    = datasets, 
                        constructor = constructor,
                        log.coffee  = log.coffee, 
                        log.txt     = log.txt, 
                        verbose     = verbose, 
                        algorithm   = estimate_coord_coa)
}

#' @rdname lapply_estimate_coord
#' @export

lapply_estimate_coord_rsp <- function(iteration, 
                                      map,
                                      datasets,
                                      constructor = constructor_rsp,
                                      log.coffee = NULL,
                                      log.txt = NULL, 
                                      verbose = TRUE) {
  
  lapply_estimate_coord(iteration   = iteration, 
                        map         = map, 
                        datasets    = datasets,
                        constructor = constructor,
                        log.coffee  = log.coffee, 
                        log.txt     = log.txt, 
                        verbose     = verbose, 
                        algorithm   = estimate_coord_rsp)
  
}

#' @rdname lapply_estimate_coord
#' @export

lapply_estimate_coord_patter <- function(iteration, 
                                         map,
                                         datasets, 
                                         constructor,
                                         log.coffee = NULL,
                                         log.txt = NULL, 
                                         verbose = TRUE) {
  
  lapply_estimate_coord(iteration   = iteration, 
                        map         = map, 
                        datasets    = datasets, 
                        constructor = constructor,
                        log.coffee  = log.coffee, 
                        log.txt     = log.txt, 
                        verbose     = verbose, 
                        algorithm   = estimate_coord_patter)
  
}