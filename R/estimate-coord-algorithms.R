#' @title Estimate coordinates: algorithms
#' @description These functions implement algorithms that estimate coordinates.
#' @param ... Arguments for algorithms defined in other packages:
#' * For [`estimate_coord_coa`], `...` is passed directly to [`patter::coa()`];
#' * For [`estimate_coord_rsp`], `...` is passed directly to [`RSP::runRSP()`];
#' @param forward,backward,smooth,verbose Arguments for [`estimate_coord_particle()`]:
#' * `forward` is a named `list` of arguments for the forward particle filter ([`patter::pf_filter()`]);
#' * `backward` is a named `list` of arguments for the backward particle filter ([`patter::pf_filter()`]);
#' * `smooth` is a named `list` of arguments for the two-filter smoother ([`patter::pf_smoother_two_filter()`]);
#' 
#' To implement smoothing, all arguments must be set. Use `smooth = list()` to implement smoothing with default arguments. 
#'
#' Use `backward = NULL` and `smooth = NULL` to suppress smoothing (and only run the forward particle filter). 
#' 
#' @return
#' All functions return estimated coordinates in function-specific formats:
#' * For [`estimate_coord_coa()`], see [`patter::coa()`];
#' * For [`estimate_coord_rsp()`], see [`RSP::runRSP()`];
#' * For [`estimate_coord_particle()`], a `named` list with three elements is returned:
#'    * `forward` holds the outputs from the forward particle filter ([`patter::pf_filter()`]);
#'    * `backward` holds the outputs from the backward particle filter ([`patter::pf_filter()`]) or is `NULL` if this routine was not implemented;
#'    * `smooth` holds the outputs from the two-filter smoother ([`patter::pf_smoother_two_filter()`]) or is `NULL` if this routine was not implemented;
#'        
#' @author Edward Lavender
#' @name estimate_coord
NULL

#' @rdname estimate_coord
#' @export

estimate_coord_coa <- function(...) {
  patter::coa(...)
}

#' @rdname estimate_coord
#' @export

estimate_coord_rsp <- function(...) {
  RSP::runRSP(...) 
}

#' @rdname estimate_coord
#' @export

estimate_coord_particle <- function(forward, backward, smooth, verbose) {
  
  #### Initialise
  rlang::check_installed("proj.lapply")
  coffee()
  fwd <- bwd <- smo <- NULL
  
  #### Implement forward filter
  cat_do("... (1) Implementing forward filter...\n", .verbose = verbose)
  fwd <- do.call(pf_filter, forward)
  
  #### Implement backward filter 
  success <- fwd$callstats$convergence
  if (success & !is.null(backward)) {
    cat_do("\n... (2) Implementing backward filter...\n", .verbose = verbose)
    backward.direction <- "backward"
    bwd                <- do.call(pf_filter, backward)
    success            <- bwd$callstats$convergence
  }
  
  #### Implement smoothing 
  if (success & !is.null(smooth)) {
    cat_do("\n... (3) Implementing smoother...\n", .verbose = verbose)
    smo <- do.call(pf_smoother_two_filter, smooth)
  }
  
  #### Julia clean up
  set_nothing()
  
  #### Return outputs
  list(forward = fwd, backward = bwd, smooth = smo)
  
}
