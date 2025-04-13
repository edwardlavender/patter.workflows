#' @title Estimate coordinates: algorithms
#' @description These functions implement algorithms that estimate coordinates.
#' @param ... Arguments for algorithms defined in other packages:
#' * For [`algorithm_coa`], `...` is passed directly to [`patter::coa()`];
#' * For [`algorithm_rsp`], `...` is passed directly to [`RSP::runRSP()`];
#' @param forward,backward,smooth,verbose Arguments for [`algorithm_particle()`]:
#' * `forward` is a named `list` of arguments for the forward particle filter ([`pf_filter()`]);
#' * `backward` is a named `list` of arguments for the backward particle filter ([`pf_filter()`]);
#' * `smooth` is a named `list` of arguments for the two-filter smoother ([`pf_smoother_two_filter()`]);
#' 
#' To implement smoothing, all arguments must be set. Use `smooth = list()` to implement smoothing with default arguments. 
#'
#' Use `backward = NULL` and `smooth = NULL` to suppress smoothing (and only run the forward particle filter). 
#' 
#' @return
#' All functions return estimated coordinates in function-specific formats:
#' * For [`algorithm_coa()`], see [`patter::coa()`];
#' * For [`algorithm_rsp()`], see [`RSP::runRSP()`];
#' * For [`algorithm_particle()`], a `named` list with three elements is returned:
#'    * `forward` holds the outputs from the forward particle filter ([`pf_filter()`]);
#'    * `backward` holds the outputs from the backward particle filter ([`pf_filter()`]) or is `NULL` if this routine was not implemented;
#'    * `smooth` holds the outputs from the two-filter smoother ([`pf_smoother_two_filter()`]) or is `NULL` if this routine was not implemented;
#'        
#' @author Edward Lavender
#' @name algorithm
NULL

#' @rdname algorithm
#' @export

algorithm_coa <- function(...) {
  patter::coa(...)
}

#' @rdname algorithm
#' @export

algorithm_rsp <- function(...) {
  RSP::runRSP(...) 
}

#' @rdname algorithm
#' @export

algorithm_particle <- function(forward, backward, smooth, verbose) {
  
  #### Initialise
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