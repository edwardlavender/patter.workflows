#' @title Estimate coordinates: algorithms
#' @description These functions are algorithms. 
#' @details
#' * [`algorithm_coa`] wraps `patter::coa()`
#' * [`algorithm_rsp`] is [`RSP::runRSP()`]
#' * [`algorithm_particle`] wraps [`patter::pf_filter()`] and [`patter::pf_smoother_two_filter()`] (via [`pf_filter_wrapper()`] and [`pf_smoother_wrapper()`]);
#' @author Edward Lavender
#' @rdname algorithm

#' @name algorithm
#' @keywords internal

algorithm_coa <- patter::coa

#' @name algorithm
#' @keywords internal

algorithm_particle <- RSP::runRSP

#' @name algorithm
#' @keywords internal

algorithm_particle <- function(sim, map, datasets, constructor) {
  
  #### Initialise
  coffee()
  cat_init(sim$index)
  check_names(sim, "smooth")
  
  # Define algorithm inputs
  # (timeline, movement model, observation model, etc.)
  args <- constructor(sim = sim, map = map, datasets = datasets)
  check_names(args, c("forward", "backward"))
  
  #### Implement forward filter
  cat("... (1) Implementing forward filter...\n")
  success <- pf_filter_wrapper(sim = sim, args = args$forward)
  
  #### Implement backward filter 
  if (success & sim$smooth) {
    cat("\n... (2) Implementing backward filter...\n")
    args$.direction <- "backward"
    success <- pf_filter_wrapper(sim = sim, args = args$backward)
  }
  
  #### Implement smoothing 
  if (success & sim$smooth) {
    cat("\n... (3) Implementing smoother...\n")
    success <- pf_smoother_wrapper(sim)
  }
  
  #### Clean up
  set_nothing()
  nothing()
  
}
