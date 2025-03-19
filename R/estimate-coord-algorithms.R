#' @title Estimate coordinates: algorithms
#' @description These functions implement algorithms that estimate coordinates. 
#' @details
#' * [`algorithm_coa`] wraps `patter::coa()`;
#' * [`algorithm_rsp`] is [`RSP::runRSP()`];
#' * [`algorithm_particle`] wraps [`patter`] functions: 
#'    - The `iteration` row (`sim`) should include:
#'        - A `"smooth"` `logical` variable that defines whether or not to implement smoothing;
#'    - The `constructor` function should return a named `list` with `"forward"` and `"backward"` elements;
#'    - The forward element are used to run the forward particle filter via [`pf_filter()`];
#'    - The backward element is used to run the backward particle filter via [`pf_filter()`];
#'    - Particle smoothing is then implemented via [`pf_smoother_two_filter()`];
#'    - The function returns a named `list` with the following elements:
#'        - `"forward`
#'        - `"backward`
#'        - `"smooth"`
#'        
#' @author Edward Lavender
#' @name algorithm
NULL

#' @rdname algorithm
#' @export

algorithm_coa <- patter::coa

#' @rdname algorithm
#' @export

algorithm_rsp <- RSP::runRSP

#' @rdname algorithm
#' @export

algorithm_particle <- function(sim, map, datasets, constructor, verbose) {
  
  # TO DO
  # * Implement verbose properly 
  
  #### Initialise
  coffee()
  cat_init(sim$index)
  check_names(sim, "smooth")
  
  # Define algorithm inputs
  # (timeline, movement model, observation model, etc.)
  args <- constructor(sim = sim, map = map, datasets = datasets, verbose = verbose)
  check_names(args, c("forward", "backward"))
  fwd <- bwd <- smo <- NULL
  
  #### Implement forward filter
  cat("... (1) Implementing forward filter...\n")
  fwd <- do.call(pf_filter, args$forward)
  
  #### Implement backward filter 
  success <- fwd$callstats$convergence
  if (success & sim$smooth) {
    cat("\n... (2) Implementing backward filter...\n")
    args$.direction <- "backward"
    bwd             <- do.call(pf_filter, args$backward)
    success         <- bwd$callstats$convergence
  }
  
  #### Implement smoothing 
  if (success & sim$smooth) {
    cat("\n... (3) Implementing smoother...\n")
    smo <- do.call(pf_smoother_two_filter, args)
  }
  
  #### Julia clean up
  set_nothing()
  
  #### Return outputs
  list(forward = fwd, backward = bwd, smooth = smo)
  
}