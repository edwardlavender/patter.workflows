#' @title Estimate coordinates
#' @description This internal function is the workhorse of [`lapply_estimate_coord()`]. The function wraps an `algorithm` function that estimates coordinates. 
#' @param sim,map,datasets,constructor,algorithm,coffee Arguments directly inherited from [`lapply_estimate_coord()`].
#' @param verbose A `logical` variable that defines whether or not to send use outputs to the console or a `.txt` file. 
#' @details For details, see the wrapper function [`lapply_estimate_coord()`].
#' @author Edward Lavender
#' @name estimate_coord
#' @keywords internal

estimate_coord <- function(sim, map, datasets, constructor, algorithm, coffee, verbose) {
  
  # Initialise
  coffee_do(coffee)
  cat_next(index = sim$index, verbose = verbose)
  
  # Define algorithm inputs
  args     <- constructor(sim = sim, map = map, datasets = datasets, verbose = verbose)
  
  # Run algorithm
  error   <- NA_character_
  success <- TRUE
  t1      <- Sys.time()
  pout    <- tryCatch(do.call(algorithm, args), error = function(e) e)
  t2      <- Sys.time()
  
  # Handle errors
  if (inherits(pout, "error")) {
    success <- FALSE
    error   <- pout$message
    message(error)
  } else {
    time <- secs(t2, t1)
  }
  
  # Collect success statistics
  # * TO DO
  # * Currently, success = TRUE even if the algorithm did not converge (in the case of a particle algorithm);
  dout <- data.table(id        = sim$index, 
                     algorithm = deparse(substitute(algorithm)), 
                     success   = success, 
                     error     = error, 
                     ntrial    = NA_integer_,
                     time      = time)
  
  # (optional) Write outputs
  write <- rlang::has_name(sim, "folder_coord")
  if (write) {
    pfile <- "coord.qs"
    dfile <- "callstats.qs"
    if (success) {
      qs::qsave(pout, file.path(sim$folder_coord, pfile))
    }
    qs::qsave(dout, file.path(sim$folder_coord, dfile))
    return(nothing())
  } else {
    return(list(coord = pout, callstats = dout))
  }
  
}