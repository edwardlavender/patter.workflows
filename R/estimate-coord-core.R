estimate_coord <- function(sim, map, datasets, constructor, algorithm) {
  
  # Initialise
  coffee()
  cat_init(sim$index)
  check_names(sim, "index")
  
  # Define algorithm inputs
  args <- constructor(sim = sim, map = map, datasets = datasets)
  
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
  dout <- data.table(id      = sim$index, 
                     method  = deparse(substitute(routine)), 
                     routine = deparse(substitute(routine)), 
                     success = success, 
                     error   = error, 
                     ntrial  = NA_integer_,
                     time    = time)
  
  # (optional) Write outputs
  write <- rlang::has_name(sim, "folder_coord")
  if (write) {
    pfile <- "coord.qs"
    dfile <- "data.qs"
    if (success) {
      qs::qsave(pout, file.path(sim$folder_coord, pfile))
    }
    qs::qsave(dout, file.path(sim$folder_coord, dfile))
    return(nothing())
  } else {
    return(list(coord = pout, callstats = dout))
  }
  
}