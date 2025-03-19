lapply_estimate_coord <- function(iteration, 
                                  map,
                                  datasets, 
                                  coffeehouse,
                                  log.txt, 
                                  verbose, 
                                  algorithm) {
  
  # Boilerplate  
  t1   <- Sys.time()
  log.txt <- sink_open(log.txt = log.txt)
  cats <- cat_setup(.fun = algorithm, .verbose = verbose)
  on.exit(eval(cats$exit, envir = cats$envir), add = TRUE)
  on.exit(sink_close(log.txt), add = TRUE)
  check_dir_empty(coffeehouse)
  
  # Estimate coordinates via algorithm()
  iteration_ls <- split(iteration, seq_row(iteration))
  out <- cl_lapply(iteration_ls, function(sim) {
    algorithm(sim = sim, 
              map = map,
              datasets = datasets)
  })
  
  out
  
}
