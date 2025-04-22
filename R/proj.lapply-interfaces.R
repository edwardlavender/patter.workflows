#' @title Additional routines for iterative workflows with `proj.lapply`
#' @description These functions provide additional configure options for selected routines. 
#' @param .sim,.cl Arguments inherited from [`proj.lapply::cl_lapply_workflow()`]. 
#' @details 
#' * [`particle_startup()`] configures sockets for [`estimate_coord_particle()`]. The function connects to `Julia` on each socket (assuming `JULIA` options are set globally) and checks for CPU oversubscription. 
#' @author Edward Lavender
#' @name config
NULL

#' @rdname config
#' @export

particle_startup <- function(.sim, .cl) {
  rlang::check_installed(c("glue", "JuliaCall", "parallel"))
  # Reconnect to Julia 
  # * We assume Julia options are set globally 
  julia_connect(.socket = TRUE)
  # Check the number of threads in Julia 
  cl_julia <- julia_eval("Threads.nthreads()")
  # Check the total number of cores available 
  cl_total <- parallel::detectCores()
  # Throw error for CPU oversubscription 
  msg("Using {.cl} R processes, each with {cl_julia} threads (i.e., {.cl * cl_julia} / {cl_total} detected cores).", 
      .envir = environment())
  abort("CPU oversubscription is not allowed.")
  nothing()
}