# Extract a call statistic (e.g., convergence) from a particle algorithm
# * x is the output of pf_filter() or pf_smoother_two_filter()
# * If x != NULL, we extract convergence from the object
# * Otherwise, we extract convergence from the Julia object
# * (i.e., if .fun was implemented with .collect)
pf_callstat <- function(.x, .fun, .direction, .stat) {
  if (!is.null(.x)) {
    check_names(.x, "callstats")
    check_names(.x$callstats, .stat)
    .x$callstats[[.stat]]
  } else {
    pf_obj <- patter:::name_particles(.fun = .fun, .direction = .direction)
    julia_eval(glue('{pf_obj}.callstats.{.stat}'))
  }
}

# Delete objects in Julia memory 
set_nothing <- function() {
  rlang::check_installed(c("glue", "JuliaCall"))
  pfwd <- patter:::name_particles(.fun = "pf_filter", .direction = "forward")
  pbwd <- patter:::name_particles(.fun = "pf_filter", .direction = "backward")
  ptf  <- patter:::name_particles(.fun = "pf_smoother_two_filter")
  julia_command(glue('{pfwd} = nothing;'))
  julia_command(glue('{pbwd} = nothing;'))
  julia_command(glue('{ptf} = nothing;'))
  nothing()
}

