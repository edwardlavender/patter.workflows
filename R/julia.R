set_nothing <- function() {
  pfwd <- patter:::name_particles(.fun = "pf_filter", .direction = "forward")
  pbwd <- patter:::name_particles(.fun = "pf_filter", .direction = "backward")
  ptf  <- patter:::name_particles(.fun = "pf_smoother_two_filter")
  julia_command(glue('{pfwd} = nothing;'))
  julia_command(glue('{pbwd} = nothing;'))
  julia_command(glue('{ptf} = nothing;'))
  nothing()
}