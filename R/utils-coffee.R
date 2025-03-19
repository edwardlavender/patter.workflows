#' @title Take a coffee break
#' @description Pause and rest the computer
#' @details Only one interval & duration is currently supported 
#' @author Edward Lavender
#' @keywords internal

coffee <- function(computer = Sys.info()["nodename"],
                   folder = file.path(tempdir(), "coffee"),
                   interval = 2 * 60 * 60,
                   duration = 15 * 60) {
  
  # Take breaks on selected computer(s) only
  if (!(Sys.info()["nodename"] %in% computer)) {
    return(nothing())
  }
  
  # Identify time since last 'coffee' break
  if (!dir.exists(folder)) {
    dir.create(folder, recursive = TRUE)
  }
  time.qs <- file.path(folder, "time.qs")
  if (!file.exists(time.qs)) {
    qs::qsave(Sys.time(), time.qs)
    return(nothing())
  }
  time              <- qs::qread(time.qs)
  time_since_coffee <- difftime(Sys.time(), time, units = "secs")
  
  # Take a break if needed
  if (time_since_coffee >= interval) {
    # Sleep for `duration`
    Sys.sleep(duration)
    # Record the time of the break
    # (We'll sleep in another `duration` secs)
    qs::qsave(Sys.time(), time.qs)
  }
  
  nothing()
  
}
