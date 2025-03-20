#' @title Utilities: task management 
#' @description Give your computer 'coffee breaks' during iterative activities for rest. 
#' @param nodenames A `character` vector that defines the name(s) of the nodes on which to take coffee breaks. 
#' @param folder A `character` that defines the folder in which coffee break times are stored. This is created, recursively, if required.
#' @param interval A `number` that defines the minimum time interval (s) between sequential coffee breaks. 
#' @param duration A `number` that defines the duration (s) of the coffee break.
#' @param args `NULL`, `list()` or a named `list` of arguments used to implement [`coffee()`].
#' @details 
#' * [`coffee()`] is designed to be called in iterative applications (i.e., loops). 
#'    * For machines (`Sys.info()["nodename"]`) in `nodenames`, the function looks up the time since the last coffee break. 
#'    * If the time since the last break exceeds `interval`, the machine sleeps for `duration` seconds. 
#'    * After the break, the function records the system time in a `time.qs` file written to `folder`. 
#'
#' * Use [`coffee_do()`] to run [`coffee()`] given `args`.
#'    
#' * Use `coffee_cleanup()` after all iterations to `unlink(folder)`.
#' 
#' @return All functions return `invisible(NULL)`. 
#' * [`coffee()`] and [`coffee_do()`] create `folder` and `time.qs` files as side effects.
#' * [`coffee_cleanup()`] unlinks `folder` as a side effect. 
#' @author Edward Lavender
#' @name coffee
NULL

#' @name coffee
#' @keywords internal

coffee <- function(nodenames = Sys.info()["nodename"],
                   folder = file.path(tempdir(), "coffee"),
                   interval = 2 * 60 * 60,
                   duration = 15 * 60) {
  
  # Take breaks on selected nodename(s) only
  if (!(Sys.info()["nodename"] %in% nodenames)) {
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

#' @name coffee
#' @keywords internal

coffee_do <- function(args) {
  if (!is.null(args)) {
    do.call(coffee, args)
  }
  nothing()
}

#' @name coffee
#' @keywords internal

coffee_cleanup <- function(args) {
  if (!is.null(args)) {
    if (rlang::has_name(args, "folder")) {
      coffeehouse <- args$folder
    } else {
      coffeehouse <- formals(args)$folder
    }
    unlink(coffeehouse)
  }
  nothing()
}
