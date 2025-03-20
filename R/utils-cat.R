#' @title Utilities: `cat_*()` helpers
#' @name cat_

#' @rdname cat_
#' @keywords internal

# Print the time 
cat_time <- function() {
  cat(paste0(Sys.time(), "\n"))
  nothing()
}

#' @rdname cat_
#' @keywords internal

# Print a line 
cat_line <- function() {
  cat("\n\n\n---------------------------------------------------------------\n")
  nothing()
}

#' @rdname cat_
#' @keywords internal

# Print the row id 
cat_row <- function(index) {
  cat(paste0("\n On row ", index, "...\n"))
  nothing()
}

#' @rdname cat_
#' @keywords internal

# Initialise cat for a given iteration
cat_next <- function(index, verbose) {
  if (verbose) {
    cat_line()
    cat_row(index)
  }
  nothing()
}

#' @title Utilities: `sink_*()` wrappers
#' @name sink_

# Optionally run additional cat() calls
cat_do <- function(..., verbose) {
  if (verbose) {
    do.call(cat, list(...))
  }
  nothing()
}

#' @title Utilities: `sink_*()` wrappers
#' @name sink_

sink_open <- function(log.txt = NULL) {
  if (!is.null(log.txt)) {
    # Define connection 
    log.txt <- file(log.txt, open = "wt")
    # Open sink
    sink(log.txt, append = TRUE)
    sink(log.txt, type = "message", append = TRUE)
    # Print start time
    cat_time()
  }
  invisible(log.txt)
}

#' @rdname sink_
#' @keywords internal

sink_close <- function(log.txt = NULL) {
  if (!is.null(log.txt)) {
    cat_time()
    sink()
    sink(type = "message")
  }
  invisible(NULL)
}