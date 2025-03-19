#' @title Utilities: `cat_*()` helpers
#' @name cat_

#' @rdname cat_
#' @keywords internal

cat_line <- function() {
  cat("\n\n\n---------------------------------------------------------------\n")
}

#' @rdname cat_
#' @keywords internal

cat_iter <- function(t) {
  msg("\n On row {t}...", .envir = environment())
}

#' @rdname cat_
#' @keywords internal

cat_init <- function(t) {
  cat_line()
  cat_iter(t)
  nothing()
}

#' @title Utilities: `sink_*()` wrappers
#' @name sink_

sink_open <- function(log.folder = NULL, log.txt = NULL) {
  # Define log.txt path
  if (!is.null(log.folder)) {
    # Define name
    if (is.null(log.txt)) {
      log.txt <- paste0("log-", as.numeric(Sys.time()), ".txt")
    }
    # Define full file path & validate 
    log.txt <- file.path(log.folder, log.txt)
    # stopifnot(!file.exists(log.txt))
    # Define connection 
    log.txt <- file(log.txt, open = "wt")
    # Open sink
    sink(log.txt, append = TRUE)
    sink(log.txt, type = "message", append = TRUE)
    # Print start time
    print(Sys.time())
  }
  invisible(log.txt)
}

#' @rdname sink_
#' @keywords internal

sink_close <- function(log.txt = NULL) {
  if (!is.null(log.txt)) {
    print(Sys.time())
    sink()
    sink(type = "message")
  }
  invisible(NULL)
}