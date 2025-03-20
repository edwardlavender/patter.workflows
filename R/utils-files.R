#' @title Utilities: file system 
#' @description These functions interface with the file system. 
#' @param paths For [`dirs.create()`], `paths` is `character` vector of directories.
#' @param from,to,cl Arguments for [`dirs.copy()`].
#' * `from` is a `character` vector of directories to copy. 
#' * `to` is a `character` vector of destination directories. 
#' * `cl` is a cluster argument passed to [`cl_lapply()`] (see `.cl`).
#' @details 
#' * [`dirs.create()`] is a simple wrapper around [`dir.create()`] that creates directories. Directories that already exist are skipped.
#' * [`dirs.copy()`] copies directories (including their contents) from `from` to `to`. 
#' @return
#' * [`dirs.create()`] returns `invisible(NULL)`;
#' * [`dirs.copy()`] returns a [`data.table`] with `from` and `to` columns plus `success` (`TRUE`/`FALSE`);
#' @author Edward Lavender
#' @name dirs

#' @rdname dirs
#' @export

dirs.create <- function(paths) {
  sapply(paths, function(path) {
    if (!dir.exists(path)) {
      dir.create(path, recursive = TRUE)
    }
  }) |> 
    suppressWarnings()
  nothing()
}

#' @rdname dirs
#' @export

dirs.copy <- function(from, to, cl) {
  
  # Assemble data.table and handle duplicates
  folders <- 
    data.table(from = from, to = to) |> 
    group_by(from) |> 
    slice(1L) |> 
    as.data.table()
  
  # Validate `from` folders exist
  stopifnot(all(dir.exists(folders$from)))
  
  # Validate `to` folders are empty 
  # * If not, old files may inappropriately persist 
  if (any(dir.exists(folders$to))) {
    msg("There are `to` directories that already exist. Use `unlink(to, recursive = FALSE)` to clean old files if required.")
    # unlink(folder$to, recursive = TRUE)
  }
  dirs.create(folders$to)
  
  # Iteratively copy folders & contents
  success <- 
    cl_lapply(split(folders, seq_row(folders)), 
              .cl = cl, 
              .fun = function(folder) {
                file.copy(folder$from, 
                          dirname(folder$to), 
                          recursive = TRUE, 
                          overwrite = TRUE)
              })
  
  # Return success
  folders[, success := unlist(success)]
  folders
  
}