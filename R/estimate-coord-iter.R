#' @title Estimate coordinates
#' @description This function iteratively estimates coordinates using a user-defined algorithm function, such as [`algorithm_coa()`], [`algorithm_rsp()`] or [`algorithm_particle()`].
#' @param iteration An iteration [`data.table`]. 
#' * Each row corresponds to one algorithm run and is passed to `constructor()`.
#' * Required columns depend on the `constructor()` function but generally include:
#'    - `index` (essential): a unique identifier for each row;
#'    - `unit_id` (generally required): a unique identifer for each unit (e.g., individual/time block);
#'    -  run-specific parameters, such as:
#'        - `delta_t` for [`constructor_coa()`];
#'        - `er.ad` for [`constructor_rsp()`];
#'    - `folder_coord` (optional): a `character` path to a folder in which to write outputs;
#' @param map A map that defines the study area, passed to `constructor()`:
#' * `map` is a typically a [`SpatRaster`];
#' * `map` may be another input, such as a `character` file path, if supported by `constructor()`;
#' @param datasets (optional) A named `list` of datasets, passed to `constructor()`. 
#' @param constructor A [`constructor`] `function` that constructs a named `list` of arguments for the `algorithm` function, given the `iteration` row, `map`, `datasets` and `verbose`: 
#' * For [`algorithm_coa()`], the default constructor is [`constructor_coa()`];
#' * For [`algorithm_rsp()`], the default constructor is [`constructor_rsp()`];
#' * For [`algorithm_particle()`], a user-defined constructor is required;
#' 
#' @param algorithm An `algorithm_*()` function that estimates coordinates, such as:
#' * [`algorithm_coa()`] for the COA algorithm;
#' * [`algorithm_rsp()`] for the RSP algorithm;
#' * [`algorithm_particle()`] for a particle algorithm;
#' 
#' @param log.coffee,log.txt (optional) Log options. 
#' * `log.coffee` is a `character` that defines the directory in which to record the timing of 'coffee breaks', during which time the computer is given a rest (see [`coffee()`]). Use `NULL` to suppress coffee breaks. 
#' * If `verbose = TRUE`, `log.txt` is a `character` that defines a path to a `log.txt` file in which output user messages are recorded. 
#' @param verbose User output controls. 
#' * `verbose` is a `logical` variable that defines whether or not to output user messages to the console or `log.txt` if specified. 
#' 
#' @details
#' [`lapply_estimate_coord()`] iterates over rows in `iteration` via [`cl_lapply()`] and applies the internal function [`estimate_coord()`]. [`estimate_coord()`] function accepts the `iteration` row, `map`, `datasets`, `constructor`, `algorithm` and `verbose` arguments. Using these inputs, [`estimate_coord()`]:
#' * Constructs a named `list` of arguments for an algorithm function, via a [`constructor`], given information in the supplied arguments;
#' * Passes the arguments to an algorithm function, such as:
#'    * [`algorithm_coa()`] for the COA algorithm;
#'    * [`algorithm_rsp()`] for the RSP algorithm;
#'    * [`algorithm_particle()`] for a particle algorithm;
#' * Implements error handling;
#' * Records estimated coordinates and call statistics (such as computation time);
#' * Optionally writes outputs to disk or returns them in memory;
#' 
#' @return The function returns a `list`, with one element for each `iteration` row. `List` elements are controlled by [`estimate_coord()`]:

#' * By default, [`estimate_coord()`] returns a named `list` with two elements:
#'    * `coord`: The object returned by the `algorithm()` function (e.g., a [`data.table`] of coordinates);
#'    * `callstats`: A one-row [`data.table`] of call statistics with the following columns:
#'        * `id`: An integer that defines the row index in `iteration` (`iteration$index`);
#'        * `algorithm`: A `character` label for the algorithm, defined by `deparse(substitute(algorithm))`;
#'        * `success`: A `logical` variable that defines whether or not `algorithm` ran successfully (without errors);
#'        * `error`: A `character` that defines error messages or `NA_character_` otherwise;
#'        * `ntrial`: An `integer` that defines the number of trials, if relevant; 
#'        * `time`: A `double` that defines the time (s) of the `algorithm` run;
#'        
#' * If `iteration` contains a `folder_coord` column, [`estimate_coord()`] returns `invisible(NULL)`. Outputs are instead written to file (reducing memory demand), as:
#'    * `file.path(folder_coord, "coord.qs")`
#'    * `file.path(folder_coord, "callstats.qs")`
#'
#' @author Edward Lavender
#' @name lapply_estimate_coord

#' @rdname lapply_estimate_coord
#' @export

lapply_estimate_coord <- function(iteration, 
                                  map,
                                  datasets, 
                                  constructor,
                                  algorithm,  
                                  log.coffee = NULL,
                                  log.txt = NULL, 
                                  verbose = TRUE
                                  ) {
  
  # Boilerplate  
  t1   <- Sys.time()
  log.txt <- sink_open(log.txt = log.txt)
  cats <- cat_setup(.fun = "lapply_estimate_coord", .verbose = verbose)
  on.exit(eval(cats$exit, envir = cats$envir), add = TRUE)
  on.exit(sink_close(log.txt), add = TRUE)
  check_dir_empty(log.coffee)
  
  # Estimate coordinates via algorithm()
  iteration <- copy(iteration)
  if (!rlang::has_name(iteration, "index")) {
    index <- NULL
    iteration[, index := 1:.N]
  }
  iteration_ls <- split(iteration, seq_row(iteration))
  out <- cl_lapply(iteration_ls, function(sim) {
    estimate_coord(sim         = sim, 
                   map         = map,
                   datasets    = datasets, 
                   constructor = constructor, 
                   algorithm   = algorithm, 
                   verbose     = verbose)
  })
  
  out
  
}
