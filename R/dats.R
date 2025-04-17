#' @title Datasets
#' @description This page describes package datasets.
#' @details
#' * [`dat_sim_moorings`] is a [`data.table::data.table`] of simulated receiver locations (see [`patter::dat_moorings`]);
#' * [`dat_sim_detections`]is a [`data.table::data.table`] of simulated detections (see [`patter::dat_moorings`]);
#' * [`dat_gebco_tm()`] loads a `TransitionLayer` object from [`actel::transitionLayer()`]. This is predefined to streamline examples. 
#' @author Edward Lavender
#' @name dats

#' @rdname dats
"dat_sim_moorings"

#' @rdname dats
"dat_sim_detections"

#' @rdname dats
#' @export

dat_gebco_tm <- function() {
  data <- system.file("extdata", "dat_gebco_tm.rds", 
                      package = "patter.workflows", mustWork = TRUE) 
  readRDS(data)
}
