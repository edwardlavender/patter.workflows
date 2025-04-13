#' @title Datasets
#' @description These functions load selected datasets. 
#' @details
#' * [`dat_gebco_tm()`] loads a `TransitionLayer` object from `actel::transitionLayer()`. This is predefined to streamline examples. 
#' @author Edward Lavender
#' @export

dat_gebco_tm <- function() {
  data <- system.file("extdata", "dat_gebco_tm.rds", 
                      package = "patter.workflows", mustWork = TRUE) 
  readRDS(data)
}
