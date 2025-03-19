#' @title Package conversion routines
#' @description This routines convert [`patter`] [`data.table`]s into formats used by other packages, such as `actel` (`RSP`) and `glatos`.
#' @param detections,moorings datasets
#' @param map SpatRaster
#' @details
#' * Convert patter to glatos format
#' * Convert `detections` [`data.table`] to `actel` format (for RSP)
#' @name as_

#' @rdname as_
#' @export

as_actel <- function(map, detections, moorings) {
  
  #### Get data
  detections <- as.data.frame(detections)
  moorings   <- as.data.frame(moorings)
  check_names(input = moorings, req = "receiver_gamma")
  
  #### Prepare actel datasets
  # Biometrics
  act_bio <-
    data.frame(Release.date = detections$timestamp[1] - 1,
               Release.site = "unspecified",
               Group = 1,
               Signal = 1L)
  # Deployments
  act_deployments <-
    moorings |>
    mutate(Start = paste(.data$receiver_start, "00:00"),
           Stop = paste(.data$receiver_end + 1, "00:00")) |>
    dplyr::select(Receiver = "receiver_id",
                  Station.name = "receiver_id",
                  "Start", "Stop")
  # Spatial datasets
  moorings_ll <-
    moorings |>
    dplyr::select("receiver_x", "receiver_y") |>
    as.matrix() |>
    terra::vect(crs = terra::crs(map)) |>
    terra::project("EPSG: 4326") |>
    terra::crds()
  moorings$lon <- moorings_ll[, 1]
  moorings$lat <- moorings_ll[, 2]
  act_spatial <-
    moorings |>
    mutate(
      Longitude = .data$lon, Latitude = .data$lat,
      x = .data$receiver_x, y = .data$receiver_y,
      Array = "A0", Section = "unspecified",
      Type = "Hydrophone", 
      Range = moorings$receiver_gamma) |>
    dplyr::select(Station.name = "receiver_id",
                  "Longitude", "Latitude", "x", "y",
                  "Array", "Section", "Type", "Range")
  # Detections
  act_detections <-
    detections |>
    mutate(Receiver = .data$receiver_id,
           Timestamp = .data$timestamp,
           CodeSpace = "unspecified",
           Signal = 1L) |>
    dplyr::select("Receiver", "Timestamp", "CodeSpace", "Signal")
  
  #### Run actel::explore() non-interactively
  # Collate datasets
  input     <- list(bio = act_bio, 
                    spatial = act_spatial, 
                    deployments = act_deployments, 
                    detections = act_detections)
  input.rds <- tempfile(fileext = ".rds")
  input.rds <- normalizePath(input.rds, winslash = "/", mustWork = FALSE)
  on.exit(unlink(input.rds), add = TRUE)
  saveRDS(input, input.rds)
  output.rds <- tempfile(fileext = ".rds")
  output.rds <- normalizePath(output.rds, winslash = "/", mustWork = FALSE)
  on.exit(unlink(output.rds), add = TRUE)
  # Build script
  actel.R <- tempfile(fileext = ".R")
  on.exit(unlink(actel.R), add = TRUE)
  actel.code <- 
    glue::glue(
      '
    # Load input datasets
    input <- readRDS("{input.rds}")
    # Preload input datasets
    act <- actel::preload(biometrics = input$bio, 
                          spatial = input$spatial,
                          deployments = input$deployments, 
                          detections = input$detections,
                          tz = "UTC") |> 
          suppressMessages() |>
          suppressWarnings()
    # Explore, non-interactively
    act <- actel::explore(act, tz = "UTC", GUI = "never") |> 
      suppressMessages() |> 
      suppressWarnings()
    saveRDS(act, "{output.rds}")
    invisible(NULL)
    '
    )
  writeLines(actel.code, actel.R)
  system(paste("Rscript", actel.R)) 
  readRDS(output.rds)
  
}

#' @rdname as_
#' @export

as_glatos <- function(detections) {
  data.frame(detection_timestamp_utc = detections$timestamp, 
             transmitter_codespace = "000",
             transmitter_id = as.character(detections$individual_id), 
             receiver_sn = as.character(detections$receiver_id)
  )
}