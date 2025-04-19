#########################
#########################
#### add-data.R

#### Aims
# 1) Prepare datasets

#### Prerequisites
# 1) NA


#########################
#########################
#### Set up

#### Wipe workspace
rm(list = ls())

#### Essential packages
library(patter)
library(data.table)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE)


#########################
#########################
#### Simulate tracking datasets

# Connect to Julia
julia_connect()
set_seed()

# Define study timeline 
# * Use a one-week timeline for example speed
start <- as.POSIXct("2025-04-13 00:00:00", tz = "UTC")
end   <- as.POSIXct("2025-04-19 23:58:00", tz = "UTC")
timeline <- seq(start, end, by = "2 mins")
unique(lubridate::floor_date(timeline, "weeks"))

# Define study area 
map <- dat_gebco()
set_map(map)

# Simulate a dense receiver network
# * For examples (e.g., with RSP), we need plenty of detections 
dat_sim_moorings <- sim_array(.map = map, 
                              .timeline = timeline,
                              .arrangement = "regular",
                              .n_receiver = 100L)

# For two individuals, simulate a movement trajectory & acoustic observations
dat_sim_list <- 
  lapply(1:2, function(id) {
    
    # Simulate path 
    path <- sim_path_walk(.map = map, 
                          .timeline = timeline, 
                          .state = "StateXY", 
                          .model_move = model_move_xy()) |> 
      mutate(individual_id = id, .before = path_id) |> 
      as.data.table()
    
    # Simulate acoustic observations
    obs <- sim_observations(.timeline = timeline, 
                            .model_obs = model_obs_acoustic_logis_trunc(dat_sim_moorings))
    
    # Extract detections, following patter::dat_detections format
    detections <- 
      obs$ModelObsAcousticLogisTrunc[[1]] |> 
      filter(obs == 1L) |>
      mutate(individual_id = id) |> 
      select("individual_id", "timestamp", receiver_id = "sensor_id") |> 
      as.data.table()
    
    # Collect outputs
    list(path = path, detections = detections)
    
  }) 

# Examine paths data.table
dat_sim_paths <-
  lapply(dat_sim_list, \(elm) elm$path) |> 
  rbindlist()
head(dat_sim_paths)

# Examine detections data.table
dat_sim_detections <- 
  lapply(dat_sim_list, \(elm) elm$detections) |> 
  rbindlist()
head(dat_sim_detections)


#########################
#########################
#### Define ancillary datasets

# Define transition matrix for RSP::runRSP() examples
# * We predefine this to avoid Julia/terra issues with actel::transitionLayer()
# * in R CMD examples
dat_gebco_tm <-
  map |>
  terra::setValues(1) |>
  terra::mask(map) |>
  terra::project("WGS84") |>
  actel::transitionLayer(directions = 8L)

# Write to inst/
saveRDS(dat_gebco_tm, file.path("inst", "extdata", "dat_gebco_tm.rds"))
usethis::use_data(dat_sim_paths, overwrite = TRUE)
usethis::use_data(dat_sim_moorings, overwrite = TRUE)
usethis::use_data(dat_sim_detections, overwrite = TRUE)


#### End of code.
#########################
#########################