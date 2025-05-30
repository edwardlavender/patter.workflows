#### Example workflows
# Iterative workflow examples for coordinate estimation and mapping
# of tagged animals (in receiver arrays)

#### Load packages
# Standard data packages
library(data.table)
library(dplyr)
library(dtplyr, warn.conflicts = FALSE)
library(lubridate)
library(ggplot2)
library(glue)
# Spatial packages
# * We pro-actively load these to avoid R/Julia issues
library(sf)
library(terra)
library(spatial.extensions) # overwriteRaster()
# Animal-tracking packages
library(patter)
# Project/workflow management packages
library(proj.lapply) # cl_lapply_workflow
library(proj.file)   # dirs.create, dir_cleanup()

#### Load data
map        <- dat_gebco()
coast      <- dat_coast()
paths      <- dat_sim_paths
detections <- dat_sim_detections
moorings   <- dat_sim_moorings

#### Set options and global parameters
op <- options(terra.pal = terrain.colors(256L, rev = TRUE))
bb <- terra::ext(map) - 3000
xlim <- bb[1:2]
ylim <- bb[3:4]


#### Workflow set up -----------------------------------------------------####
#### ---------------------------------------------------------------------####

#### Set up Julia
julia_connect()
set_seed()
set_map(map)

#### Define study period
# * For speed, we focus on a one-week timeline 
start <- as.POSIXct("2025-04-13 00:00:00", tz = "UTC")
end   <- as.POSIXct("2025-04-19 23:58:00", tz = "UTC")

#### Define unitsets and observations
# We have simulated acoustic detections for an example species e.g. flapper skate
detections |>
  ggplot() + 
  geom_point(aes(timestamp, factor(individual_id)))
# We focus on observations within the study timeline
detections <- 
  detections |> 
  filter(timestamp %within% lubridate::interval(start, end)) |> 
  as.data.table()
# We define a data.table with individual/block groupings of interest
detections[, individual_id]
detections[, week_id := lubridate::floor_date(timestamp, "weeks")]
detections[, unit_id := .GRP, by = .(individual_id, week_id)]
unitsets <-
  detections |> 
  group_by(unit_id) |> 
  slice(1L) |> 
  ungroup() |>
  select(unit_id, individual_id, week_id) |> 
  as.data.table()
# Check the number of observations (detections) for each unit_id
detections |> 
  group_by(unit_id) |> 
  summarise(n = n())

#### Build directories
# For each unitset (individual/week), we will create an algorithm folder
# (in which we can optionally write outputs)
unitsets <- 
  unitsets |>
  mutate(folder_home = file.path(tempdir(), "data", "output", "sim", "runs",
                                 individual_id, week_id),
         folder_home_coa = file.path(folder_home, "coa"),
         folder_home_rsp = file.path(folder_home, "rsp"),
         folder_home_patter = file.path(folder_home, "patter")) |>
  as.data.table()
dirs.create(file.path(tempdir(), "data", "input", "sim"))
dirs.create(unitsets$folder_home_coa)
dirs.create(unitsets$folder_home_rsp)
dirs.create(unitsets$folder_home_patter)


#### ---------------------------------------------------------------------####
#### Iteratively estimate COAs -------------------------------------------####
#### ---------------------------------------------------------------------####

#### Prepare iteration dataset
# Define parameters (e.g., 'best-guess', 'restrictive', 'flexible')
# (We can use multiple settings to examine sensitivity)
pars <- data.table(sensitivity = c("best", "ac(-)", "ac(+)"), 
                   parameter_id = 1:3L, 
                   delta_t = c("2 hours", "1 hour", "4 hours"))
# Define iteration 
iteration <- 
  unitsets |> 
  select(unit_id, individual_id, week_id, folder_home = folder_home_coa) |>
  cross_join(pars) |> 
  mutate(index = row_number(),
         folder_coord = file.path(folder_home, "coord", parameter_id), 
         folder_dens = file.path(folder_home, "dens", parameter_id)) |> 
  as.data.table()
dirs.create(iteration$folder_coord)
dirs.create(iteration$folder_dens)

#### Compute coordinates and maps and write to file

## (A) Write coordinates to file
iteration[, file_coord := file.path(folder_coord, "coord.qs")]
iteration[, file_output := file_coord]
dcoord     <- list(map = map, 
                   moorings = moorings, 
                   detections_by_unit = split(detections, detections$unit_id))
coord_list <- cl_lapply_workflow(.iteration   = iteration,
                                 .datasets    = dcoord,
                                 .constructor = constructor_coa, 
                                 .algorithm   = estimate_coord_coa)
# Examine coordinates
list.files(iteration$folder_coord)
lapply_qplot_coord(iteration, .map = map)

## (B) Write maps to file
iteration[, file_ud := file.path(folder_dens, "map_dens.tif")]
iteration[, file_output := file_ud]
ddens     <- list(map = map, coord = coord_list)
dens_list <- cl_lapply_workflow(.iteration = iteration, 
                               .datasets = ddens, 
                               .constructor = constructor_map_dens, 
                               .algorithm = estimate_map_dens, 
                               .write = overwriteRaster)
# Examine maps
list.files(iteration$folder_dens)
lapply_qplot_ud(iteration)

# Record iteration
iteration_coa <- copy(iteration)


#### ---------------------------------------------------------------------####
#### Iteratively estimate RSPs -------------------------------------------####
#### ---------------------------------------------------------------------####

#### Define iteration dataset
# Define parameters
pars <- data.table(sensitivity = c("best", "ac(-)", "ac(+)"), 
                   parameter_id = 1:3L, 
                   er.ad = c(20, 10, 40))
# Define iteration
iteration <- 
  unitsets |> 
  select(unit_id, individual_id, week_id, folder_home = folder_home_rsp) |>
  cross_join(pars) |> 
  mutate(index = row_number(),
         folder_coord = file.path(folder_home, "coord", parameter_id), 
         folder_dens = file.path(folder_home, "dens", parameter_id)) |> 
  as.data.table()
dirs.create(iteration$folder_coord)
dirs.create(iteration$folder_dens)

#### Compute coordinates and maps and write to file

## (A) Estimate coordinates
iteration[, file_coord := file.path(folder_coord, "coord.qs")]
iteration[, file_output := file_coord]
dcoord <- list(map = map, 
               moorings = moorings, 
               detections_by_unit = split(detections, detections$unit_id))
coord_list <- cl_lapply_workflow(.iteration   = iteration,
                                 .datasets    = dcoord,
                                 .constructor = constructor_rsp, 
                                 .algorithm   = estimate_coord_rsp, 
                                 t.layer     = dat_gebco_tm())
# Examine coordinates
list.files(iteration$folder_coord)
lapply_qplot_coord(iteration, 
                   .datasets = list(coordinates = function(rsp) {
                     interp <- rsp[["detections"]][[1]]
                     cbind(interp$Longitude, interp$Latitude) |> 
                       terra::vect(crs = "WGS84") |> 
                       terra::project(map) |> 
                       terra::crds() |> 
                       data.table()
                   }), 
                   .map = map)

## (B) Write maps to file
iteration[, file_ud := file.path(folder_dens, "map_dens.tif")]
iteration[, file_output := file_ud]
map_ll   <- map |> terra::project("EPSG:4326")
water_ll <- terra::mask(terra::setValues(map_ll, TRUE), map_ll)
ddens     <- list(map = map, coord = coord_list)
dens_list <- cl_lapply_workflow(.iteration   = iteration,
                               .datasets    = ddens,
                               .constructor = constructor_map_dbbmm, 
                               .algorithm   = estimate_map_dbbmm, 
                               # 'static' arguments:
                               base.raster = water_ll, 
                               UTM = 29, 
                               .write = overwriteRaster)
# Examine maps
list.files(iteration$folder_dens)
lapply_qplot_ud(iteration)

# Record iteration
iteration_rsp <- copy(iteration)


#### ---------------------------------------------------------------------####
#### Iteratively sample particles ----------------------------------------####
#### ---------------------------------------------------------------------####

#### Define iteration dataset
# Define movement parameters (best, restrictive, flexible)
pars_movement <- data.table(k = c(1.0, 0.5, 2.0), 
                            theta = c(250.0, 125.0, 500.0), 
                            mobility = c(750.0, 375.0, 1500.0))
# Define detection parameters
pars_detection <- data.table(receiver_alpha = c(4, 4*0.9, 4*1.1),
                             receiver_beta  = c(-0.01, -0.01*1.1, -0.01*0.9),
                             receiver_gamma = c(750.0, -750.0*0.9, -750.0*1.1))
# Collect parameter combinations
pars <- 
  rbind(
    cbind(sensitivity = "best", pars_movement[1, ], pars_detection[1, ]),
    cbind(sensitivity = "move(-)", pars_movement[2, ], pars_detection[1, ]),
    cbind(sensitivity = "move(+)", pars_movement[3, ], pars_detection[1, ]),
    cbind(sensitivity = "ac(-)", pars_movement[1, ], pars_detection[2, ]),
    cbind(sensitivity = "ac(+)", pars_movement[1, ], pars_detection[3, ])
  ) |> 
  mutate(parameter_id = row_number()) |> 
  as.data.table()
# Define iteration 
iteration <- 
  unitsets |> 
  select(unit_id, individual_id, week_id, folder_home = folder_home_patter) |>
  cross_join(pars) |> 
  mutate(index = row_number(),
         folder_coord = file.path(folder_home, "coord", parameter_id), 
         folder_dens = file.path(folder_home, "dens", parameter_id)) |> 
  as.data.table()
dirs.create(iteration$folder_coord)
dirs.create(iteration$folder_dens)

#### Define custom estimate_coord constructor function 
constructor_ac <- function(.sim, .datasets, .verbose, ...) {
  
  stopifnot(length(list(...)) == 0L)
  
  # Define timeline
  # * We could create a timeline over the following week here (given weekly time blocks)
  # * As it is, we focus on a smaller timeline for example speed. 
  timeline <- seq(.sim$week_id, 
                  .sim$week + 2 * 24 * 60 * 60,
                  by = "2 mins")
  
  # Define movement model
  state <- "StateXY"
  model_move <- 
    model_move_xy(.mobility = .sim$mobility,
                  .dbn_length = glue("truncated(Gamma({.sim$k}, {.sim$theta}), 
                                      lower = 0.0, upper = {.sim$mobility})"),
                  .dbn_heading = "Uniform(-pi, pi)")
  
  # Assemble observations
  # (We could also read datasets for .sim$unit_id from file)
  moorings   <- get_dataset_moorings(.sim, .datasets)
  detections <- get_dataset_detections(.sim, .datasets)
  acoustics  <- assemble_acoustics(.timeline = timeline, 
                                   .detections = detections, 
                                   .moorings = moorings)
  yobs_fwd <- yobs_bwd <- list(ModelObsAcousticLogisTrunc = acoustics)
  if (length(which(acoustics$obs == 1L) > 2L)) {
    # Include containers if necessary
    containers <- assemble_acoustics_containers(.timeline = timeline, 
                                                .acoustics = acoustics, 
                                                .mobility = .sim$mobility, 
                                                .map = map)
    yobs_fwd$ModelObsContainer <- containers$forward
    yobs_bwd$ModelObsContainer <- containers$backward
  }
  
  # Define arguments for forward filter run
  # * Limit .n_particle for example speed only 
  args_fwd <- list(.timeline   = timeline, 
                   .state      = state,
                   .model_move = model_move, 
                   .yobs       = yobs_fwd, 
                   .n_particle = 1e4L, 
                   .direction  = "forward", 
                   .verbose    = .verbose)
  
  # Define arguments for backward filter run
  args_bwd            <- args_fwd
  args_bwd$.yobs      <- yobs_bwd
  args_bwd$.direction <- "backward"
  
  # Define smoother arguments
  # * Limit .n_sim and .n_particle for example speed only
  args_smo <- list(.n_sim = 30L, .n_particle = 100L)
  
  # Checks
  stopifnot(all(names(args_fwd) %in% names(formals(pf_filter))))
  stopifnot(all(names(args_bwd) %in% names(formals(pf_filter))))
  stopifnot(all(names(args_smo) %in% names(formals(pf_smoother_two_filter))))
  
  # Collate filter arguments
  # * particle_algorithm() requires the following arguments:
  # - `forward`
  # - `backward`  (if smoothing desired, NULL otherwise)
  # - `smooth`    (if smoothing desired, NULL otherwise)
  list(forward = args_fwd, backward = args_bwd, smooth = args_smo, verbose = .verbose)
  
}

## (A) Write coordinates to file
# Define datasets 
datasets <- list(map = map, 
                 moorings = moorings, 
                 detections_by_unit = split(detections, detections$unit_id))
# Select iterations 
iteration <- iteration[mobility == pars$mobility[1], ][1:5L, ]
iteration[, file_coord := file.path(folder_coord, "coord.qs")]
iteration[, file_output := file_coord]
set_vmap(.map = map, .mobility = pars$mobility[1])
# Estimate coordinates
# * NB we could specify `.trials` and `.success` here
coord_list <- 
  cl_lapply_workflow(.iteration   = iteration,
                     .datasets    = datasets,
                     .constructor = constructor_ac, 
                     .algorithm   = estimate_coord_particle)
# Examine coordinates
list.files(iteration$folder_coord)
lapply_qplot_coord(iteration, 
                   .datasets = list(coordinates = function(x) x$smooth$states),
                   .map = map)

## (B) Write maps to file
iteration[, file_ud := file.path(folder_dens, "map_dens.tif")]
iteration[, file_output := file_ud]
iteration <- iteration[file.exists(file_coord), ]
ddens      <- list(map = map, 
                  coord = coord_list, 
                  coordinates = function(x) x$smooth$states)
dens_list  <- cl_lapply_workflow(.iteration = iteration, 
                                .datasets = ddens, 
                                .constructor = constructor_map_dens, 
                                .algorithm = estimate_map_dens, 
                                .write = overwriteRaster)
# Examine maps
list.files(iteration$folder_dens)
lapply_qplot_ud(iteration)

# Record iteration
iteration_patter <- copy(iteration)


#### ---------------------------------------------------------------------####
#### Synthesis -----------------------------------------------------------####
#### ---------------------------------------------------------------------####

#### Plot simulated pattern of space use
pp <- par(mfrow = c(1, 2))
cl_lapply(split(paths, paths$individual_id), function(d) {
  ud <- map_dens(.map = map, .coord = d, .discretise = TRUE, .plot = FALSE)$ud 
  terra::plot(ud, legend = FALSE)
  patter:::add_sp_path(x = d$x, y = d$y, length = 0.005, lwd = 0.25)
})
par(pp)

#### Plot 'best-guess maps
# COA algorithm
iteration_coa |> 
  filter(sensitivity == "best") |> 
  select(row = individual_id, column = week_id, file_ud) |> 
  ggmaps(.map = map, 
         .coast = coast, .coast_mask = TRUE, 
         .moorings = moorings,
         .xlim = xlim, .ylim = ylim)
# RSP algorithm
iteration_rsp |> 
  filter(sensitivity == "best") |> 
  select(row = individual_id, column = week_id, file_ud) |> 
  ggmaps(.map = map, 
         .coast = coast, .coast_mask = TRUE,
         .moorings = moorings,
         .xlim = xlim, .ylim = ylim)
# Particle algorithms
# * NB: Note for speed above we did not model the whole timeline  
iteration_patter |> 
  filter(sensitivity == "best") |> 
  select(row = individual_id, column = week_id, file_ud) |> 
  ggmaps(.map = map, 
         .coast = coast, .coast_mask = TRUE, 
         .moorings = moorings,
         .xlim = xlim, .ylim = ylim)

#### Plot sensitivity analyses
# We leave this as an exercise for the reader!


dir_cleanup(unitsets$folder_home)
options(op)


#### ---------------------------------------------------------------------####
#### End -----------------------------------------------------------------####
#### ---------------------------------------------------------------------####