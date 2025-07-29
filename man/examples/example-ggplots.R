library(data.table)
library(ggplot2)
library(patter)
library(proj.lapply)        # cl_lapply_workflow
library(spatial.extensions) # overwriteRaster

#### Set up example

# Define temporary directory
folder <- file.path(tempdir(), "ggmaps")
dir.create(folder)

# Define spatial datasets for example
map   <- dat_gebco()
coast <- dat_coast()
mpa   <- dat_mpa()
bb    <- terra::ext(map) - 3000
xlim <- bb[1:2]
ylim <- bb[3:4]

# Define example iteration dataset
iteration <- data.table(individual_id = c("A", "A", "A", "B", "B", "B"),
                        parameter_id = c(1, 2, 3, 1, 2, 3),
                        file_output = file.path(folder, paste0("ud-", 1:6, ".tif")))

# Run example workflow
# * For each unit, generate some maps
out <- cl_lapply_workflow(.iteration = iteration[c(1, 2, 3, 5, 6)],
                          .datasets = list(map = map),
                          .constructor = function(.sim, .datasets, .verbose, ...) {
                            .datasets
                          },
                          .algorithm = function(map) {
                            # This is a placeholder for a more complicated workflow
                            xy <- terra::spatSample(map, size = 50L,
                                                    xy = TRUE, na.rm = TRUE)
                            map_dens(.map = map, .coord = xy, .plot = FALSE)$ud
                          },
                          .write = overwriteRaster)

#### Examples

# Define mapdt
mapdt <- data.table(file_ud = iteration$file_output,
                    row     = iteration$individual_id,
                    column  = iteration$parameter_id)

# Plot map with default options
# * Note that panel four is blank
ggmaps(mapdt, 
       .map = map, .xlim = xlim, .ylim = ylim)

# Add coastline
# * Note that the coastline in panel four is transparent
# * (optional) Simplify coastline for improved speed
ggmaps(mapdt, 
       .map = map, .xlim = xlim, .ylim = ylim,
       .coast = coast, 
       .coast_tolerance = 10, .coast_mask = TRUE)

# Add polygon (e.g., MPA boundary)
# * Note that the coastline in panel four is transparent
ggmaps(mapdt,
       .map = map, # .xlim = xlim, .ylim = ylim,
       .coast = coast, .coast_mask = TRUE,
       .poly = mpa)

# Add points e.g., acoustic receivers
# * Note panel four
ggmaps(mapdt, 
       .map = map, .xlim = xlim, .ylim = ylim,
       .coast = coast, .coast_mask = TRUE,
       .moorings = dat_sim_moorings)

# Add movement path(s)
# a) Define example movement paths for a couple of rows/columns
paths <- rbind(
  data.table(row = "A", column = 1, 
             timestep = 1:3, 
             x = c(706864.4, 709374.7, 708224.2), 
             y = c(6250694, 6256238, 6264606)),
  data.table(row = "B", column = 2, 
             timestep = 1:3, 
             x = c(699333.5, 699281.2, 701373.1), 
             y = c(6268266, 6266645, 6267325)))
# b) Plot maps with movement paths
ggmaps(mapdt, 
       .map = map, .xlim = xlim, .ylim = ylim,
       .coast = coast, .coast_mask = TRUE,
       .moorings = dat_sim_moorings, 
       .path = paths)


proj.file::dir_cleanup(folder)
