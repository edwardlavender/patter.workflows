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

# Define mapdt
mapdt <- data.table(file_ud = iteration$file_output,
                    row     = iteration$individual_id,
                    column  = iteration$parameter_id)

#### Example (1): Plot map with default options
# Note that panel four is blank
ggmaps(mapdt, 
       .map = map, .xlim = xlim, .ylim = ylim)

#### Example (2): Add coastline
# * Note that the coastline in panel four is transparent
# * (optional) Simplify coastline for improved speed
ggmaps(mapdt, 
       .map = map, .xlim = xlim, .ylim = ylim,
       .coast = coast, 
       .coast_tolerance = 10, .coast_mask = TRUE)
# Adjust the coastline colour (fill) by specifying coast$col and coast$alpha
coast_green       <- coast
coast_green$col   <- "darkgreen"
coast_green$alpha <- 0.5
ggmaps(mapdt, 
       .map = map, .xlim = xlim, .ylim = ylim,
       .coast = coast_green, 
       .coast_tolerance = 10, .coast_mask = TRUE)
# Customise other options via .geom_coast
ggmaps(mapdt, 
       .map = map, .xlim = xlim, .ylim = ylim,
       .coast = coast_green, 
       .coast_tolerance = 10, .coast_mask = TRUE, 
       .geom_coast = list(linewidth = 0))
# To completely hide the coastline on blank panels, set:
# `.trans_coast = 0` & `linewidth = 0`
ggmaps(mapdt, 
       .map = map, .xlim = xlim, .ylim = ylim,
       .coast = coast_green, 
       .coast_tolerance = 10, .coast_mask = TRUE, 
       .trans_coast = 0,
       .geom_coast = list(linewidth = 0))

#### Example (3): Add polygon (e.g., MPA boundary)
# Note that the coastline in panel four is transparent
# Colours are specified via poly$col, poly$alpha as for coast
# Other options are specified via .geom_poly
ggmaps(mapdt,
       .map = map, .xlim = xlim, .ylim = ylim,
       .coast = coast, .coast_mask = TRUE,
       .poly = mpa, 
       .geom_poly = list(linewidth = 2))
# Hide the polygon on blank plots:
ggmaps(mapdt,
       .map = map, .xlim = xlim, .ylim = ylim,
       .coast = coast, .coast_mask = TRUE,
       .poly = mpa, 
       .trans_coast = 0,
       .geom_coast = list(linewidth = 0),
       .trans_poly = 0,
       .geom_poly = list(linewidth = 2))
# Add individual-specific polygons
# * Here, we compute individual-specific home ranges
# * We collect them in a MULTIPOLYGON with 'row' and 'column' columns
# * Then we plot the polygons as usual via `.poly`
polys <- 
  lapply(split(iteration, seq_len(nrow(iteration))), function(d) {
    if (file.exists(d$file_output)) {
      # Load raster & compute home range
      r <- terra::rast(d$file_output)
      poly <- patter::map_hr_home(r)
      poly <- terra::as.polygons(poly == 1)
      poly <- poly[poly[[1]] == 1]
      poly <- poly |> sf::st_as_sf()
      # Add 'row' and 'column' columns
      poly$row    <- d$individual_id
      poly$column <- d$parameter_id
      poly
    }
})
polys <- do.call(rbind, polys)
ggmaps(mapdt,
       .map = map, .xlim = xlim, .ylim = ylim,
       .coast = coast, .coast_mask = TRUE,
       .poly = polys, 
       .trans_coast = 0,
       .geom_coast = list(linewidth = 0))

#### Example (4) Add points e.g., acoustic receivers
# Note receivers are not added on blank panels
ggmaps(mapdt, 
       .map = map, .xlim = xlim, .ylim = ylim,
       .coast = coast, .coast_mask = TRUE,
       .moorings = dat_sim_moorings)
ggmaps(mapdt, 
       .map = map, .xlim = xlim, .ylim = ylim,
       .coast = coast, .coast_mask = TRUE,
       .moorings = dat_sim_moorings, 
       .geom_moorings = list(col = "red", shape = 4, size = 5))

#### Example (5) Add movement path(s)
# Define example movement paths for a couple of rows/columns
# (We assume there are no paths for blank panels)
paths <- rbind(
  data.table(row = "A", column = 1, 
             timestep = 1:3, 
             x = c(706864.4, 709374.7, 708224.2), 
             y = c(6250694, 6256238, 6264606)),
  data.table(row = "B", column = 2, 
             timestep = 1:3, 
             x = c(699333.5, 699281.2, 701373.1), 
             y = c(6268266, 6266645, 6267325)))
# Plot maps with movement paths
ggmaps(mapdt, 
       .map = map, .xlim = xlim, .ylim = ylim,
       .coast = coast, .coast_mask = TRUE,
       .moorings = dat_sim_moorings, 
       .path = paths)
# Update path colour scheme via .scale_path
ggmaps(mapdt, 
       .map = map, .xlim = xlim, .ylim = ylim,
       .coast = coast, .coast_mask = TRUE,
       .moorings = dat_sim_moorings, 
       .path = paths, 
       .scale_path = scale_colour_gradientn(colours = rainbow(100)))
# And/or customise via .geom_path
ggmaps(mapdt, 
       .map = map, .xlim = xlim, .ylim = ylim,
       .coast = coast, .coast_mask = TRUE,
       .moorings = dat_sim_moorings, 
       .path = paths, 
       .geom_path = list(linewidth = 1, arrow = grid::arrow(length = unit(0.2, "cm"))))

proj.file::dir_cleanup(folder)
