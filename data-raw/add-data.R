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


#########################
#########################
#### Define datasets

# Define map
map <- dat_gebco()

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


#### End of code.
#########################
#########################