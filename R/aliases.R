# aliases.R
# * This script defines aliases for functions in suggested packages
# * This is a lightweight solution that avoids the need 
#   to import packages/functions required by a few routines

# dplyr
filter <- dplyr::filter
mutate <- dplyr::mutate

# ggplot2
aes                 <- ggplot2::aes
coord_sf            <- ggplot2::coord_sf
element_blank       <- ggplot2::element_blank
facet_wrap          <- ggplot2::facet_wrap
geom_point          <- ggplot2::geom_point
geom_raster         <- ggplot2::geom_raster
geom_sf             <- ggplot2::geom_sf
ggplot              <- ggplot2::ggplot
scale_fill_identity <- ggplot2::scale_fill_identity
theme               <- ggplot2::theme
theme_bw            <- ggplot2::theme_bw
xlab                <- ggplot2::xlab
ylab                <- ggplot2::ylab

# glue
glue <- glue::glue

# JuliaCall
julia_command <- JuliaCall::julia_command
julia_eval    <- JuliaCall::julia_eval

# spatial.extensions
spatNormalise <- spatial.extensions::spatNormalise

# proj.lapply
coffee <- proj.lapply::coffee