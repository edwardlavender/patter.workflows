% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimate-map-algorithms.R
\name{estimate_map-algorithm}
\alias{estimate_map-algorithm}
\alias{estimate_map_pou}
\alias{estimate_map_dens}
\alias{estimate_map_dbbmm}
\title{Estimate maps: algorithms}
\usage{
estimate_map_pou(...)

estimate_map_dens(...)

estimate_map_dbbmm(map, ...)
}
\arguments{
\item{...}{Arguments for algorithms defined in other packages:
\itemize{
\item For \code{\link[=estimate_map_pou]{estimate_map_pou()}}, \code{...} is passed directly to \code{\link[patter:map_pou]{patter::map_pou()}};
\item For \code{\link[=estimate_map_dens]{estimate_map_dens()}}, \code{...} is passed to \code{\link[patter:map_dens]{patter::map_dens()}};
\item For \code{\link[=estimate_map_dbbmm]{estimate_map_dbbmm()}}, \code{...} is passed directly to \code{\link[RSP:dynBBMM]{RSP::dynBBMM()}};
}}

\item{map}{A \link[terra:SpatRaster-class]{terra::SpatRaster}.}
}
\value{
All functions return a \code{\link[terra:SpatRaster-class]{terra::SpatRaster}}.
}
\description{
These functions map estimated coordinates.
}
\details{
\itemize{
\item \code{estimate_map_pou(...)} is simply \code{map_pou(...)$ud}
\item \code{estimate_map_dens(...)} is simply \code{map_dens(...)$ud}
\item \code{estimate_map_dbbmm(...)} runs \code{RSP::dynBBMM(...)} and then:
\itemize{
\item Converts the UD to a \code{\link[terra:SpatRaster-class]{terra::SpatRaster}}
\item Aggregates individual UD layers, if required
\item Projects and resamples the aggregated UD onto \code{map}
\item Masks the result by \code{map} and normalises the distribution;
}
}
}
\author{
Edward Lavender
}
