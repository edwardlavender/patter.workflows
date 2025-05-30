% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimate-coord-constructors.R
\name{estimate_coord-constructor}
\alias{estimate_coord-constructor}
\alias{constructor_coa}
\alias{constructor_rsp}
\title{Estimate coordinates: constructors}
\usage{
constructor_coa(.sim, .datasets, .verbose, ...)

constructor_rsp(.sim, .datasets, .verbose, ...)
}
\arguments{
\item{.sim, .datasets, .verbose, ...}{Arguments inherited from \code{\link[proj.lapply:cl_lapply_workflow]{proj.lapply::cl_lapply_workflow()}}:
\itemize{
\item \code{.sim} is a \code{\link[data.table:data.table]{data.table::data.table}} row;
\item \code{.datasets} is a named \code{list} of .datasets, generally used to pass iteration-specific arguments to a \code{constructor};
\item \code{...} are additional static arguments passed to \code{constructor} for an \code{algorithm} function;
\item \code{.verbose} controls user outputs;
}}
}
\value{
\code{constructor} functions return a named \code{list} of arguments for an algorithm (e.g., \code{\link[=estimate_coord_coa]{estimate_coord_coa()}}).
}
\description{
\verb{estimate_coord_*()} \code{constructor} functions construct a named \code{list} of arguments for an \verb{estimate_coord_*()} function.
}
\details{
The following \code{constructors} are built in to \code{\link{patter.workflows}}:
\itemize{
\item \code{\link[=constructor_coa]{constructor_coa()}} assembles a named list of arguments for \code{\link[=estimate_coord_coa]{estimate_coord_coa()}};
\item \code{\link[=constructor_rsp]{constructor_rsp()}} assembles a named list of arguments for \code{\link[=estimate_coord_rsp]{estimate_coord_rsp()}};
}

For \code{\link[=constructor_coa]{constructor_coa()}}:
\itemize{
\item \code{.sim} should include the following columns:
\itemize{
\item \code{unit_id}: an integer that defines the individual/block ID;
\item \code{delta_t}: the time interval over which to calculate centres of activity (see \code{\link[=estimate_coord_coa]{estimate_coord_coa()}});
}
\item \code{.datasets} should include the following elements:
\itemize{
\item \code{map}: A \code{\link[terra:SpatRaster-class]{terra::SpatRaster}} (see \code{\link[patter:coa]{patter::coa()}})
\item \code{detections_by_unit}: a \code{list} of acoustic detection \code{\link[data.table:data.table]{data.table::data.table}}s (see \code{\link[=estimate_coord_coa]{estimate_coord_coa()}}) for each \code{unit_id};
\item \code{moorings}: a \code{moorings} data.table (see \code{\link[=estimate_coord_coa]{estimate_coord_coa()}});
}
}

For \code{\link[=constructor_rsp]{constructor_rsp()}}:
\itemize{
\item \code{.sim} should include the following columns:
\itemize{
\item \code{unit_id}: see above;
\item \code{er.ad}: the \code{er.ad} parameter (see \code{\link[=estimate_coord_rsp]{estimate_coord_rsp()}});
}
\item \code{.datasets} should include the following elements:
\itemize{
\item \code{map} as above;
\item \code{detections_by_unit}: see above;
\item \code{moorings}: see above;
}
}

Inbuilt constructors are implemented as follows:
\itemize{
\item The dataset(s) for \code{.sim$unit_id} (e.g., for a specific individual/time window) are extracted from \code{.datasets} via \verb{get_dataset_*()} functions (e.g., \code{\link[=get_dataset_detections]{get_dataset_detections()}});
\item Supported parameters (e.g., \code{er.ad}) are extracted from \code{.sim} via \code{\link[=get_parameter]{get_parameter()}};
\item (Other parameters are left at default settings);
\item A named \code{list} of arguments, including .datasets and parameters, plus any arguments passed via \code{...}, for the relevant function (e.g., \code{\link[=estimate_coord_coa]{estimate_coord_coa()}}) is returned;
}

For more control, define a custom \code{constructor} function.

For \code{\link[=estimate_coord_particle]{estimate_coord_particle()}} constructors have to be defined manually, following the same function signature.
}
\author{
Edward Lavender
}
