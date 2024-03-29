% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/R02-Files_and_paths.R
\name{camr_load_from_RData}
\alias{camr_load_from_RData}
\title{Load R Object From .RData File}
\usage{
camr_load_from_RData(path_to_RData_file, object_name)
}
\arguments{
\item{path_to_RData_file}{A character string, the
file path to the \code{.RData} file.}

\item{object_name}{A character string, the specific
R object to load from the \code{.RData} file.}
}
\value{
An R object loaded in from the \code{.RData} file.
}
\description{
Loads a specified R object in from a \code{.RData} file.
}
