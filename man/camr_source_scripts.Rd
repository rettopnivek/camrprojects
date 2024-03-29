% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/R02-Files_and_paths.R
\name{camr_source_scripts}
\alias{camr_source_scripts}
\title{Source in Multiple R Scripts in a Folder}
\usage{
camr_source_scripts(files_to_include = NULL, path = "R")
}
\arguments{
\item{files_to_include}{A vector of either...
\itemize{
\item Numeric indices denoting which files
to include;
\item A character string matching the initial
set of letters across all relevant files (e.g., if
all scripts of interest start with the letter 'S');
\item A character vector with the full file names
for the files to include.
}}

\item{path}{The folder name with the scripts to source.}
}
\description{
A convenience function that loops through
and reads in code in .R files stored in a
folder located in the current working directory.
}
\author{
Kevin Potter
}
