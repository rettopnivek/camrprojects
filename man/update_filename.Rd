% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/S18-update_filename.R
\name{update_filename}
\alias{update_filename}
\title{Create Standardized Filename and Remove Previous File}
\usage{
update_filename(
  description,
  extension,
  tag = NULL,
  number = NULL,
  file_date = NULL,
  project_version = NULL
)
}
\arguments{
\item{description}{A human-readable brief description of
the file content.}

\item{extension}{The file extension (e.g., 'docx', 'RData').}

\item{tag}{A single upper-case letter for file organization.}

\item{number}{A character string with a two-digit number
for file organization.}

\item{file_date}{A character string with the date
in the format 'mm_dd_yyyy'.}

\item{project_version}{The git version for the project
in the format 'v.X.X.X'.}
}
\value{
A character string, and deletes previous versions of
the file.
}
\description{
Generates a standardized file name following the format
TXX-description-dd_mm_yyyy-v.X.X.X.extension, giving
a file tag and counter (e.g., 'F01', 'D02'), a
human-readable description and date for version control,
a machine-readable version number, and the file
extension. Furthermore, checks if a previous version of
the file (i.e., with earlier dates or project versions)
exists in the current directory - if it does, removes this
file to avoid duplicate files.
}
\author{
Kevin Potter
}
