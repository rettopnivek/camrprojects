% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/R02-Files_and_paths.R
\name{camr_build_path}
\alias{camr_build_path}
\title{Build a File/Directory Path
Optionally create a directory or apply \code{camr_name_file} to
the generated file path. Use shorthand "root" directories as
specified in the project \code{config.yml}.}
\usage{
camr_build_path(
  ...,
  root = NULL,
  lgl_verify = TRUE,
  lgl_create = FALSE,
  lgl_name = FALSE
)
}
\arguments{
\item{...}{Arguments to be passed to file.path.}

\item{lgl_verify}{Verify that the path already exists?}

\item{lgl_create}{Create a directory at the path if it does not exist?}

\item{lgl_name}{Apply \code{camr_name_file} on the last argument?}

\item{chr_root}{Directory named in \code{local-directories} of the
project's \code{config.yml}.}
}
\value{
(Character) A vector of paths.
}
\description{
Build a File/Directory Path
Optionally create a directory or apply \code{camr_name_file} to
the generated file path. Use shorthand "root" directories as
specified in the project \code{config.yml}.
}
\examples{
\dontrun{
write.csv(
  iris, camr_build_path('data', 'Example.csv', lgl_name=T, lgl_verify=F)
)

camr_build_path(root='dropbox', 'fNIRS', 'Source')
}
}
