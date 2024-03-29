% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/R04-Data_wrangling.R
\name{camr_column}
\alias{camr_column}
\title{Extract Column Names Meeting Inclusion/Exclusion Criteria}
\usage{
camr_column(dtf, ...)
}
\arguments{
\item{dtf}{A data frame.}

\item{...}{Character strings with the sub-strings to match
(or exclude) against the column names in \code{dtf}.
If an entry starts with either \code{!}, \code{~}, or
\code{-}, any columns containing the substring will be
excluded. Otherwise, the function will locate
all column names containing all inputted sub-strings.}
}
\value{
A vector of column names meeting the inclusion
and exclusion criteria.
}
\description{
A function that matches or excludes column names in a
data frame based on user-supplied sub-strings.
}
\examples{
data( example_CAM_data_set )

# All variables containing 'SSS'
example_CAM_data_set |> camr_column( 'SSS' )

# All variables containing both 'SSS' and 'CHR'
example_CAM_data_set |> camr_column( 'SSS', 'CHR' )

# Variables containing 'SSS' but not 'CHR'
example_CAM_data_set |> camr_column( 'INV', '~INT' )

}
\author{
Kevin Potter
}
