% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/R05-Statistical_tools.R
\name{camr_statistic}
\alias{camr_statistic}
\title{Compute Univariate Statistic}
\usage{
camr_statistic(
  x,
  f = length,
  include = NULL,
  exclude = NULL,
  na.rm = TRUE,
  default = NA,
  ...
)
}
\arguments{
\item{x}{A vector.}

\item{f}{A function that computes a univariate statistc
(e.g., \code{mean}, \code{length}).}

\item{include}{Optional logical vector of matching length
to \code{x} indicating obserations to include when computing
the statistic.}

\item{exclude}{Optional vector of types of observations in
\code{x} to exclude (exact matches).}

\item{na.rm}{Logical; if \code{TRUE}, removes \code{NA} values.}

\item{default}{The default value to return in the case of
missing data (i.e., no observation over which to compute a
statistic).}

\item{...}{Additional parameters for the function \code{f}.}
}
\value{
A univariate statistic, or the default value.
}
\description{
A function for flexible and robust computation of univariate
statistics over a vector of observations. Can be combined
with the \code{\link[dplyr:group_by]{dplyr::group_by()}} and
\code{\link[dplyr:summarise]{dplyr::summarise()}} functions from
the package 'dplyr'.
}
\examples{
# Load in base R data set
data( iris )

# Sepal length
sln <- iris$Sepal.Length
# Number of observations for sepal length
camr_statistics( sln )

# Mean sepal length
camr_statistics( sln, f = mean )
# Define custom function for formatted mean
f_x <- function(x) as.character( round( mean(x), 2 ) )
camr_statistics( sln, f = f_x )

# Petal length
pln <- iris$Petal.Length
# Conditional mean for sepal length when petal length < 3.5
camr_statistics( sln, f = f_x, include = pln < 3.5 )

# Species of iris
spc <- iris$Species
# Isolate species 'virginica'
vrg <- spc == 'virginica'
# No petal lengths less than 3.5 for virginica, so return default
camr_statistics(
  sln[vrg], f = f_x, include = pln[vrg] < 3.5, default = 'No obs'
)

# Compute percentage of species 'setosa'
f_p <- function(x) paste0( round( 100*mean(x == 'setosa') ), '\%' )
camr_statistics( spc, f = f_p )
# Exclude 'virginica'
camr_statistics( spc, f = f_p, exclude = 'virginica' )

}
\author{
Kevin Potter
}
