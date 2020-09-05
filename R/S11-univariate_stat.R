#' Compute Univariate Statistic
#'
#' A function for flexible and robust computation of univariate
#' statistics over a vector of observations. Can be combined
#' with the \code{\link[dplyr]{group_by}} and
#' \code{\link[dplyr]{summarise}} functions from
#' the package 'dplyr'.
#'
#' @param x A vector.
#' @param f A function that computes a univariate statistc
#'   (e.g., \code{mean}, \code{length}).
#' @param include Optional logical vector of matching length
#'   to \code{x} indicating obserations to include when computing
#'   the statistic.
#' @param exclude Optional vector of types of observations in
#'   \code{x} to exclude (exact matches).
#' @param na.rm Logical; if \code{TRUE}, removes \code{NA} values.
#' @param default The default value to return in the case of
#'   missing data (i.e., no observation over which to compute a
#'   statistic).
#' @param ... Additional parameters for the function \code{f}.
#'
#' @author Kevin Potter
#'
#' @return A univariate statistic, or the default value.
#'
#' @examples
#' # Load in base R data set
#' data( iris )
#'
#' # Sepal length
#' sln <- iris$Sepal.Length
#' # Number of observations for sepal length
#' univariate_stat( sln )
#'
#' # Mean sepal length
#' univariate_stat( sln, f = mean )
#' # Define custom function for formatted mean
#' f_x <- function(x) as.character( round( mean(x), 2 ) )
#' univariate_stat( sln, f = f_x )
#'
#' # Petal length
#' pln <- iris$Petal.Length
#' # Conditional mean for sepal length when petal length < 3.5
#' univariate_stat( sln, f = f_x, include = pln < 3.5 )
#'
#' # Species of iris
#' spc <- iris$Species
#' # Isolate species 'virginica'
#' vrg <- spc == 'virginica'
#' # No petal lengths less than 3.5 for virginica, so return default
#' univariate_stat( sln[vrg], f = f_x, include = pln[vrg] < 3.5, default = 'No obs' )
#'
#' # Compute percentage of species 'setosa'
#' f_p <- function(x) paste0( round( 100*mean(x == 'setosa') ), '%' )
#' univariate_stat( spc, f = f_p )
#' # Exclude 'virginica'
#' univariate_stat( spc, f = f_p, exclude = 'virginica' )
#'
#' @export

univariate_stat <- function( x,
                             f = length,
                             include = NULL,
                             exclude = NULL,
                             na.rm = T,
                             default = NA,
                             ... ) {

  # Initialize output
  out <- default

  # If there is any data
  if ( length( x ) > 0 ) {

    # If no logical vector is provided
    if ( is.null( include ) ) {
      include <- rep( T, length( x ) )
    }

    if ( !is.null( exclude ) ) {
      include <- include & !x %in% exclude
    }

    if ( na.rm ) {
      include <- include & !is.na( x )
    } else {
      include <- include | is.na( x )
    }

    # If any data remains, apply function
    if ( any( include ) ) {
      out <- f( x[include], ... )
    }

  }

  return( out )
}

