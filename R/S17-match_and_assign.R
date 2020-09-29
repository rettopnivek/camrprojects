#' Assign New Values Based on Partial or Exact Matching
#'
#' Assigns new values based on partial or exact matches with
#' values from an input vector.
#'
#' @param x A vector of values to match over.
#' @param matches A list of values in \code{x} to match over
#' @param new_values A vector of new values to assign based on
#'   matches to elements from \code{matches} (vector must be
#'   of equivalent length to \code{matches}).
#' @param type The type of matching, either 'partial' or 'exact'
#'   (uses \code{grepl} or \code{\%in\%}, respectively).
#' @param default The default value to assign in the absence of
#'   a match.
#'
#' @return A new vector of equivalent length to \code{x}, with
#'   values assigned based on successful matches.
#'
#' @author Kevin Potter
#'
#' @examples
#' x <- c( 'Cat', 'Hat', 'Rat', 'Dog', 'Fog', 'Cog' )
#' # Partial matching
#' match_and_assign( x, list( 'at', 'og' ), c('A','B') )
#' # Exact matching
#' match_and_assign( x, list( 'Cat', c( 'Dog', 'Fog' ) ), c('A','B'),
#'                   default = '' )
#'
#' @export

match_and_assign <- function( x, matches, new_values, type = 'partial',
                              default = NA ) {

  # Number of observations
  No <- length( x )

  # Initialize output
  output <- rep( default, No )

  # Number of values/elements to match over
  Nm <- length( matches )

  # Check that vector with new values has
  # same length as values to match over
  if ( length( new_values ) != Nm ) {
    stop( paste0(
      "Length of argument 'new_values' must be equivalent to ",
      "argument 'matches'"
    ), call. = F )
  }

  # Loop over values and match
  for ( i in 1:Nm ) {

    is_match <- rep( F, No )

    if ( type == 'partial' ) {
      is_match <- grepl( matches[[i]], x, fixed = T )
    }

    if ( type == 'exact' ) {
      is_match <- x %in% matches[[i]]
    }

    output[ is_match ] <- new_values[i]

  }

  return( output )
}

