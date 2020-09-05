#' Extract Unique Elements of a Vector
#'
#' A convenience function that extracts the unique elements
#' of a vector, after optionally excluding/keeping specified
#' observations. The function can summarize the number of
#' unique values, or return the unique values as a vector.
#'
#' @param x A vector.
#' @param counts Logical; if \code{TRUE} returns a count of the number
#'   of unique values, otherwise returns a vector with the unique values.
#' @param include Optional logical vector of matching length to \code{x}
#'   indicating obserations to keep.
#' @param exclude Optional vector of types of observations to exclude
#'   (exact matches).
#' @param na.rm Logical; if \code{TRUE}, removes \code{NA} values.
#'
#' @author Kevin Potter
#'
#' @return A count of the number of unique values or a vector
#' of the unique values.
#'
#' @examples
#' vec <- c( rep( c( 'A', 'B', 'C', 'D' ), each = 3 ), NA )
#' # Number of unique elements - NA excluded by default
#' elements( vec )
#' # Unique elments as vector
#' elements( vec, count = F )
#' # Keep NA
#' elements( vec, na.rm = F )
#' elements( vec, na.rm = F, count = F )
#' # Exclude specific values
#' elements( vec, exclude = c( 'A', 'B' ) )
#' elements( vec, exclude = c( 'A', 'B' ), count = F )
#' elements( vec, exclude = c( 'A', 'B' ), count = F, na.rm = F )
#'
#' @export

elements = function( x, counts = T,
                     include = NULL, exclude = '',
                     na.rm = T ) {

  # Number of observations
  n = length( x )

  # By default, include all observations
  if ( is.null( include ) ) {
    include = rep( T, n )
  }

  # Identify observations to exclude
  if ( !is.null( exclude ) ) {

    to_exclude = x %in% exclude

    if ( na.rm ) {
      to_exclude[ is.na( to_exclude ) ] = T
    } else {
      to_exclude[ is.na( to_exclude ) ] = F
    }
  } else {
    to_exclude = rep( F, n )
  }

  # If specified, identify missing data
  if ( na.rm ) {
    is_na = is.na( x )
  } else {
    is_na = rep( F, n )
  }

  entries =
    include &
    !to_exclude &
    !is_na

  if ( counts ) {
    out = length( unique( x[ entries ] ) )
  } else {
    out = unique( x[ entries ] )
  }

  return( out )
}
