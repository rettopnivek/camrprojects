#' Function for Linear Interpolation
#'
#' Given a pair of x and y values, uses linear
#' interpolation to compute a new x or y value.
#'
#' @param vec A vector of 5 values, consisting of...
#' \describe{
#'   \item{x0}{The x coordinate for the lower boundary;}
#'   \item{y0}{The y coordinate for the lower boundary;}
#'   \item{x1}{The x coordinate for the upper boundary;}
#'   \item{y1}{The y coordinate for the upper boundary;}
#'   \item{x or y}{The coordinate at which interpolation should occur.}
#' }
#'   A named vector can be provided, otherwise the order is assumed to
#'   follow the list given above.
#' @param interp_y Logical; if \code{TRUE}, the value to
#'   interpolate is assumed to be a y coordinate.
#'
#' @author Kevin Potter
#'
#' @return The interpolated x or y value. If outside the lower or upper
#'   boundaries, \code{NA} is returned instead.
#'
#' @examples
#' # Linear interpolation for y
#' linear_interp( c( x0 = 0, y0 = 0, x1 = 1, y1 = 2, x = .5 ) )
#' # Linear interpolation for x
#' linear_interp( c( x0 = 0, y0 = 0, x1 = 1, y1 = 2, y = .5 ), FALSE )
#' # Linear interpolation across multiple values
#' x = c( 0, 1, 2 )
#' y = c( 0, 2, 4 )
#' # Create matrix with 5th column for points to interpolate at
#' m = cbind( x[-3], y[-3], x[-1], y[-1], c( .5, 1.5 ) )
#' # Linear interpolation for y values
#' apply( m, 1, linear_interp )
#' # Linear interpolation for x values (NA for value outside boundary)
#' apply( m, 1, linear_interp, interp_y = F )
#'
#' @export

linear_interp <- function( vec, interp_y = T ) {

  vec <- as.vector( vec )

  if ( length( vec ) != 5 ) {
    stop( paste0(
      "Must provide 5 values, 1-2) the lower boundaries for x and y, ",
      "3-4) the upper boundaries for x and y, and 5) the x or y ",
      "at which to interpolate"
      ), call. = F )
  }

  check <- is.null( names(vec) )
  if ( !check ) {
    check <- !all( names( vec ) %in% c( 'x0', 'y0', 'x1', 'y1', 'x', 'y' ) )
  }

  if ( check ) {
    val_to_interp <- 'y'; if ( interp_y ) val_to_interp <- 'x'
    names( vec ) <- c( 'x0', 'y0', 'x1', 'y1', val_to_interp )
  }

  # If any missing data
  if ( any( is.na( vec ) ) ) {
    return( NA )
  }

  x0 <- vec['x0']
  y0 <- vec['y0']
  x1 <- vec['x1']
  y1 <- vec['y1']

  if ( interp_y ) {
    x <- vec['x']
  } else {
    y <- vec['y']
  }

  X = matrix( 1, 2, 2 )
  X[1,2] <- x0
  X[2,2] <- x1
  Y = matrix( NA, 2, 1 )
  Y[1,1] <- y0
  Y[2,1] <- y1

  # Check for matching limits
  if ( x0 == x1 & y0 == y1 ) {
    if ( interp_y ) {
      out <- y0; names( out ) <- 'y'
    } else {
      out <- x0; names( out ) <- 'x'
    }
    return( out )
  }

  # Intercept and slope
  tX <- t(X)
  tXX <- tX %*% X
  B <- solve( tXX ) %*% tX %*% Y

  # Linear interpolation based on
  # whether x or y value was provided
  out = NA
  if ( interp_y ) {
    # If x is between x0 and x1
    if ( x >= x0 & x <= x1 ) {
      out <- B[1,1] + B[2,1]*x
      names( out ) <- 'y'
    }
  } else {
    # If y is between y0 and y1
    if ( y >= y0 & y <= y1 ) {
      out <- (y - B[1,1])/B[2,1]
      names( out ) <- 'x'
    }
  }

  return( out )
}
