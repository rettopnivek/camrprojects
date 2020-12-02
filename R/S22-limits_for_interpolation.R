#' Function to Find Limits for Interpolation
#'
#' A function that, given a vector of x and y values,
#' identifies the lower and upper bounds to use for
#' linear interpolation.
#'
#' @param value The x or y-axis value to interpolate.
#' @param x A vector of numeric values for the x-axis
#'   (must match \code{y} in length).
#' @param y A vector of numeric values for the y-axis
#'   (must match \code{x} in length).
#' @param interp_y Logical; if \code{TRUE}, the value to
#'   interpolate is assumed to be a y coordinate.
#'
#' @author Kevin Potter
#'
#' @return A vector of 5 values, the lower x and y-axis values
#'   followed by the upper x and y-axis values that bracket the
#'   point to interpolate. If there are no coordinates bracketting
#'   the specified coordinate, a vector of \code{NA} values is returned.
#'
#' @examples
#' # Example curve
#' x = 0:5
#' y = c( 0, .5, 2, 4, 8, 16 )
#'
#' # Value between x-axis values 2 and 3
#' inp = limits_for_interp( 2.5, x, y )
#' linear_interp( inp )
#'
#' #' # Interpolate x-axis value
#' inp = limits_for_interp( 3, x, y, interp_y = F )
#' linear_interp( inp, interp_y = F )
#'
#' # Value at an existing point
#' inp = limits_for_interp( 3, x, y )
#' linear_interp( inp )
#'
#' # Value out of range
#' inp = limits_for_interp( 6, x, y )
#' linear_interp( inp )
#'
#'
#' @export

limits_for_interp = function( value, x, y,
                                     interp_y = T ) {

  # Check that time/outcome are actually aligned
  if ( length( x ) != length( y ) ) {
    stop( 'Vectors for x and y must match in length',
          call. = F )
  }

  # Initialize output
  out = rep( NA, 5 )

  # Identify non-missing values
  no_na =
    !is.na( x ) &
    !is.na( y )

  # if data present
  if ( any( no_na ) ) {

    # Remove missing values
    x = x[ no_na ]
    y = y[ no_na ]

    # Sort data
    o = order( x )
    x = x[ o ]
    y = y[ o ]
    n = length( x )

    # Identify cases above/below specified value
    vrb = x
    if ( !interp_y ) vrb = y

    # Check if value matches
    exact_match = value == vrb

    if ( any( exact_match ) ) {

      out[1] = x[exact_match]
      out[2] = y[exact_match]

      out[3] = x[exact_match]
      out[4] = y[exact_match]

      out[5] = value

      return( out )
    }

    sel_above = vrb > value
    sel_below = vrb < value

    if ( any( sel_above ) & any( sel_below ) ) {

      sel = max( which( sel_below ) )
      out[1] = x[sel]
      out[2] = y[sel]

      sel = min( which( sel_above ) )
      out[3] = x[sel]
      out[4] = y[sel]

      out[5] = value
    }

  }

  return( out )
}
