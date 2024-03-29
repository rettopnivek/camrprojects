# Statistical tools
# Written by
#   Kevin Potter
#   William Schmitt
# Maintained by...
#   Kevin Potter
# Email:
#   kpotter5@mgh.harvard.edu
# Please email us directly if you
# have any questions or comments
# Last updated: 2022-09-08

# Table of contents
# 1) camr_limits_for_interpolation
# 2) camr_linear_interpolation
# 3) camr_build_demographics_table
# 4) camr_statistics

#### 1) camr_limits_for_interpolation ####
#' Function to Find Limits for Interpolation
#'
#' A function that, given a vector of x and y values,
#' identifies the lower and upper bounds to use for
#' linear interpolation.
#'
#' @param value The x or y-axis value to interpolate.
#' @param x A vector of numeric values for the x-axis
#'   (must match `y` in length).
#' @param y A vector of numeric values for the y-axis
#'   (must match `x` in length).
#' @param interp_y Logical; if `TRUE`, the value to
#'   interpolate is assumed to be a y coordinate.
#' @param use_first_last Logical; if `TRUE` when values
#'   fall outside the provided vectors the function uses the
#'   first or last set of x and y values as an approximation.
#' @param warn_if_first_last Logical; if `TRUE` warns
#'   the user when an approximation is used for cases that
#'   fall outside the range of the provided vectors.
#'
#' @author Kevin Potter
#'
#' @return A vector of 5 values, the lower x and y-axis values
#'   followed by the upper x and y-axis values that bracket the
#'   point to interpolate. If there are no coordinates bracketting
#'   the specified coordinate, a vector of `NA` values is returned.
#'
#' @examples
#' # Example curve
#' x = 0:5
#' y = c( 0, .5, 2, 4, 8, 16 )
#'
#' # Value between x-axis values 2 and 3
#' inp = camr_limits_for_interpolation( 2.5, x, y )
#' camr_linear_interpolation( inp )
#'
#' # Interpolate x-axis value
#' inp = camr_limits_for_interpolation( 3, x, y, interp_y = F )
#' camr_linear_interpolation( inp, interp_y = F )
#'
#' # Value at an existing point
#' inp = camr_limits_for_interpolation( 3, x, y )
#' camr_linear_interpolation( inp )
#'
#' # Value out of range
#' inp = camr_limits_for_interpolation( 6, x, y )
#' camr_linear_interpolation( inp )
#'
#' # Approximation with first/last value (use with caution)
#' inp = camr_limits_for_interpolation( 6, x, y, use_first_last = T )
#' camr_linear_interpolation( inp )
#'
#' @export

camr_limits_for_interpolation = function(
    value,
    x,
    y,
    interp_y = TRUE,
    use_first_last = FALSE,
    warn_if_first_last = TRUE ) {

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
    } else {

      if ( use_first_last ) {

        # if no values above specified value
        if ( !any( sel_above ) ) {

          # Use final x and y-value
          out[1] = x[n]
          out[2] = y[n]
          out[3] = x[n]
          out[4] = y[n]
          if ( interp_y ) out[5] = y[n] else out[5] = x[n]

          which_edge = 'last'
        }

        # if no values below specified value
        if ( !any( sel_below ) ) {

          # Use first x and y-value
          out[1] = x[1]
          out[2] = y[1]
          out[3] = x[1]
          out[4] = y[1]
          if ( interp_y ) out[5] = y[1] else out[5] = x[1]

          which_edge = 'first'
        }

        if ( warn_if_first_last ) {
          warning(
            paste0(
              'Specified value outside provided vectors; using ',
              which_edge, ' set of values as approximation'
            ),
            call. = F
          )
        }

      }

    }

  }

  return( out )
}

#### 2) camr_linear_interpolation ####
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
#' @param interp_y Logical; if `TRUE`, the value to
#'   interpolate is assumed to be a y coordinate.
#'
#' @author Kevin Potter
#'
#' @return The interpolated x or y value. If outside the lower or upper
#'   boundaries, `NA` is returned instead.
#'
#' @examples
#' # Linear interpolation for y
#' inp <- c( x0 = 0, y0 = 0, x1 = 1, y1 = 2, x = .5 )
#' camr_linear_interpolation( inp )
#' # Linear interpolation for x
#' inp <- c( x0 = 0, y0 = 0, x1 = 1, y1 = 2, y = .5 )
#' camr_linear_interpolation( inp, FALSE )
#' # Linear interpolation across multiple values
#' x = c( 0, 1, 2 )
#' y = c( 0, 2, 4 )
#' # Create matrix with 5th column for points to interpolate at
#' m = cbind( x[-3], y[-3], x[-1], y[-1], c( .5, 1.5 ) )
#' # Linear interpolation for y values
#' apply( m, 1, camr_linear_interpolation )
#' # Linear interpolation for x values (NA for value outside boundary)
#' apply( m, 1, camr_linear_interpolation, interp_y = FALSE )
#'
#' @export

camr_linear_interpolation <- function(
    vec,
    interp_y = TRUE ) {

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

#### 3) camr_build_demographics_table ####
#' Build Demographics Table
#'
#' A function that will build a data frame that is well-
#' formatted to easily create a flextable object.
#'
#' @param df A data frame that contains variables named
#'   according to the specification created by Kevin
#'   Potter (i.e. XXX.FFF.Var_name), where XXX is a 3-letter
#'   subset code and FFF is a 3-letter format identifier.
#' @param grp A variable name that contains the grouping
#'   variable for the demographics table.
#' @param funcs A named list of purrr-style lambda functions
#'   where the names follow one of 3 conventions (below). Note:
#'   for all CHR variables, the function operates on the unique
#'   values within that variable, NOT on the variable itself.
#'   Further, the value `n` is available for use within these
#'   functions. All functions must return a CHR type.
#'   \itemize{
#'   \item{FFF}{which applies the function to all variables (with
#'     the exception of IDS.CHR...) with that format identifier.}
#'   \item{XXX.FFF}{which applies the function to all variables (with
#'     the exception of IDS.CHR...) with that subset code and
#'     format identifier.}
#'   \item{XXX.FFF.Var_name}{which applies the function to that
#'     variable}
#'     }
#'
#' @author William Schmitt
#'
#' @returns A data frame.
#'
#' @examples
#' # Example data frame
#' df <- data.frame(
#'   SSS.CHR.Group = c('G1', 'G1', 'G2', 'G2'),
#'   SBJ.INT.Age = c(45, 47, 48, 52),
#'   SBJ.CHR.Race = c('White', 'White', 'Asian', 'Black')
#' )
#'
#' # Runs function on CHR and INT variables
#' dt <- camr_build_demographics_table(
#'   df,
#'   SSS.CHR.Group,
#'   list(
#'     'INT' = ~paste0(round(mean(.), 2)),
#'     'CHR' = ~paste0(./n*100, '%')
#'   )
#' )
#'
#' # Runs function on SBJ.INT and SBJ.CHR variables
#' dt <- camr_build_demographics_table(
#'   df,
#'   SSS.CHR.Group,
#'   list(
#'     'SBJ.INT' = ~paste0(round(mean(.), 2)),
#'     'SBJ.CHR' = ~paste0(./n*100, '%')
#'   )
#' )
#'
#' # Runs function on individual columns
#' dt <- camr_build_demographics_table(
#'   df,
#'   SSS.CHR.Group,
#'   list(
#'     'SBJ.INT.Age' = ~paste0(round(mean(.), 2)),
#'     'SBJ.CHR.Race' = ~paste0(./n*100, '%')
#'   )
#' )
#' @export
#' @md

camr_build_demographics_table <- function(
    df,
    grp,
    funcs ) {

  eqGrp <- rlang::enquo(grp)

  eqFuncs <- rlang::enquo(funcs)

  # Calculate n for grp
  n_tb <- df %>%
    group_by(
      !!eqGrp
    ) %>%
    summarise(
      n = n()
    )
  tb <- n_tb

  for (i in 1:length(funcs)) {

    col <- names(funcs)[i]

    if (str_detect(col, '^\\w{3}\\.\\w{3}\\..+')) {
      cur_df <- df %>%
        select(
          !!eqGrp,
          col
        )
    } else if (str_detect(col, '^\\w{3}\\.\\w{3}')) {
      cur_df <- df %>%
        select(
          !!eqGrp,
          contains(col)
        )
    } else if (str_detect(col, '^\\w{3}$')) {
      cur_df <- df %>%
        select(
          !!eqGrp,
          contains(str_c('.', col, '.'))
        )
    } else {
      warning(str_c(col, ' specification invalid. Skipping.'))
      next
    }

    if (str_detect(col, 'INT')) {
      int_df <- cur_df %>%
        group_by(
          !!eqGrp
        ) %>%
        summarise(
          across(
            contains('.INT.'),
            (!!funcs)[[i]]
          )
        )

      tb <- left_join(tb, int_df)
    } else if (str_detect(col, 'CHR')) {

      chr_df <- cur_df %>%
        select(
          matches('(?<!^IDS)\\.CHR\\.', perl=T),
          !!eqGrp
        ) %>%
        pivot_longer(
          -!!eqGrp,
          names_to = 'Category',
          values_to = 'Value'
        ) %>%
        count(
          !!eqGrp,
          Category,
          Value
        ) %>%
        pivot_wider(
          names_from = c('Category', 'Value'),
          values_from = n,
          names_sep = '...',
          values_fill = 0
        )

      chr_df <- chr_df %>%
        left_join(
          n_tb
        ) %>%
        mutate(
          across(
            contains('.CHR.') & -!!eqGrp,
            (!!eqFuncs)[[i]]
          )
        ) %>%
        select(
          -n
        )

      tb <- left_join(tb, chr_df)
    }
  }

  tb <- tb %>%
    mutate(
      !!eqGrp := str_c(!!eqGrp, '\n(n=', n, ')')
    ) %>%
    select(
      -n
    ) %>%
    pivot_longer(
      -!!eqGrp,
      names_to = 'Category',
      values_to = 'Value'
    ) %>%
    pivot_wider(
      names_from = !!eqGrp,
      values_from = Value
    ) %>%
    mutate(
      Category = str_replace(Category, '\\w{3}\\.(INT|CHR)\\.', ''),
      Category = str_replace(Category, '\\.{3}$', '...Not Reported')
    ) %>%
    separate(
      Category,
      c('Header', 'Category'),
      sep = '\\.{3}'
    ) %>%
    mutate(
      Category = if_else(
        is.na(Category),
        Header,
        Category
      )
    )

  head_vals <- unique(tb$Header[duplicated(tb$Header)])
  blank_rw <- data.frame(matrix(ncol=length(names(tb)),
                                nrow = length(head_vals)))
  names(blank_rw) <- names(tb)
  blank_rw$Header <- head_vals
  blank_rw$Category <- head_vals

  if (any(str_detect(names(tb), 'Total'))) {
    total_col <- names(tb)[str_detect(names(tb), 'Total')]
    other_idx <- names(tb)
    other_idx <- other_idx[!(other_idx %in% total_col)]
    tb <- tb[c(other_idx, total_col)]
  } else {
    total_col <- names(tb)[3]
  }

  tb <- bind_rows(tb, blank_rw) %>%
    arrange(
      Header,
      !is.na(!!as.name(total_col))
    ) %>%
    mutate(
      Category = if_else(
        duplicated(Header) & !is.na(Category),
        str_c('    ', Category),
        Category
      )
    ) %>%
    select(
      -Header
    )

  return(tb)
}

#### 4) camr_statistics ####
#' Compute Univariate Statistic
#'
#' A function for flexible and robust computation of univariate
#' statistics over a vector of observations. Can be combined
#' with the [dplyr::group_by()] and
#' [dplyr::summarise()] functions from
#' the package 'dplyr'.
#'
#' @param x A vector.
#' @param f A function that computes a univariate statistc
#'   (e.g., `mean`, `length`).
#' @param include Optional logical vector of matching length
#'   to `x` indicating obserations to include when computing
#'   the statistic.
#' @param exclude Optional vector of types of observations in
#'   `x` to exclude (exact matches).
#' @param na.rm Logical; if `TRUE`, removes `NA` values.
#' @param default The default value to return in the case of
#'   missing data (i.e., no observation over which to compute a
#'   statistic).
#' @param ... Additional parameters for the function `f`.
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
#' camr_statistics( sln )
#'
#' # Mean sepal length
#' camr_statistics( sln, f = mean )
#' # Define custom function for formatted mean
#' f_x <- function(x) as.character( round( mean(x), 2 ) )
#' camr_statistics( sln, f = f_x )
#'
#' # Petal length
#' pln <- iris$Petal.Length
#' # Conditional mean for sepal length when petal length < 3.5
#' camr_statistics( sln, f = f_x, include = pln < 3.5 )
#'
#' # Species of iris
#' spc <- iris$Species
#' # Isolate species 'virginica'
#' vrg <- spc == 'virginica'
#' # No petal lengths less than 3.5 for virginica, so return default
#' camr_statistics(
#'   sln[vrg], f = f_x, include = pln[vrg] < 3.5, default = 'No obs'
#' )
#'
#' # Compute percentage of species 'setosa'
#' f_p <- function(x) paste0( round( 100*mean(x == 'setosa') ), '%' )
#' camr_statistics( spc, f = f_p )
#' # Exclude 'virginica'
#' camr_statistics( spc, f = f_p, exclude = 'virginica' )
#'
#' @export

camr_statistic <- function(
    x,
    f = length,
    include = NULL,
    exclude = NULL,
    na.rm = TRUE,
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

