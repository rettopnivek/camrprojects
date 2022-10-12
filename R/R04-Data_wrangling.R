# Data wrangling functions
# Written by...
#   Kevin Potter
#   William Schmitt
# Maintained by...
#   Michael Pascale
#   Kevin Potter
# Email:
#   mppascale@mgh.harvard.edu
#   kpotter5@mgh.harvard.edu
# Please email us directly if you
# have any questions or comments
# Last updated: 2022-10-07

# Table of contents
# 1) camr_add_attribute
# 2) camr_missing
# 3) camr_make_index
# 4) camr_unique_values
# 5) camr_column
# 6) camr_values_labels
# 7) camr_match_and_assign
# 8) camr_merge_data_frames
# 9) camr_shuffle_groups
# 10) camr_list_elements

#### 1) camr_add_attribute ####
#' Add Attribute to R Object
#'
#' Function to add an attribute to an
#' R object while preserving any
#' existing attributes (especially
#' useful for not overwriting attributes
#' for dates to function properly).
#'
#' @param x An R object.
#' @param attribute The element to add
#'   as an attribute.
#' @param label The label to use in the
#'   list of attributes.
#'
#' @author Kevin Potter
#'
#' @return The R object with updated attributes.
#'
#' @examples
#'
#' x <- 1:3
#' x <- x |> camr_add_attribute( "Hello world", "example" )
#' print( x )
#'
#' # Preserves existing attributes
#' x <- as.Date( c( "2000-01-01", "2010-06-15", "2020-12-25" ) )
#' x <- x |> camr_add_attribute( "Hello world", "example" )
#' print( class( x ) )
#' print( attributes( x )$example )
#'
#' @export

camr_add_attribute <- function( x, attribute, label ) {

  lst <- attributes( x )

  if ( is.null( lst ) ) {
    lst <- list( attribute )
    l <- 1
  } else {
    l <- length(lst) + 1
    lst <- c( lst, list( attribute ) )
  }
  names( lst )[l] <- label

  attributes(x) <- lst

  return(x)
}

#### 2) camr_missing ####
#' Identify Missing Data
#'
#' Function that checks for missing values in
#' a vector. Allows for checking against multiple
#' different codes that denote missing values
#' (e.g., a character vector where missing
#' values can be either \code{NA} or \code{""}).
#'
#' @param x A vector of values.
#' @param codes A vector of possible values
#'   of \code{x} that indicate missing data.
#' @param attribute_label The label for
#'   sublist containing codes for missing data
#'   in the list of attributes for \code{x}.
#' @param return_codes A logical value; if
#'   \code{TRUE} returns the set of codes
#'   for missing data actually found in the
#'   data set.
#'
#' @author Kevin Potter
#'
#' @return A logical vector equal to \code{TRUE}
#' if a value is missing and \code{FALSE} otherwise.
#'
#' @examples
#' x <- c( NA, "", "Cat", "Dog", "Missing" )
#' print( camr_missing(x) )
#'
#' # Custom codes
#' print( camr_missing(x, codes = c( NA, "Missing" ) ) )
#'
#' # Codes can be saved as an attribute
#' x <- x |> camr_add_attribute( c( NA, "Missing" ), "missing" )
#' # Function can automatically access
#' print( camr_missing(x) )
#'
#' # Can return only applicable codes for a vector
#' x <- c( "", "Cat", "Dog" ) # No NA values
#' print( camr_missing(x, return_codes = TRUE ) ) # Only "" returned
#'
#' @export

camr_missing <- function(
    x,
    codes = NULL,
    attribute_label = "missing",
    return_codes = FALSE ) {

  lst <- attributes( x )

  if ( !is.null(lst) ) {

    if ( !is.null( lst[[ attribute_label ]] ) ) {

      codes <- lst[[ attribute_label ]]

    }

  }

  if ( is.null( codes ) ) {

    # Default codes for missing data
    codes <- c( NA, "" )

  }

  N <- length(x)
  out <- rep( FALSE, N )

  M <- length( codes )
  code_found <- rep( FALSE, M )

  # Loop over codes
  for ( m in 1:M ) {

    # If code is NA
    if ( is.na( codes[m] ) ) {

      is_missing <- is.na( x )

      # Close 'If code is NA'
    } else {

      is_missing <- x %in% codes[m]

      # Close else for 'If code is NA'
    }

    # If any missing data found
    if ( any( is_missing ) ) {

      code_found[m] <- TRUE

      # Close 'If any missing data found'
    }

    out[ is_missing ] <- TRUE

    # Close 'Loop over codes'
  }

  # If specified return applicable codes
  if ( return_codes ) {

    if ( any( code_found ) ) {
      return( codes[ code_found ] )
    } else {
      return( NULL )
    }

    # Close 'If specified return applicable codes'
  }

  return( out )
}

#### 3) camr_make_index ####
#' Create Numerical Index for Variable
#'
#' Creates a numerical index starting from 1
#' going to the total number of distinct entries
#' for a variable.
#'
#' @param x A vector.
#' @param ... Additional arguments for the
#'   [camr_missing] function.
#'
#' @author Kevin Potter
#'
#' @return A vector of integers starting from 1
#'   indexing the distinct entries in the vector
#'   \code{x}.
#'
#' @examples
#' camr_make_index( rep( LETTERS[1:3], each = 2 ) )
#'
#' @export

camr_make_index <- function( x, ... ) {

  missing_values <- camr_missing( x, ... )

  out <- rep( NA, length(x) )
  out[ !missing_values ] <- as.numeric( factor( x[ !missing_values ] ) )

  return( out )
}

#### 4) camr_unique_values ####
#' Extract Unique Values
#'
#' Function that filters out missing values and
#' propagates the unique values associated with
#' distinct cases in an index column. Useful,
#' for example, taking values (like participant
#' characteristics) that were collected
#' at a single time point and propagating them
#' across all time points.
#'
#' @param dtf A data frame.
#' @param index The column in \code{dtf} that serves
#'   as the grouping factor (i.e., each distinct case
#'   in \code{index} should correspond to a unique value).
#'   Non-standard evaluation possible.
#' @param variables A character vector, all columns in
#'   \code{dtf} in which unique values should be extracted.
#' @param default The default value to return if unique
#'   values are missing.
#' @param propagate A logical value; if \code{TRUE},
#'   propagates unique values by \code{index} to match
#'   the number of rows of \code{dtf}. If \code{FALSE}
#'   returns the unique value for each distinct
#'   case of \code{index}.
#' @param first A logical value; if \code{TRUE} uses the
#'   first value found there is more than one value
#'   identified for a given case in \code{index},
#'   otherwise uses the final value.
#' @param warn_if_multiple A logical value; if \code{TRUE}
#'   returns a warning whenever multiple values are
#'   found for a given case in \code{index}.
#'
#' @author Kevin Potter
#'
#' @return A vector of unique values associated with the
#' distinct cases in \code{index}.
#'
#' @examples
#' data( example_CAM_data_set )
#'
#' example_CAM_data_set |>
#'   camr_unique_values(
#'     IDS.INT.Screening,
#'     "SBJ.INT.Age"
#'  )
#'
#' example_CAM_data_set |>
#'   camr_unique_values(
#'     IDS.INT.Screening,
#'     "SBJ.INT.Age",
#'     propagate = FALSE
#'   )
#'
#' # Can extract values over multiple columns
#' example_CAM_data_set |>
#'   camr_unique_values(
#'     IDS.INT.Screening,
#'     c( "SBJ.INT.Age", "SBJ.INT.Age_at_visit_1" ),
#'     propagate = FALSE,
#'     warn_if_multiple = FALSE
#'   )
#' # If 'warn_if_multiple' was TRUE function
#' # would appropriately generate warning for
#' # 3rd individual who has different ages
#' # recorded across variables
#'
#' @export

camr_unique_values <- function(
    dtf,
    index,
    variables,
    default = NA,
    propagate = TRUE,
    first = TRUE,
    warn_if_multiple = TRUE ) {

  # Non-standard evaluation for index variable
  chr_index <- as.character( substitute( index ) )

  # Extract index variable
  idx <- dtf[[ chr_index ]]
  # Determine non-missing entries
  entries <- !camr_missing( idx )

  # Number of variables
  K <- length(variables)
  # Number of entries
  N <- nrow( dtf )

  # Indices to loop over
  idx_to_loop <- idx

  # If specified collapse to unique cases
  if ( !propagate ) {

    idx_to_loop <- unique( idx[ entries ] )
    N <- length(idx_to_loop)

    # Close 'If specified collapse to unique cases'
  }

  # Initialize matrix
  values <- matrix( default, N, K )

  # Loop over variables
  for ( k in 1:K ) {

    # Determine missing values
    x_missing <- camr_missing( dtf[[ variables[k] ]] )

    # Apply function over individual cases
    values[,k] <- sapply( 1:N, function(j) {

      # Match against index variable
      i <- idx %in% idx_to_loop[j] & entries

      # If any matches
      if ( any(i) ) {

        # If non-missing values
        if ( any( !x_missing[i] ) ) {

          # Unique values
          out <- unique( dtf[[ variables[k] ]][i & !x_missing] )

          # Check for multiple unique values
          if ( warn_if_multiple ) {

            n_unq <- length( out )

            # If multiple values detected
            if ( n_unq > 1 ) {

              wrn_msg <- paste0(
                "Index ", idx_to_loop[j], ": ",
                "Multiple values found for '",
                variables[k], "'"
              )
              warning(
                wrn_msg
              )

              # Close 'If multiple values detected'
            }

            # Close 'Check for multiple unique values'
          }

          out <- out[1]

          # Close 'If non-missing values'
        } else {

          out <- default

          # Close else for 'If non-missing values'
        }

        # Close 'If any matches'
      } else {

        out <- default

        # Close else for 'If any matches'
      }

      return( out )

      # Close 'Apply function over individual cases'
    } )

    # Close 'Loop over variables'
  }

  # If multiple variables
  if ( K > 1 ) {

    # Collapse over variables
    unq_val <- sapply( 1:nrow( values ), function(i) {

      # Extract values for current row
      x <- values[i,]
      x <- unique( x )

      # Non-missing values
      no_missing <- !is.na(x) & !( x %in% default )

      # Take first instance
      if ( first ) {

        case_to_take <- 1

        # Close 'Take first instance'
      } else {

        case_to_take <- sum( no_missing )

        # Close else for 'Take first instance'
      }

      # If non-missing values
      if ( any( no_missing ) ) {

        # Check for multiple unique values
        if ( warn_if_multiple ) {

          # If multiple values detected
          if ( sum( no_missing ) > 1 ) {

            wrn_msg <- paste0(
              "Index ", idx[i], ": ",
              "Multiple values found across variables"
            )
            warning(
              wrn_msg
            )

            # Close 'If multiple values detected'
          }

          # Close 'Check for multiple unique values'
        }

        return( x[ which( no_missing )[case_to_take] ] )

        # Close 'If non-missing values'
      } else {

        return( default )

        # Close else for 'If non-missing values'
      }

      # Close 'Collapse over variables'
    } )

    # Close 'If multiple variables'
  } else {

    unq_val <- values

    # Close else for 'If multiple variables'
  }

  if ( is.matrix( unq_val ) ) {
    unq_val <- unq_val[,1]
  }

  if ( !propagate ) {
    names( unq_val ) <- idx_to_loop
  }

  return( unq_val )
}

#### 5) camr_column ####
#' Extract Column Names Meeting Inclusion/Exclusion Criteria
#'
#' A function that matches or excludes column names in a
#' data frame based on user-supplied sub-strings.
#'
#' @param dtf A data frame.
#' @param ... Character strings with the sub-strings to match
#'   (or exclude) against the column names in `dtf`.
#'   If an entry starts with either `!`, `~`, or
#'   `-`, any columns containing the substring will be
#'   excluded. Otherwise, the function will locate
#'   all column names containing all inputted sub-strings.
#'
#' @author Kevin Potter
#'
#' @return A vector of column names meeting the inclusion
#' and exclusion criteria.
#'
#' @examples
#' data( example_CAM_data_set )
#'
#' # All variables containing 'SSS'
#' example_CAM_data_set |> camr_column( 'SSS' )
#'
#' # All variables containing both 'SSS' and 'CHR'
#' example_CAM_data_set |> camr_column( 'SSS', 'CHR' )
#'
#' # Variables containing 'SSS' but not 'CHR'
#' example_CAM_data_set |> camr_column( 'INV', '~INT' )
#'
#' @export

camr_column <- function(
    dtf,
    ... ) {

  checkmate::assert_data_frame( dtf )

  args <- list(...)
  n_args <- length( args )

  include <- rep( '', n_args )
  exclude <- rep( '', n_args )
  inc_i <- 1
  inc_e <- 1
  for ( i in 1:n_args ) {
    txt <- as.character( args[[i]] )
    if ( grepl( '!', txt, fixed = T ) |
         grepl( '~', txt, fixed = T ) |
         grepl( '-', txt, fixed = T ) ) {
      txt <- gsub( '!', '', txt, fixed = T )
      txt <- gsub( '~', '', txt, fixed = T )
      txt <- gsub( '-', '', txt, fixed = T )
      exclude[inc_e] <- txt
      inc_e <- inc_e + 1
    } else {
      include[inc_i] <- txt
      inc_i <- inc_i + 1
    }
  }

  if ( all( include == '' ) ) {
    include = NULL
  } else {
    include <- include[ include != '' ]
  }
  if ( all( exclude == '' ) ) {
    exclude = NULL
  } else {
    exclude <- exclude[ exclude != '' ]
  }

  clm <- colnames( dtf )
  K <- length( clm )

  if ( !is.null( include ) ) {
    each_include <- sapply( include, function(x) {
      grepl( x, clm, fixed = T )
    } )
  } else {
    each_include = cbind( rep( T, K ) )
  }


  if ( !is.null( exclude ) ) {
    each_exclude <- sapply( exclude, function(x) {
      grepl( x, clm, fixed = T )
    } )
  } else {
    each_exclude = cbind( rep( F, K ) )
  }

  entries =
    rowSums( each_include ) == length( include ) &
    !( rowSums( each_exclude ) > 0 )

  return( clm[ entries ] )
}


#### 6) camr_values_labels ####
#' Display Values and Associated Labels
#'
#' A function that takes two columns in a data frame
#' (assumed to be an initial column of values and
#' a subsequent column of associated labels)
#' and displays the assignment of values to labels.
#'
#' @param dtf A data frame.
#' @param values A character string, the column name for
#'   the values of interest (non-standard evaluation possible).
#' @param labels A character string, the column name for
#'   the labels of interest (non-standard evaluation possible).
#'
#' @author Kevin Potter
#'
#' @return A data frame with a column for values and
#' a column for associated labels.
#'
#' @examples
#' data( example_CAM_data_set )
#'
#' example_CAM_data_set$SBJ.CHR.Biological_sex <-
#'   c( "Male", "Female" )[ example_CAM_data_set$SBJ.INT.Biological_sex ]
#'
#' example_CAM_data_set |>
#'   camr_values_labels( SBJ.INT.Biological_sex, SBJ.CHR.Biological_sex )
#'
#' @export

camr_values_labels <- function(
    dtf,
    values,
    labels ) {

  # Non-standard evaluation
  V <- as.character( substitute( values ) )
  L <- as.character( substitute( labels ) )

  dtf$Cur_values <- dtf[[ V ]]
  dtf$Cur_labels <- dtf[[ L ]]

  not_missing <-
    !camr_missing( dtf[[ V ]] ) &
    !camr_missing( dtf[[ L ]] )

  if ( any( not_missing ) ) {

    out = dtf %>%
      filter(
        not_missing
      ) %>%
      group_by(
        Values = Cur_values
      ) %>%
      summarise(
        Labels = unique( Cur_labels ),
        .groups = 'drop'
      ) %>%
      data.frame( stringsAsFactors = F )

  } else {
    stop( 'No complete cases found' )
  }

  return( out )
}

#### 7) camr_match_and_assign ####
#' Assign New Values Based on Partial or Exact Matching
#'
#' A flexible function for matching old values and assigning
#' new values based on matches. Can also assign new values
#' based on whether old values fall within a range or interval
#' of values.
#'
#' @param x Either (a) a vector of values to match over, or
#'   (b) a data frame with columns to match over.
#' @param values Either (a) a vector of values to match against,
#'   or (b) a named list with values to match against. If
#'   matching over multiple columns in a data frame, the
#'   names for the elements of `values` must equal the
#'   column names to match against. Optionally, the user
#'   can also include an element named `new_values`.
#' @param new_values A vector of new values to assign based on
#'   matches to elements from `values` (vector must be
#'   of equivalent length to `values` or the elements of `values`).
#'   Note if this argument is not supplied, then the argument
#'   `values` must contain an element `new_values`.
#' @param type The type of matching, either 'exact' (cases for
#'   `x` and `values` must match exactly), 'partial'
#'   (cases for `x` must contain `values` in some form),
#'   'greater than' (lower boundary > `x` \eqn{\leq} upper boundary) or
#'   'less than' (lower boundary \eqn{\geq} `x` < upper boundary).
#' 'partial' or 'exact'
#'   (uses `grepl` or `\%in\%`, respectively).
#' @param default Either a single value to assign in the absence of
#'   a match, or a vector equivalent in length to `x`.
#' @param replace_if An optional vector specifying the subset of
#'   default values when it is appropriate to assign new values.
#'
#' @return A new vector of equivalent length to `x`, with
#'   values assigned based on successful matches.
#'
#' @author Kevin Potter
#'
#' @examples
#' # Replace letters with numbers
#' x <- rep( c("A", "B", "C" ), each = 2 )
#' camr_match_and_assign(
#' x,
#'   c( "A", "B", "C" ),
#'   new_values = 1:3
#' )
#'
#' # Alternative formats for input
#'
#' # As single list with both old and new values
#' camr_match_and_assign(
#'   x,
#'   list( x = c( "A", "B", "C" ), new_values = 1:3 )
#' )
#'
#' # Converting matrix to data frame
#' camr_match_and_assign(
#'   x,
#'   rbind(
#'     c( "A", "1" ),
#'     c( "B", "2" ),
#'     c( "C", "3" )
#'   ) |> data.frame() |> setNames( c("x", "new_values" ) )
#' )
#'
#' # Can restrict subset of entries
#' # over which replacement occurs;
#' # replace odd-numbered rows
#' camr_match_and_assign(
#'   x,
#'   c( "A", "B", "C" ),
#'   new_values = 1:3,
#'   default = x, # Can pass vector as default
#'   replace_if = seq_along( x ) %in% c( 1, 3, 5 )
#' )
#'
#' # Can base on partial matching
#' x <- rep( c("AD", "BE", "CF" ), each = 2 )
#' camr_match_and_assign(
#'   x,
#'   c( "A", "B", "C" ),
#'   new_values = 1:3,
#'   type = 'partial'
#' )
#'
#' # Can match over range or interval of values
#' set.seed( 112 )
#' x <- rnorm( 10 )
#' camr_match_and_assign(
#'   x,
#'   list(
#'     lower = c(    -Inf,  -1,    1 ),
#'     upper = c(      -1,   1,  Inf ),
#'     new_values = c( "A", "B", "C" )
#'   ),
#'   new_values = c( "A", "B", "C" ),
#'   type = 'greater than'
#' )
#'
#' @export

camr_match_and_assign <- function(
    x,
    values,
    new_values = NULL,
    type = 'exact',
    default = NA,
    replace_if = NULL ) {

  # Define flexible function for matching
  matches <- function( x, y, type ) {

    if ( type == 'partial' ) return( x %pm% y )
    if ( type == 'exact' ) return( x %em% y )

    stop( "Argument 'type' must be either 'exact' or 'partial'" )
  }

  # Convert to data frame
  against <- data.frame( values, check.rows = TRUE )
  to_match <- data.frame(x)

  N <- nrow( to_match )
  K <- nrow( against )

  # Initialize output
  out <- NULL
  if ( length( default ) == 1 ) {
    out <- rep( default, N )
  }
  if ( length( default ) == N ) {
    out <- default
  }

  if ( is.null( out ) ) {
    err_msg <- paste0(
      "Argument 'default' must be a single value or ",
      "a vector matching in length to 'x'"
    )
    stop( err_msg )
  }

  if ( is.null( replace_if ) ) {
    replace_if <- rep( TRUE, N )
  }

  checkmate::assert_logical( replace_if, len = N )

  # Incorporate new values for assignment
  if ( !is.null( new_values ) ) {

    # If mismatch in length
    if ( length( new_values ) != K ) {

      err_msg <- paste0(
        "Argument 'new_values' must match in ",
        "length with elements of 'values'"
      )
      stop( err_msg )

      # Close 'If mismatch in length'
    }

    against$new_values <- new_values

    # Close 'Incorporate new values for assignment'
  }

  if ( "new_values" %not_in% colnames( against ) ) {
    err_msg <- paste0(
      "Must provide argument 'new_values' or include an element ",
      "in 'values' named 'new_values'"
    )
    stop( err_msg )
  }

  clm <- colnames( against )
  clm <- clm[ clm %not_in% "new_values" ]

  # If matching over interval of values
  if ( type %in% c( 'greater than', 'less than' ) ) {

    # Loop over intervals
    for ( k in 1:K ) {

      not_NA <- !is.na( to_match[[1]] )

      # If matching NA
      if ( is.na( against[[ clm[1] ]][k] ) ) {

        does_match <- !not_NA

        # Close 'If matching NA'
      } else {

        # If lower boundary is greater than
        if ( type %in% 'greater than' ) {

          does_match <-
            not_NA &
            to_match > against[[ clm[1] ]][k] &
            to_match <= against[[ clm[2] ]][k]

          # Close 'If lower boundary is greater than'
        } else {

          does_match <-
            not_NA &
            to_match >= against[[ clm[1] ]][k] &
            to_match < against[[ clm[2] ]][k]

          # Close else for 'If lower boundary is greater than'
        }

        # Close else for 'If matching NA'
      }

      to_replace <-
        as.logical( does_match ) & replace_if
      out[ to_replace ] <- against[k, "new_values"]

      # Close 'Loop over intervals'
    }

    # Close 'If matching over interval of values'
  } else {

    if ( any( clm %not_in% colnames( to_match ) ) ) {

      if ( ncol( to_match ) == 1 ) {
        colnames( to_match ) <- clm
      } else {
        stop( "Columns for matching not found in 'x'" )
      }

    }

    # Number of variables to match over
    V <- length( clm )

    # Loop over assignments
    for ( k in 1:K ) {

      # Indicator for matches
      does_match <- matrix( FALSE, N, V )

      # Loop over variables
      for ( v in 1:V ) {

        does_match[,v] <-
          to_match[[ clm[v] ]] |> matches(
            against[ k, clm[v] ], type
          )

        # Close 'Loop over variables'
      }

      to_replace <-
        apply( does_match, 1, all ) &
        replace_if

      out[ to_replace ] <- against[k,"new_values"]


      # Close 'Loop over assignments'
    }

    # Close else for 'If matching over interval of values'
  }

  if ( all( is.na( default) ) ) {
    if ( all( is.na( out ) ) ) {
      stop( "No matches found" )
    }
  } else {
    if ( all( out %in% default ) ) {
      stop( "No matches found" )
    }
  }

  return( out )
}

#### 8) camr_merge_data_frames ####
#' Merge Multiple Data Frames Together
#'
#' Function to merge multiple data frames
#' (assuming equal number of rows) together.
#'
#' @param ... The data frames to merge together.
#'
#' @return A single data frame.
#'
#' @examples
#' dtf_1 <- data.frame( X = 1:2 )
#' dtf_2 <- data.frame( Y = LETTERS[1:2] )
#' dtf_3 <- data.frame( Z = 3:4 )
#'
#' print( camr_merge_data_frames( dtf_1, dtf_2, dtf_3 ) )
#'
#' @export

camr_merge_data_frames <- function( ... ) {

  args <- list(...)
  n_args <- length( args )

  # Loop over arguments
  for ( i in 1:n_args ) {

    # Initialize data frame
    if ( i == 1 ) {

      dtf_merged = args[[i]]

      # Check inputs
      checkmate::assert_data_frame( dtf_merged )

      # Number of rows
      n_rows <- nrow( dtf_merged )

      all_columns <- colnames( dtf_merged )

      # Close 'Initialize data frame'
    } else {

      # Check inputs
      checkmate::assert_data_frame( args[[i]] )

      # If mismatch in dimensions
      if ( nrow( args[[i]] ) != n_rows ) {

        err_msg <- paste0(
          "Number of rows for '", names( args )[i], "' ",
          "does not match number of rows for '", names( args )[1], "'"
        )
        stop( err_msg )

        # Close 'If mismatch in dimensions'
      }

      # Check for duplicate column names
      current_columns <- colnames( args[[i]] )
      if ( any( current_columns %in% all_columns ) ) {

        duplicate_columns <-
          current_columns[ current_columns %in% all_columns ]

        err_msg <- paste0(
          "Duplicate column names in '", names( arg )[i], "'; check...",
          paste( paste0( "    ", duplicate_columns, "\n" ), collapse = "\n" )
        )
        stop( err_msg )

        # Close 'Check for duplicate column names'
      }

      # Merge data frames
      dtf_merged <- cbind( dtf_merged, args[[i]] )

      all_columns <- colnames( dtf_merged )

      # Close else for 'Initialize data frame'
    }
  }

  return( dtf_merged )
}


#### 9) camr_shuffle_groups ####
#' Shuffle Grouping Variable Levels
#'
#' Function that shuffles the levels of a grouping variable
#' (e.g., treatment or intervention assignments) over
#' participants (and optionally a within-participant
#' variable like study visit or time point). This is
#' useful, for example, to create a data set for an
#' analyst-blind design.
#'
#' @param dtf A data frame.
#' @param id A character string, the column with participant
#'   identifiers.
#' @param group A character string, the column for the
#'   grouping variable.
#' @param within An optional character string, the column with
#'   the levels for a within-participant variable (e.g.,
#'   time points or visits).
#' @param include An optional logical vector matching in length
#'   to the number of rows in `dtf`, indicating the subset
#'   of cases to shuffle. If `NULL` all rows are used.
#' @param group_levels An optional character vector, the subset
#'   of levels of `group` to consider.
#' @param original_freq Logical; if `TRUE` shuffles
#'   data in a way that preserves the original frequencies
#'   for group levels - otherwise, assigns new levels in
#'   equal frequencies.
#' @param save_unshuffled Logical; if `TRUE` adds a
#'   new column with the original unshuffled group levels.
#' @param rng_seed An integer, the RNG seed to use to ensure
#'   reproducibility.
#'
#' @return A data frame with shuffled group levels for the
#'   `group` variable.
#'
#' @author Kevin Potter
#'
#' @export

camr_shuffle_groups <- function(
    dtf,
    id,
    group,
    within = NULL,
    include = NULL,
    group_levels = NULL,
    original_freq = FALSE,
    save_unshuffled = TRUE,
    rng_seed = NULL ) {

  # Specify RNG seed
  if ( is.null( rng_seed ) ) {

    rng_seed <- round( runif(1)*100000 )

    # Close 'Specify RNG seed'
  }
  set.seed( rng_seed )

  # Extract participant identifiers
  IDS <- dtf[[ id ]]

  # Extract grouping variable
  GRP <- dtf[[ group ]]

  # Determine levels for grouping variable
  if ( is.null( group_levels ) ) {

    group_levels <- unique( GRP )

    # Close 'Determine levels for grouping variable'
  }

  # Determine rows to include
  if ( is.null( include ) ) {

    include <- rep( TRUE, nrow( dtf ) )

    # Close 'Determine rows to include'
  }
  include <- include & GRP %in% group_levels

  # If group assignment is not within participant
  if ( is.null( within ) ) {

    # Determine group level assigned to each participant
    grp <- aggregate(
      GRP[ include ],
      list( IDS[ include ] ),
      function( x ) unique( x )[1]
    )
    colnames( grp ) <- c( 'ID', 'Group' )

    # Shuffle group assignments

    # Preserve original frequencies for levels
    if ( original_freq ) {

      grp$Group <- sample( grp$Group )

      # Close 'Preserve original frequencies for levels'
    } else {

      grp$Group <- sample(
        group_levels, size = nrow( grp ),
        replace = TRUE
      )

      # Close else for 'Preserve original frequencies for levels'
    }

    # Save original unshuffled assignments
    if ( save_unshuffled ) {

      dtf <- cbind( dtf, GRP )
      colnames( dtf )[ ncol( dtf ) ] <-
        paste0( group, '.Unshuffled' )

      # Close 'Save original unshuffled assignments'
    }

    # Logical indicator for rows that were shuffled
    dtf <- cbind( dtf, include )
    colnames( dtf )[ ncol( dtf ) ] <-
      'INC.LGC.Shuffled_terms'

    # Rewrite grouping variable with new shuffled levels
    dtf[[ group ]][ include ] <- unlist(
      sapply(
        dtf[[ id ]][ include ],
        function( s ) {
          j <- grp$ID == s
          return( grp$Group[j] )
        }
      )
    )

    # Close 'If group assignment is not within participant'
  } else {

    # Extract variable separating
    # within-participant assignments
    WTH <- dtf[[ within ]]

    # Determine group level assigned to each particiant
    # by each level of the within-participant variable
    grp <- aggregate(
      GRP[ include ],
      list( IDS[ include ], WTH[ include ] ),
      function( x ) unique( x )[1]
    )
    colnames( grp ) <- c( 'ID', 'Within', 'Group' )

    # Extract participant IDs
    ids <- unique( grp$ID )
    n_ids <- length( ids )

    # Shuffle group assignments

    # Preserve original frequencies for levels
    if ( original_freq ) {

      # Loop over particpants
      for ( s in 1:n_ids ) {

        j <- grp$ID == ids[s]
        # Reorder existing group levels
        grp$Group[j] <- sample( grp$Group[j] )

        # Close 'Loop over particpants'
      }

      # Close 'Preserve original frequencies for levels'
    } else {

      # Loop over particpants
      for ( s in 1:n_ids ) {

        # Isolate data for current participant
        j <- grp$ID == ids[s]
        # Assign new group levels
        grp$Group[j] <- sample( group_levels, size = sum(j) )

        # Close 'Loop over particpants'
      }

      # Close else for 'Preserve original frequencies for levels'
    }

    # Save original unshuffled assignments
    if ( save_unshuffled ) {

      dtf <- cbind( dtf, GRP )
      colnames( dtf )[ ncol( dtf ) ] <-
        paste0( group, '.Unshuffled' )

      # Close 'Save original unshuffled assignments'
    }

    # Logical indicator for rows that were shuffled
    dtf <- cbind( dtf, include )
    colnames( dtf )[ ncol( dtf ) ] <-
      'INC.LGC.Shuffled_terms'

    # Rewrite grouping variable with new shuffled levels

    # Loop over new assignments
    for ( s in 1:nrow( grp ) ) {

      # Isolate data for current participant
      j <-
        dtf[[ id ]] == grp$ID[s] &
        dtf[[ within ]] == grp$Within[s]

      # Rewrite grouping variable with new shuffled levels
      dtf[[ group ]][j] <- grp$Group[s]

      # Close 'Loop over particpants'
    }

    # Close else for 'If group assignment is not within participant'
  }

  return( dtf )
}

#### 10) camr_list_elements ####
#' List Elements as Embedded in R Code
#'
#' Function to generate R code listing the distinct
#' elements of \code{x} (e.g., embedding them in a
#' concatenate or list statement).
#'
#' @param x A vector of values.
#' @param left A character string to attach
#'   to the left of each element in \code{x}.
#' @param right A character string to attach
#'   to the right of each element in \code{x}.
#' @param first A character string to attach
#'   to the left of the final collapsed string.
#' @param last A character string to attach
#'   to the right of the final collapsed string.
#' @param drop_last An optional character string
#'   indicating any patterns to remove from the
#'   final element.
#' @param console A logical value; if \code{TRUE}
#'   displays the results in the console window,
#'   otherwise returns a character string.
#'
#' @returns Either a message to the console window
#' or a character string.
#'
#' @examples
#' camr_list_elements( LETTERS[1:3] )
#'
#' # Operates only on distinct values
#' camr_list_elements( rep( LETTERS[1:3], each = 2 ) )
#'
#' # Can adjust how elements are embedded in R code
#' camr_list_elements( 1:3, left = '  ', right = ',\n' )
#' camr_list_elements( LETTERS[1:3], first = 'list(\n' )
#'
#' @export

camr_list_elements <- function(
    x,
    left = '  "',
    right = '",\n',
    first = 'c(\n',
    last = ')',
    drop_last = ",",
    console = TRUE ) {

  u <- unique( x )

  s <- paste0( left, u, right )

  if ( !is.null( drop_last ) ) {
    s[ length(s) ] <-
      gsub( drop_last, "", s[ length(s) ], fixed = TRUE )
  }

  out <- paste( s, collapse = "" )

  out <- paste0( first, out, last )

  if ( console ) {
    message( out )
  } else {
    return( out )
  }

}

