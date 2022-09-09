# General-purpose functions
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
# Last updated: 2022-09-08

# Table of contents
# 3) camr_extract_unique_value
# 4) camr_column
# 5) camr_values_labels
# 6) camr_check_for_missing
# 7) camr_shuffle_groups


#### 1) camr_extract_unique_value ####
#' Extract Unique Values From Data Frames or Lists
#'
#' A function that will search over a subset of rows in a data frame
#' (or a list structured like a data frame) and extract a unique
#' value after excluding missing or irrelevant data.
#'
#' @param x A data frame or a list of variables with matching lengths.
#' @param variable_names A vector of variables names to loop over when
#'   attempting to isolate the unique value.
#' @param entries A logical vector, `TRUE` for rows to search for
#'   the unique value and `FALSE` otherwise.
#' @param default The default output to return if no unique values can
#'   be found.
#' @param missing A vector of values to treat as missing and exclude
#'   when searching for the unique value.
#' @param reference An optional character string giving the variable
#'   name in `x` to use when printing warnings in the case of
#'   multiple values being found. Defaults to the first variable
#'   in `x`.
#' @param check_for_multiple Logical; if `TRUE` will check if
#'   more than one value was found in the subset of rows to consider
#'   and display a warning message with details if this occurs.
#' @param allow_multiple Logical; if `TRUE` will allow multiple
#'   return values. By default, it does not override
#'   `check_for_multiple`, so remember to change this
#'   if applicable.
#'
#' @author Kevin Potter, William Schmitt
#'
#' @return A single value.
#'
#' @examples
#'
#' # Create example data frame
#'
#' # Create three 'ID' levels
#' dtf <- data.frame( ID = rep( 1:3, each = 3 ) )
#'
#' # Create an age variable, but assume entered
#' # over two different variables with several
#' # missing values
#' dtf$Age_session_1 <- NA; dtf$Age_session_1[c(1,4)] = c(25,20)
#' dtf$Age_session_2 <- NA; dtf$Age_session_2[7] = 21
#'
#' # Create a status variable, but assume multiple
#' # indicators for missing data
#' dtf$Status <- c( 'No entry', 'Good', 'Poor',
#'                  'No entry', 'N/A', 'Good',
#'                  'No entry', 'Poor', 'N/A' )
#'
#' # Extract unique value for age for two different
#' # variables per levels of 'ID', with user-defined
#' # value for missing cases
#' # Loop over levels for 'ID'
#' for ( id in 1:3 ) {
#'   val <- camr_extract_unique_value(
#'     x = df,
#'     variable_names = c( 'Age_session_1', 'Age_session_2' ),
#'     entries = df$ID == id,
#'     # Specify default value when no values found
#'     default = 'Missing'
#'   )
#'   # Display results
#'   cat( paste0( 'ID: ', id, '; Age = ', val, '\n' ) )
#' }
#'
#' # - Extract unique values for status given
#' #   multiple types of missing data indicators
#' # - Also display warning for case where there
#' #   were multiple non-missing values
#' # Traverse levels for 'ID' via the 'sapply' function
#' res <- sapply( 1:3, function( id ) {
#'   val <- camr_extract_unique_value(
#'     x = df,
#'     variable_names = 'Status',
#'     entries = df$ID == id,
#'     # Deal with multiple indicators for missing data
#'     missing = c( 'No entry', 'N/A' ),
#'     # Specify reference for warnings
#'     reference = 'ID'
#'   )
#'   # Save results
#'   paste0( 'ID: ', id, '; Status = ', val, '\n' )
#' } )
#' # Display results
#' cat( res )
#'
#' @export

camr_extract_unique_value <- function(
    x,
    variable_names,
    entries,
    default = "",
    missing = c( "" ),
    reference = NULL,
    check_for_multiple = TRUE,
    allow_multiple = FALSE ) {

  # Number of variables/columns to loop over
  K <- length( variable_names )

  # Initialize output
  output <- default
  current_value <- default

  # Specify reference variable if there are issues
  # (i.e., subject identifier)
  if ( is.null( reference ) ) {
    reference = names( x )[1]
  }

  # Loop over variables
  for ( k in 1:K ) {

    # Check for missing data over range of entries for subject
    no_missing <-
      entries &
      !is.na( x[[ variable_names[k] ]] ) &
      !x[[ variable_names[k] ]] %in% missing

    # If entry provided, set output to unique value
    if ( any( no_missing ) ) {

      current_value <- unique( x[[ variable_names[k] ]][ no_missing ] )

      # If specified, check if multiple values are found
      # and return a warning
      if ( check_for_multiple ) {

        # Print a warning if multiple values are detected
        if ( length( current_value ) > 1 ) {

          warning_message <- paste0(
            'Multiple values detected:\n',
            'Variable = ', variable_names[k], '\n',
            reference, ' = ', unique( x[[ reference ]][entries] )[1], '\n',
            'Values = ', paste( current_value, collapse = ', ' ), '\n'
          )

          warning( warning_message, call. = FALSE )

        }

      }

      if (allow_multiple) {
        # Append to output
        output <- append(output, current_value)
      } else {

        # Take the first unique value
        output <- current_value[1]

        # Once a unique value is found, stop looping over
        # remaining variables
        break()
      }
    }

  }

  # Return output
  return( output )
}

#### 3) camr_column ####
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
#' # Create a data frame
#' dtf <- data.frame(
#'   IDS.INT.Subject = rep( 1:4, each = 2 ),
#'   SSS.CHR.Group = rep( c( 'A', 'A', 'B', 'B' ), each = 2 ),
#'   SSS.INT.Group = rep( c( 1, 1, 2, 2 ), each = 2 ),
#'   SSS.LGC.Group_A = rep( c( T, T, F, F ), each = 2 ),
#'   SSS.CHR.Time_point = rep( c( 'Pre', 'Post' ), 4 ),
#'   SSS.INT.Time_point = rep( 0:1, 4 ),
#'   OUT.DBL.Scores = rnorm( 8 )
#' )
#'
#' #' # All variables containing 'SSS'
#' dtf %>% camr_column( 'SSS' )
#'
#' # All variables containing both 'SSS' and 'CHR'
#' dtf %>% camr_column( 'SSS', 'CHR' )
#'
#' # Variables containing 'SSS' but not 'CHR'
#' dtf %>% camr_column( 'SSS', '~CHR' )
#'
#' @export

camr_column <- function(
    dtf,
    ... ) {

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


#### 4) camr_values_labels ####
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
#' @export

camr_values_labels <- function(
    dtf,
    values,
    labels ) {

  # Non-standard evaluation
  V = as.character( substitute( values ) )
  L = as.character( substitute( labels ) )

  dtf$Cur_values = dtf[[ V ]]
  dtf$Cur_labels = dtf[[ L ]]

  out = dtf %>%
    group_by(
      Values = Cur_values
    ) %>%
    summarise(
      Labels = unique( Cur_labels ),
      .groups = 'drop'
    ) %>%
    data.frame( stringsAsFactors = F )

  return( out )
}

#### 5) camr_check_for_missing ####
#' Checks for Missing Data
#'
#' Given a list of different codes for missing
#' data (e.g., `NA`, `''`, etc.),
#' identifies missing data in a vector and
#' also determines which missing data codes are
#' applicable.
#'
#' @param x A vector of values.
#' @param codes A list of different codes for
#'   missing data (e.g., `NA`, `''`).
#'
#' @details Vectors of class `Date` are
#' handled slightly differently, as comparisons
#' against values that are not dates will return
#' `NA`. Therefore, dates are only checked
#' against other dates and for `NA` values.
#'
#' @return A list with...
#' \itemize{
#'   \item `missing_values`: A logical vector indicating
#'     which values of `x` are missing;
#'   \item `x_no_missing`: All non-missing values of `x`;
#'   \item `codes_for_missing`: A list with all missing
#'     value codes that were found in `x`. If no missing
#'     values were found, is `NULL`.
#' }
#'
#' @author  Kevin Potter
#'
#' @examples
#' # Vector with two types of missing values
#' x <- c( 'A', 'B', '', NA, 'C' )
#' camr_check_for_missing( x )
#'
#' # Dates
#' x <- as.Date( c( '2000-01-01', '1970-01-01', '2000-02-02', NA ),
#'               format = '%Y-%m-%d' )
#' camr_check_for_missing( x )
#'
#' @export

camr_check_for_missing <- function(
    x,
    codes = list(
      NA, '',
      as.Date( '1970-01-01', format = '%Y-%m-%d' )
    ) ) {

  # Number of observations
  n_obs <- length( x )

  # Number of missing value codes
  n_codes <- length( codes )

  # Identify missing values
  missing_values <- rep( FALSE, n_obs )
  # Vector to track whether codes for missing
  # values found in variable
  is_missing <- rep( TRUE, n_codes )

  #< Loop over codes
  for ( k in 1:n_codes ) {

    #<| Check code for NA
    if ( is.na( codes[[k]] ) ) {

      if ( any( is.na(x) ) ) {
        # Update logical vector for missing values
        missing_values[ is.na( x ) ] <- TRUE
      } else {
        # Indicate code not found in variable
        is_missing[k] <- FALSE
      }

      #|> Close 'Check code for NA'
    } else {

      # Only consider non-NA values for missing
      # if not a date variable
      if ( class( x ) != 'Date' ) {

        if ( class( codes[[k]] ) != 'Date' ) {
          # Check variable for missing values
          entries <- !is.na(x) & x == codes[[k]]
        } else {
          entries <- rep( FALSE, n_obs )
        }

      } else {

        if ( class( codes[[k]] ) == 'Date' ) {
          entries <- x == codes[[k]]
          entries[ is.na(x) ] <- TRUE
        } else {
          entries <- rep( FALSE, n_obs )
        }

      }

      if ( any( entries ) ) {
        # Update logical vector for missing values
        missing_values[entries] <- TRUE
      } else {
        # Indicate code not found in variable
        is_missing[k] <- FALSE
      }

      #|> Close else for 'Check code for NA'
    }

    #> Close 'Loop over codes'
  }

  out <- list(
    missing_values = missing_values,
    x_no_missing = x,
    codes_for_missing = NULL
  )

  if ( any( is_missing ) ) {
    # Remove missing values
    out$x_no_missing <- x[ !missing_values ]
    # Include only codes actually found in variable
    out$codes_for_missing <- codes[ is_missing ]
  }

  return( out )
}

#### 6) camr_match_and_assign ####
#' Assign New Values Based on Partial or Exact Matching
#'
#' Assigns new values based on partial or exact matches with
#' values from an input vector.
#'
#' @param x A vector of values to match over.
#' @param matches A list of values in `x` to match over
#' @param new_values A vector of new values to assign based on
#'   matches to elements from `matches` (vector must be
#'   of equivalent length to `matches`).
#' @param type The type of matching, either 'partial' or 'exact'
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
#' x <- c( 'Cat', 'Hat', 'Rat', 'Dog', 'Fog', 'Cog' )
#' # Partial matching
#' camr_match_and_assign( x, list( 'at', 'og' ), c('A','B') )
#' # Exact matching
#' camr_match_and_assign( x, list( 'Cat', c( 'Dog', 'Fog' ) ), c('A','B'),
#'                   default = '', type = 'exact' )
#' # Vector input for argument 'default'
#' x <- c( 'A', 'A', 'D', 'C', 'A', 'A', 'C', 'D' )
#' camr_match_and_assign( x, list( 'C', 'D' ), c('B','B'), default = x )
#' # Using 'replace_if' for conditional assignment
#' x1 <- rep( LETTERS[1:4], each = 2 )
#' x2 <- rep( LETTERS[5:6], 4 )
#' camr_match_and_assign( x2, list( 'E', 'F' ), c('1','2'),
#'                   default = x1, replace_if = c( 'A', 'B' ) )
#'
#' @export

camr_match_and_assign <- function(
    x,
    matches,
    new_values,
    type = 'partial',
    default = NA,
    replace_if = NULL ) {

  # Number of observations
  No <- length( x )

  # Initialize output
  if ( length( default ) == No ) {
    output <- default
  } else {
    output <- rep( default[1], No )
  }

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

    # If a vector of default values was provided
    # and user provided vector of values specifying
    # when replacement should occur
    if ( ( length( default ) == No ) &
         !is.null( replace_if ) ) {

      # Replace only if default values are in
      # subset appropriate for replacement
      output[ is_match & output %in% replace_if ] <- new_values[i]

    } else {
      # Update output
      output[ is_match ] <- new_values[i]
    }

  }

  return( output )
}

#### 7) camr_shuffle_groups ####
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

    # Determine group level assigned to each particiant
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

