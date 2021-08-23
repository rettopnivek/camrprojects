# Legacy functions
# Written by...
#   Kevin Potter
#   William Schmitt
# email: kevin.w.potter@gmail.com
# Please email me directly if you
# have any questions or comments
# Last updated 2021-08-15

# Note: Legacy functions retained to ensure compatability with
#       old CAM projects

# Table of contents

#### 1) save_data_files ####

#' Save Data Files
#'
#' A convenience function that will save a
#' set of data frames in the global environment
#' as external files using the specified extension
#' and filenames.
#'
#' @param data_frames_to_save A character vector with the names
#'   of the data frames in the global environment that should
#'   be saved
#' @param extensions A character vector with the file extensions
#'   to output. Currently supports 'RData' and 'csv'.
#' @param filenames The filename(s) for the output, either a
#'   single character string or a vector matching in length
#'   to \code{extension}.
#' @param filename_modifiers A character vector of modifiers to the
#'   filenames When saving multiple data frames as .csv files.
#' @param tag An optional argument for the \code{create_standardized_filename}
#'   controlling the beginning tag at the start of the filename.
#' @param number An optional argument for the \code{create_standardized_filename}
#'   specifying the file number.
#' @param use_standard_names Logical; if \code{TRUE} uses
#'   the \code{create_standardized_filename} to generate filenames in
#'   a standardized format.
#'
#' @author Kevin Potter
#'
#' @export

save_data_files = function( data_frames_to_save,
                            extensions,
                            filenames,
                            filename_modifiers = NULL,
                            tag = NULL,
                            number = NULL,
                            use_standard_names = TRUE ) {

  ### Setup

  # Determine number of file types to save
  n_ext = length( extensions )

  # Match length of file names to number of extensions
  if ( length( filenames ) == 1 ) {
    filenames = rep( filenames, n_ext )
  }

  # if no user-supplied number, start file numbering at 1
  if ( is.null( number ) ) {
    current_number = 1
  } else {
    current_number = number
  }

  # If no user-supplied tag, use default
  if ( is.null( tag ) ) {
    tag = 'R'
  }
  # Match length of tags to number of extensions
  if ( length( tag ) == 1 ) {
    tag = rep( tag, n_ext )
  }

  # Function to create a two-digit character string
  # based on supplied number
  two_digits = function( n ) {

    n = as.character( n )
    if ( nchar( n ) == 1 ) {
      n = paste0( '0', n )
    }

    return( n )
  }

  ### Save .RData files

  if ( 'RData' %in% extensions ) {

    file_index = ( extensions %in% 'RData' )

    if ( use_standard_names ) {
      # If specified to use standardized naming scheme

      fname = create_standardized_filename(
        filenames[ file_index ],
        'RData',
        tag = tag[ file_index ],
        number = two_digits( current_number )
      )
      # Increment file number
      current_number = current_number + 1

    } else {
      # Otherwise use supplied filename

      fname = paste0( filenames[ file_index ], '.RData' )

    }

    # Save specified R objects located in global environment
    save(
      list = data_frames_to_save,
      envir = .GlobalEnv,
      file = fname
    )

  }

  ### Save .csv files

  if ( 'csv' %in% extensions ) {

    file_index = ( extensions %in% 'csv' )

    # Determine number of objects to save
    n_obj = length( data_frames_to_save )

    # Defaults for filename modifiers
    if ( is.null( filename_modifiers ) ) {
      filename_modifiers = rep( '', n_obj )
    }

    # Loop over R objects
    for ( i in 1:n_obj ) {

      if ( use_standard_names ) {
        # If specified to use standardized naming scheme

        fname = create_standardized_filename(
          paste0( filenames[ file_index ], filename_modifiers[i] ),
          'csv',
          tag = tag[ file_index ],
          number = two_digits( current_number )
        )

      } else {
        # Otherwise use supplied filename

        if ( filename_modifiers[i] == '' ) {
          # If no modifier is provided, to prevent files
          # being overwritten, include a file number

          fname = paste0( filenames[ file_index ],
                          '_', current_number,
                          '.csv' )
        } else {
          # Otherwise include modifier

          fname = paste0( filenames[ file_index ],
                          filename_modifiers[i],
                          '.csv' )
        }

      }

      # Save .csv file
      write.csv(
        get( data_frames_to_save[i], envir = .GlobalEnv),
        file = fname,
        row.names = F,
        quote = T
      )

      # Increment file number
      current_number = current_number + 1

    }

  }

}

#### 2) match_and_assign ####
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
#' @param default Either a single value to assign in the absence of
#'   a match, or a vector equivalent in length to \code{x}.
#' @param replace_if An optional vector specifying the subset of
#'   default values when it is appropriate to assign new values.
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
#'                   default = '', type = 'exact' )
#' # Vector input for argument 'default'
#' x <- c( 'A', 'A', 'D', 'C', 'A', 'A', 'C', 'D' )
#' match_and_assign( x, list( 'C', 'D' ), c('B','B'), default = x )
#' # Using 'replace_if' for conditional assignment
#' x1 <- rep( LETTERS[1:4], each = 2 )
#' x2 <- rep( LETTERS[5:6], 4 )
#' match_and_assign( x2, list( 'E', 'F' ), c('1','2'),
#'                   default = x1, replace_if = c( 'A', 'B' ) )
#'
#' @export

match_and_assign <- function( x, matches, new_values, type = 'partial',
                              default = NA, replace_if = NULL ) {

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

